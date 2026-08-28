#!/usr/bin/env python3
"""Render a descriptive release body for every sportsdataverse-data release.

Every repo path emitted as a link is checked against the local checkout first;
anything that cannot be proven to exist is dropped rather than published as a
dead link.
"""

from __future__ import annotations

import json
import os
import re
import sys
from collections import defaultdict
from pathlib import Path

SP = Path(__file__).parent
sys.path.insert(0, str(SP))
ROOT = Path(os.environ.get("SDV_REPOS", "/mnt/sdv_repos"))

from families import DATA_REPO, DL, FAMILIES, KIND, ORG  # noqa: E402


def load_json(name: str):
    with open(SP / name) as fh:
        return json.load(fh)


INV = load_json("inventory2.json")
PYMAP = load_json("py_tag_loaders.json")
RMAP = load_json("r_tag_loaders.json")
CAT = load_json("db_catalog.json")
ORCH = load_json("orch_pipelines.json")
WORKFLOWS = load_json("workflows.json")
#: Every release description as it stood before this generator first ran, keyed
#: by tag. Long entries are hand-written per-dataset prose (column dictionaries,
#: model cards, methodology warnings) and are re-emitted verbatim rather than
#: regenerated away — see the `## Dataset notes` section below.
BACKUP = load_json("backup_bodies.json")

BY_LOADER = defaultdict(list)
for d in CAT:
    if d["loader"]:
        BY_LOADER[d["loader"]].append(d)

SKIP_WF = {
    "tests.yml",
    "orphan_scripts.yml",
    "test.yml",
    "R-CMD-check.yaml",
    "pkgdown.yaml",
}


# --- helpers ----------------------------------------------------------------
def exists(repo_path: str) -> bool:
    return (ROOT / repo_path).exists()


def repo_link(
    repo: str, path: str | None = None, label: str | None = None
) -> str | None:
    """Markdown link to a repo or a file inside it, only if the path really exists."""
    if path:
        if not exists(f"{repo}/{path}"):
            return None
        return f"[`{label or path}`]({ORG}/{repo}/blob/main/{path})"
    if not (ROOT / repo).exists():
        return None
    return f"[`{label or repo}`]({ORG}/{repo})"


def human_bytes(n: int) -> str:
    for unit in ("B", "KB", "MB", "GB", "TB"):
        if n < 1024 or unit == "TB":
            return f"{n:.0f} {unit}" if unit in ("B", "KB") else f"{n:.1f} {unit}"
        n /= 1024.0
    return f"{n:.1f} TB"


def family_of(tag: str) -> tuple[str, dict]:
    for prefix, meta in FAMILIES:
        if tag.startswith(prefix):
            return prefix, meta
    return "", {}


def kind_of(tag: str, prefix: str) -> str:
    rest = tag[len(prefix) :] if prefix else tag
    return KIND.get(rest, "")


CRON_MONTH = {
    1: "Jan",
    2: "Feb",
    3: "Mar",
    4: "Apr",
    5: "May",
    6: "Jun",
    7: "Jul",
    8: "Aug",
    9: "Sep",
    10: "Oct",
    11: "Nov",
    12: "Dec",
}


def cron_english(expr: str) -> str:
    """A short human gloss for the 5-field cron expressions used in this org."""
    f = expr.split()
    if len(f) != 5:
        return f"`{expr}`"
    minute, hour, dom, month, dow = f
    when = (
        f"{int(hour):02d}:{int(minute):02d} UTC"
        if hour.isdigit() and minute.isdigit()
        else f"`{expr}`"
    )
    if month != "*":
        months = []
        for part in month.split(","):
            if "-" in part:
                a, b = part.split("-")
                months.append(
                    f"{CRON_MONTH.get(int(a), a)}-{CRON_MONTH.get(int(b), b)}"
                )
            else:
                months.append(CRON_MONTH.get(int(part), part))
        window = " ".join(months)
    else:
        window = "year-round"
    days = "" if dom == "*" else f" (days {dom})"
    dows = "" if dow == "*" else f" on weekday {dow}"
    return f"{when}, {window}{days}{dows}"


def creation_scripts(tag: str, prefix: str, meta: dict) -> list[str]:
    """The numbered per-dataset creation script(s), where a repo uses that layout."""
    cd = meta.get("creation_dir")
    if not cd:
        return []
    repo, sub, pref = cd
    d = ROOT / repo / sub
    if not d.is_dir():
        return []
    rest = tag[len(prefix) :]
    out = []
    for f in sorted(d.glob(f"{pref}[0-9][0-9]_*")):
        stem = f.stem[len(pref) + 3 :]
        for suffix in ("_creation", "_scrape", "_parse", "_build", "_publish"):
            stem = stem.removesuffix(suffix)
        if stem == rest or stem == rest.rstrip("s") or rest.startswith(stem):
            link = repo_link(repo, f"{sub}/{f.name}")
            if link:
                out.append(link)
    return out


def build_scripts_from_inventory(tag: str, own: set[str]) -> list[str]:
    """Producer files the grep proved, ranked so the per-dataset script comes first."""
    prods = [p for p in INV[tag]["producers"] if not own or p.split("/")[0] in own]
    rest = tag.split("_", 1)[-1]
    keep = []
    for p in prods:
        base = Path(p).stem.lower()
        if any(x in base for x in ("__init__", "conftest")):
            continue
        score = 0
        if rest and rest in base:
            score -= 2
        if "creation" in base or "_build" in base or "publish" in base:
            score -= 1
        keep.append((score, p))
    keep.sort()
    return [p for _s, p in keep]


def wf_rows(repos: list[str], tag_health: dict) -> list[str]:
    rows = []
    for repo in repos:
        for w in WORKFLOWS.get(repo, []):
            if w["file"] in SKIP_WF or not w["crons"]:
                continue
            blob = (w["file"] + " " + w["name"]).lower()
            if any(
                x in blob
                for x in (
                    "preview",
                    "postweek",
                    "figure",
                    "skeet",
                    "parity",
                    "trigger",
                    "cran",
                    "codeql",
                    "lint",
                )
            ):
                continue
            link = repo_link(repo, f".github/workflows/{w['file']}", w["file"])
            if not link:
                continue
            sched = "<br>".join(cron_english(c) for c in w["crons"][:4])
            h = tag_health.get((repo, w["name"]), {})
            status = ""
            if h:
                icon = {"ok": "✅", "FAIL": "❌", "STALE": "⚠️"}.get(h["state"], "")
                status = f"{icon} {h['state']} · last run {h['age']}"
            else:
                status = "—"
            rows.append(
                f"| {repo_link(repo)} | {link} | {w['name']} | {sched} | {status} |"
            )
    return rows


def parse_health() -> dict:
    log = ROOT / "bin" / "workflow_health.log"
    if not log.exists():
        return {}
    txt = log.read_text(errors="replace")
    blocks = txt.split("SUMMARY:")
    body = blocks[-2] if len(blocks) > 1 else txt
    out = {}
    row = re.compile(
        r"^\s*!?\s*(FAIL|STALE|ok)\s+(\S+)\s+(.+?)\s{2,}(\S+)\s+(\S+ ago)\s+every ~(\S+)\s*$"
    )
    for ln in body.splitlines():
        m = row.match(ln)
        if m:
            state, repo, wname, _concl, age, cadence = m.groups()
            out[(repo, wname.strip())] = {
                "state": state,
                "age": age,
                "cadence": cadence,
            }
    return out


HEALTH = parse_health()
CRONTAB = SP / "crontab_snapshot.txt"


def droplet_cron(repos: list[str]) -> list[str]:
    """Host-scheduled runs for these repos, as schedule + script only.

    The raw crontab line is deliberately NOT published: it carries absolute host
    paths and log destinations, which say nothing useful to a data consumer and
    describe the host's layout on a public page. What is worth publishing is
    when the job runs and which in-repo script it invokes -- both already public.
    """
    if not CRONTAB.exists():
        return []
    script_re = re.compile(r"(?:^|[/\s])((?:scripts|bin)/[\w.-]+\.(?:sh|R|py))")
    out = []
    for ln in CRONTAB.read_text(errors="replace").splitlines():
        ln = ln.strip()
        if ln.startswith("#") or not ln or "=" in ln.split()[0]:
            continue  # comments and MAILTO=/PATH= assignments
        for r in repos:
            if f"/{r}" not in ln:
                continue
            fields = ln.split(None, 5)
            if len(fields) < 6:
                break
            when = cron_english(" ".join(fields[:5]))
            m = script_re.search(fields[5])
            script = repo_link(r, m.group(1)) if m else None
            out.append(
                f"- **`{r}`** — {when}" + (f", running {script}" if script else "")
            )
            break
    return out


# --- the renderer -----------------------------------------------------------
def render(tag: str) -> str:
    rec = INV[tag]
    prefix, meta = family_of(tag)
    a = rec["assets"]
    kind = kind_of(tag, prefix)
    ls = (a["season_max"] or 2025) + meta.get("loader_season", 0)
    title = meta.get("title", tag)

    L: list[str] = []

    # ---- header
    desc = f"**{title}** — {kind}." if kind else f"**{title}**."
    L.append(desc)
    L.append("")
    if meta.get("archived"):
        L.append(
            "> **Status: archived.** These assets are final and are not being updated."
        )
        L.append("")
    if a["n"] == 0:
        L.append(
            "> **Status: empty.** This tag is reserved but currently carries no assets."
        )
        L.append("")

    cover = []
    if a["season_min"]:
        cover.append(
            f"**{a['n_seasons']} season{'' if a['n_seasons'] == 1 else 's'}** ({a['season_min']}–{a['season_max']})"
        )
    cover.append(f"**{a['n']} asset{'' if a['n'] == 1 else 's'}**")
    if a["bytes"]:
        cover.append(human_bytes(a["bytes"]))
    if a["latest_asset_update"]:
        cover.append(f"last asset written **{a['latest_asset_update']}**")
    L.append(" · ".join(cover))
    L.append("")
    L.append(
        f"Upstream source: {meta.get('provider', 'see the producing repository')}."
    )
    if meta.get("season_key"):
        L.append("")
        L.append(f"> **Season key.** {meta['season_key']}")
    if meta.get("note"):
        L.append("")
        L.append(f"> **Note.** {meta['note']}")
    L.append("")

    # ---- preserved hand-written notes (column dictionaries, methodology warnings)
    prior = BACKUP.get(tag, "").strip()
    if len(prior) > 250:
        L.append("## Dataset notes")
        L.append("")
        L.append("<sub>Written by hand for this dataset and preserved verbatim.</sub>")
        L.append("")
        L.append(prior)
        L.append("")

    # ---- files
    if a["n"]:
        L.append("## Files in this release")
        L.append("")
        L.append("| Format | Files | Name pattern |")
        L.append("|---|---:|---|")
        stems = [
            s
            for s in a["stems"]
            if not s.endswith(".txt") and s not in ("timestamp", "package_function")
        ]
        pattern = stems[0] if stems else tag
        for ext, n in a["exts"].items():
            if ext in (".txt", ".json", "(none)"):
                continue
            suffix = "_{season}" if a["season_min"] else ""
            shown = "{dataset}" if len(stems) > 3 else pattern
            L.append(f"| `{ext.lstrip('.')}` | {n} | `{shown}{suffix}{ext}` |")
        L.append("")
        if len(stems) > 1:
            L.append(
                f"This tag carries **{len(stems)} distinct datasets** under one release: "
                + ", ".join(f"`{s}`" for s in stems[:14])
                + (f", and {len(stems) - 14} more." if len(stems) > 14 else ".")
            )
            L.append("")
        example = None
        for name_hint in (".parquet", ".csv.gz", ".csv", ".rds"):
            if name_hint in a["exts"]:
                example = f"{pattern}{('_' + str(a['season_max'])) if a['season_max'] else ''}{name_hint}"
                break
        if example:
            L.append("Direct download (no auth, no API key):")
            L.append("")
            L.append("```")
            L.append(f"{DL}/{tag}/{example}")
            L.append("```")
            L.append("")
        if any(
            s.endswith(".txt") or s in ("timestamp", "package_function")
            for s in a["stems"]
        ):
            L.append(
                "`timestamp.txt` and `package_function.txt` are `piggyback` bookkeeping sidecars "
                "written by the R publisher; they record when the release was last refreshed and "
                "which function wrote it."
            )
            L.append("")

    # ---- how to load
    L.append("## How to load it")
    L.append("")
    rows = []
    pys = PYMAP.get(tag, [])
    for mod, fn, sig in pys[:4]:
        # curated entries carry a ready-made call example; scanned ones carry a signature
        args = (
            sig.replace("SEASON", str(ls))
            if ("SEASON" in sig or '"sdv"' in sig)
            else f"seasons=[{ls}]"
        )
        rows.append(f"| Python | `from {mod} import {fn}`<br>`{fn}({args})` |")
    if not pys:
        rows.append(
            "| Python | *no `sportsdataverse-py` loader yet — read the asset URL directly* |"
        )
    rs = RMAP.get(tag, [])
    for pkg, fn, _f in rs[:4]:
        rows.append(f"| R | `{pkg}::{fn}(seasons = {ls})` |")
    if not rs:
        rows.append(
            "| R | *no SportsDataverse R loader — read the asset URL directly* |"
        )

    tables = sorted(
        {
            (d["league"], d["name"], d["partition_col"])
            for _m, fn, _s in pys
            for d in BY_LOADER.get(fn, [])
        }
    )
    for lg, nm, pcol in tables[:4]:
        rows.append(
            f"| SQL | `SELECT * FROM {lg}.{nm}{f' WHERE {pcol} = {ls}' if pcol else ''}` "
            f"in the `sportsdataverse` Postgres warehouse |"
        )
        rows.append(
            f"| HTTP | `GET https://data.sportsdataverse.org/v1/{lg}/{nm}{f'?{pcol}={ls}' if pcol else ''}` "
            f"(bearer token required) |"
        )
    if a["n"]:
        rows.append(
            f'| Any | `pd.read_parquet("{DL}/{tag}/...")` — the assets are plain files on a public URL |'
        )
    L.append("| Surface | Call |")
    L.append("|---|---|")
    L += rows
    L.append("")

    # ---- production chain
    L.append("## How it is produced")
    L.append("")
    step = 1
    if meta.get("raw"):
        rrepo, rdesc = meta["raw"]
        link = repo_link(rrepo) or f"`{rrepo}`"
        L.append(f"{step}. **Capture** — {link}: {rdesc}.")
        step += 1
    if meta.get("build"):
        brepo, bdesc = meta["build"]
        link = repo_link(brepo) or f"`{brepo}`"
        L.append(f"{step}. **Build** — {link}: {bdesc}.")
        step += 1
    if meta.get("publish"):
        L.append(
            f"{step}. **Publish** — {meta['publish']} uploads the assets to this tag."
        )
        step += 1
    L.append("")

    scripts = creation_scripts(tag, prefix, meta)
    own = {r for r in (meta.get("raw", (None,))[0], meta.get("build", (None,))[0]) if r}
    prods = build_scripts_from_inventory(tag, own)
    named = []
    for p in prods[:4]:
        repo = p.split("/")[0]
        link = repo_link(repo, p.split("/", 1)[1])
        if link:
            named.append(f"- {link} &nbsp;<sub>{repo}</sub>")
    if scripts:
        named = [
            f"- {s} &nbsp;<sub>{meta['creation_dir'][0]}</sub>" for s in scripts
        ] + named
    if named:
        L.append("**The code that writes these files:**")
        L.append("")
        L += named[:5]
        L.append("")

    # ---- orchestration
    orch_key = meta.get("orch")
    p = ORCH.get(orch_key) if orch_key else None
    repos = [
        r for r in ({meta.get("raw", (None,))[0], meta.get("build", (None,))[0]}) if r
    ]
    L.append("## Automation")
    L.append("")
    wrows = wf_rows(sorted(repos), HEALTH)
    if wrows:
        L.append(
            "**GitHub Actions** — scheduled workflows in the producing repositories:"
        )
        L.append("")
        L.append("| Repo | Workflow | Name | Schedule (UTC) | Last scheduled run |")
        L.append("|---|---|---|---|---|")
        L += wrows[:8]
        L.append("")
    if p:
        L.append(
            f"**Orchestrator** — registered in [`sdv-orch`]({ORG}/sdv-orch) as pipeline "
            f"`{orch_key}` ({p['label']}), seasons {p['season_min']}–{p['season_max']}."
        )
        L.append("")
        L.append("| Stage | Repo | Script |")
        L.append("|---|---|---|")
        for sk, s in p["stages"].items():
            link = repo_link(s["repo"], s["script"]) or f"`{s['script']}`"
            L.append(f"| `{sk}` | `{s['repo']}` | {link} |")
        L.append("")
        if not p["schedule_active"]:
            driver = (
                "the GitHub Actions workflows above are the live driver"
                if wrows
                else "these stage scripts are driven on demand from the SportsDataverse host"
            )
            L.append(
                f"_The Prefect schedule for this pipeline is currently paused; {driver}._"
            )
            L.append("")
    dc = droplet_cron(sorted(repos))
    if dc:
        L.append(
            "**Droplet cron** — this pipeline also runs from the SportsDataverse host:"
        )
        L.append("")
        L += dc[:4]
        L.append("")
    if not wrows and not p and not dc:
        L.append(
            "_No scheduled automation is attached to this tag: it is refreshed on demand._"
        )
        L.append("")

    # ---- who depends on it
    L.append("## What depends on it")
    L.append("")
    dep = []
    for mod, fn, _sig in pys:
        src = repo_link("sportsdataverse-py", mod.replace(".", "/"), f"{mod}.{fn}()")
        dep.append(
            f"- **`sportsdataverse-py`** — {src or chr(96) + mod + chr(46) + fn + chr(40) + chr(41) + chr(96)}"
            f" reads this tag directly ([API docs](https://py.sportsdataverse.org/))."
        )
    for pkg, fn, fl in rs:
        link = repo_link(pkg, f"R/{fl}", f"{pkg}::{fn}()")
        dep.append(
            f"- **`{pkg}`** — {link or f'`{pkg}::{fn}()`'} reads this tag directly."
        )
    for lg, nm, _pcol in tables:
        dep.append(
            f"- **`sdv-db`** — registered in the sdv-db catalog and ingested into the `sportsdataverse` Postgres warehouse as "
            f"`{lg}.{nm}`, and served by the Data API at `/v1/{lg}/{nm}`."
        )
    loader_files = {m.split(".")[-1] for m, _f, _s in pys}
    generic = {"config", "discover", "engines", "utils", "helpers", "constants", "cli"}
    extra = [
        m
        for m in sorted(rec.get("py_modules", []))
        if m not in loader_files and m not in generic
    ]
    if extra:
        dep.append(
            "- **`sportsdataverse-py`** — also read outside the loaders by "
            + ", ".join(f"`{m}`" for m in extra[:6])
            + " (models, scorers and feature builders)."
        )
    down = {d.split("/")[0] for d in rec["downstream"] if not d.endswith(".md")}
    for d in sorted(down):
        dep.append(f"- **`{d}`** — reads this tag in application code.")
    if dep:
        L += dep[:14]
    else:
        L.append(
            "_Nothing in the ecosystem reads this tag through a named loader yet._"
        )
    L.append("")

    # ---- siblings
    sibs = [t for t in INV if prefix and t.startswith(prefix) and t != tag]
    if sibs:
        L.append("## Related releases")
        L.append("")
        L.append(
            ", ".join(
                f"[`{s}`]({DATA_REPO}/releases/tag/{s})" for s in sorted(sibs)[:40]
            )
        )
        if len(sibs) > 40:
            L.append(f" …and {len(sibs) - 40} more in the same family.")
        L.append("")

    L.append("---")
    L.append("")
    L.append(
        f"Part of [SportsDataverse]({ORG}). Every release in this repository is a public, "
        f"versioned data asset: the files are stable URLs you can read straight from R, Python, "
        f"or anything that speaks HTTP. Issues with the data belong on the producing repository "
        f"listed above; issues with a loader belong on that package's repository."
    )
    return "\n".join(L).rstrip() + "\n"


def main() -> None:
    outdir = SP / "bodies"
    outdir.mkdir(exist_ok=True)
    sizes = []
    for tag in INV:
        body = render(tag)
        (outdir / f"{tag}.md").write_text(body)
        sizes.append((len(body), tag))
    sizes.sort()
    print(f"rendered {len(sizes)} bodies into {outdir}")
    print(f"  smallest: {sizes[0][1]} ({sizes[0][0]} chars)")
    print(f"  largest:  {sizes[-1][1]} ({sizes[-1][0]} chars)")
    print(f"  median:   {sizes[len(sizes) // 2][0]} chars")
    over = [t for n, t in sizes if n > 125000]
    if over:
        print("OVER GitHub's 125k body limit:", over)


if __name__ == "__main__":
    main()
