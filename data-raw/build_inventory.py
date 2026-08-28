#!/usr/bin/env python3
"""Per-tag lineage inventory for sportsdataverse-data releases (precision pass).

Joins five evidence sources:
  tag_hits.txt        every source-file reference to a release tag (ripgrep)
  releases_full.json  the live release + asset list from the GitHub API
  db_catalog.json     sdv-db REGISTRY (sdv-py loader -> warehouse table)
  orch_pipelines.json sdv-orch PIPELINES (sport -> stage scripts, crons)
  workflow_health.log the nightly org-wide scheduled-workflow health sweep
"""

from __future__ import annotations

import json
import os
import re
from collections import defaultdict
from pathlib import Path

SP = Path(__file__).parent
ROOT = Path(os.environ.get("SDV_REPOS", "/mnt/sdv_repos"))

R_PKGS = {
    "hoopR",
    "wehoop",
    "cfbfastR",
    "fastRhockey",
    "baseballr",
    "softballR",
    "oddsapiR",
    "sportsdataverse-R",
    "cfbseedR",
    "cfb4th",
}
SKIP_REPOS = {
    "sdv-py",
    "ClaudeCowork",
    "dotfiles_saiemgilani",
    "background-research",
    "sdv-internal-refs",
    "Sports-Research-Papers",
    "tmp",
    "universe",
    "agent",
}

PY_DEF = re.compile(r"^(\s*)def\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(")
R_DEF = re.compile(r"^([A-Za-z_.][A-Za-z0-9_.]*)\s*(?:<-|=)\s*function\s*\(")

_cache: dict[str, list[str]] = {}


def lines_of(path: Path) -> list[str]:
    k = str(path)
    if k not in _cache:
        try:
            _cache[k] = path.read_text(errors="replace").splitlines()
        except OSError:
            _cache[k] = []
    return _cache[k]


def py_enclosing(path: Path, lineno: int) -> str | None:
    """Name of the def containing `lineno`, or None if the line is module-level."""
    ls = lines_of(path)
    if lineno > len(ls):
        return None
    hit = ls[lineno - 1]
    hit_indent = len(hit) - len(hit.lstrip())
    if hit_indent == 0:
        return None  # module scope: a URL-template constant, not a function body
    for i in range(lineno - 2, -1, -1):
        m = PY_DEF.match(ls[i])
        if m and len(m.group(1)) < hit_indent:
            return m.group(2)
    return None


def r_enclosing(path: Path, lineno: int) -> str | None:
    ls = lines_of(path)
    for i in range(min(lineno, len(ls)) - 1, -1, -1):
        m = R_DEF.match(ls[i])
        if m:
            return m.group(1)
    return None


# --- workflows --------------------------------------------------------------
CRON = re.compile(
    r"""cron:\s*(?:["']([^"']+)["']|([^\s#][^#\n]*?))\s*(?:#.*)?$""", re.M
)
WF_NAME = re.compile(r"^name:\s*[\"']?(.+?)[\"']?\s*$", re.M)


def scan_workflows() -> dict[str, dict]:
    """repo/.github/workflows/x.yml -> {name, crons, body}."""
    out = {}
    for wf in ROOT.glob("*/.github/workflows/*.y*ml"):
        repo = wf.parts[len(ROOT.parts)]
        if repo in SKIP_REPOS:
            continue
        try:
            body = wf.read_text(errors="replace")
        except OSError:
            continue
        nm = WF_NAME.search(body)
        out[f"{repo}/.github/workflows/{wf.name}"] = {
            "repo": repo,
            "file": wf.name,
            "name": nm.group(1).strip() if nm else wf.stem,
            "crons": [(a or b).strip() for a, b in CRON.findall(body)],
            "body": body,
        }
    return out


def parse_health() -> dict[tuple[str, str], dict]:
    """(repo, workflow display name) -> {state, age, cadence, url}."""
    log = ROOT / "bin" / "workflow_health.log"
    if not log.exists():
        return {}
    txt = log.read_text(errors="replace")
    # keep only the most recent sweep block
    blocks = txt.split("SUMMARY:")
    body = blocks[-2] if len(blocks) > 1 else txt
    out = {}
    row = re.compile(
        r"^\s*(!?\s*(?:FAIL|STALE|ok))\s+(\S+)\s+(.+?)\s{2,}(\S+)\s+(\S+ ago)\s+every ~(\S+)\s*$"
    )
    for ln in body.splitlines():
        m = row.match(ln)
        if m:
            state, repo, wname, concl, age, cadence = m.groups()
            out[(repo, wname.strip())] = {
                "state": state.replace("!", "").strip(),
                "conclusion": concl,
                "age": age,
                "cadence": cadence,
            }
    return out


SEASON = re.compile(r"(\d{4})(?=\.(?:parquet|csv\.gz|rds|csv)$)")


def asset_stats(assets: list[dict]) -> dict:
    exts: dict[str, int] = defaultdict(int)
    stems: dict[str, int] = defaultdict(int)
    seasons: set[int] = set()
    total = 0
    latest = ""
    for a in assets:
        n = a["name"]
        total += a.get("size", 0)
        latest = max(latest, a.get("updated_at", "") or "")
        for e in (".parquet", ".csv.gz", ".rds", ".csv", ".qs", ".json", ".RData"):
            if n.endswith(e):
                exts[e] += 1
                base = n[: -len(e)]
                break
        else:
            exts[Path(n).suffix or "(none)"] += 1
            base = n
        m = re.search(r"(\d{4})$", base)
        if m:
            seasons.add(int(m.group(1)))
            stems[base[: m.start()].rstrip("_")] += 1
        else:
            stems[base] += 1
    return {
        "n": len(assets),
        "exts": dict(sorted(exts.items(), key=lambda kv: -kv[1])),
        "stems": dict(sorted(stems.items(), key=lambda kv: -kv[1])),
        "season_min": min(seasons) if seasons else None,
        "season_max": max(seasons) if seasons else None,
        "n_seasons": len(seasons),
        "bytes": total,
        "latest_asset_update": latest[:10],
    }


def main() -> None:
    tags = [t.strip() for t in (SP / "tags.txt").read_text().split() if t.strip()]
    tagset = set(tags)
    tag_re = re.compile(
        "|".join(re.escape(t) for t in sorted(tags, key=len, reverse=True))
    )

    releases = {r["tag_name"]: r for r in json.load(open(SP / "releases_full.json"))}
    catalog = json.load(open(SP / "db_catalog.json"))
    by_loader: dict[str, list[dict]] = defaultdict(list)
    for d in catalog:
        if d["loader"]:
            by_loader[d["loader"]].append(d)
    orch = json.load(open(SP / "orch_pipelines.json"))
    workflows = scan_workflows()
    health = parse_health()

    # tag -> role -> hits
    hits: dict[str, dict[str, list]] = defaultdict(lambda: defaultdict(list))
    for raw in (SP / "tag_hits.txt").read_text(errors="replace").splitlines():
        parts = raw.split(":", 2)
        if len(parts) < 3 or not parts[1].isdigit():
            continue
        path, lineno, content = parts[0], int(parts[1]), parts[2]
        rel = path[2:] if path.startswith("./") else path
        segs = rel.split("/")
        repo, inner = segs[0], "/".join(segs[1:])
        if repo in SKIP_REPOS or not inner:
            continue
        for tag in set(tag_re.findall(content)) & tagset:
            hits[tag][repo].append((inner, lineno, content.strip()))

    out: dict[str, dict] = {}
    for tag in tags:
        rel = releases.get(tag, {})
        rec: dict[str, object] = {
            "tag": tag,
            "name": rel.get("name"),
            "published_at": (rel.get("published_at") or "")[:10],
            "created_at": (rel.get("created_at") or "")[:10],
            "body": rel.get("body", ""),
            "html_url": rel.get("html_url"),
            "assets": asset_stats(rel.get("assets", [])),
        }
        py, r, producers, wfs, dbref, downstream, docs = [], [], [], [], [], [], []
        py_modules: set[str] = set()
        for repo, items in hits.get(tag, {}).items():
            for inner, lineno, content in items:
                full = ROOT / repo / inner
                low = inner.lower()
                is_wf = low.startswith(".github/workflows/")
                if is_wf:
                    wfs.append(f"{repo}/{inner}")
                elif repo == "sportsdataverse-py":
                    if low.startswith("sportsdataverse/"):
                        fn = py_enclosing(full, lineno)
                        mod = inner.replace("/", ".").removesuffix(".py")
                        if fn and fn.startswith("load_"):
                            py.append((mod, fn))
                        elif fn is None and ("loader" in low or "_loaders" in low):
                            py.append((mod, None))
                        # Modules that read the tag but are not loaders: the
                        # models, scorers and feature builders that consume it.
                        base = inner.rsplit("/", 1)[-1].removesuffix(".py")
                        if not (
                            base.endswith("_loaders")
                            or base.startswith("_")
                            or base == "__init__"
                            or "/tests/" in low
                        ):
                            py_modules.add(base)
                    elif low.startswith("docs/"):
                        docs.append(inner)
                elif repo in R_PKGS:
                    if low.startswith("r/"):
                        fn = r_enclosing(full, lineno)
                        if fn and fn.startswith("load_"):
                            r.append((repo, fn, inner))
                elif repo == "sdv-db":
                    dbref.append(f"{repo}/{inner}")
                elif repo in (
                    "sportsdataverse-web",
                    "sportsdataverse-js",
                    "sportsdataverse-org",
                    "game-on-paper-app",
                    "game-on-paper-app-cloudflare",
                    "sdv-next-clone",
                ):
                    downstream.append(f"{repo}/{inner}")
                elif inner.endswith((".py", ".R", ".r", ".sh")) and not low.startswith(
                    ("tests/", "test/", "docs/")
                ):
                    producers.append(f"{repo}/{inner}")

        rec["py"] = sorted({tuple(x) for x in py}, key=lambda t: (t[0], t[1] or ""))
        rec["r"] = sorted({tuple(x) for x in r})
        rec["producers"] = sorted(set(producers))
        rec["docs"] = sorted(set(docs))
        rec["downstream"] = sorted(set(downstream))
        rec["db_refs"] = sorted(set(dbref))
        rec["py_modules"] = sorted(py_modules)[:6]

        # warehouse tables, via the sdv-py loader name
        tables = []
        for _mod, fn in rec["py"]:
            for d in by_loader.get(fn or "", []):
                tables.append(d)
        rec["db_tables"] = sorted({(t["league"], t["name"]) for t in tables})

        # workflows: named directly, or running one of the producing scripts
        wf_hits = set(wfs)
        prod_repos = {p.split("/")[0] for p in rec["producers"]}
        prod_names = {Path(p).name for p in rec["producers"]}
        for key, w in workflows.items():
            if w["repo"] in prod_repos and any(n in w["body"] for n in prod_names):
                wf_hits.add(key)
        wl = []
        for key in sorted(wf_hits):
            w = workflows.get(key)
            if not w:
                continue
            h = health.get((w["repo"], w["name"]), {})
            wl.append(
                {
                    "repo": w["repo"],
                    "file": w["file"],
                    "name": w["name"],
                    "crons": w["crons"],
                    "health": h,
                }
            )
        rec["workflows"] = wl

        # orch pipeline whose stage repos overlap the producing repos
        opipes = []
        for sport, p in orch.items():
            srepos = {s["repo"] for s in p["stages"].values()}
            if srepos & prod_repos:
                opipes.append(
                    {
                        "sport": sport,
                        "label": p["label"],
                        "crons": p["crons"],
                        "schedule_active": p["schedule_active"],
                        "season_min": p["season_min"],
                        "season_max": p["season_max"],
                        "stages": p["stages"],
                        "packages": p["packages"],
                    }
                )
        rec["orch"] = opipes
        out[tag] = rec

    (SP / "inventory2.json").write_text(json.dumps(out, indent=1))
    n_prod = sum(1 for t in tags if out[t]["producers"] or out[t]["workflows"])
    n_py = sum(1 for t in tags if out[t]["py"])
    n_r = sum(1 for t in tags if out[t]["r"])
    n_db = sum(1 for t in tags if out[t]["db_tables"])
    n_wf = sum(1 for t in tags if out[t]["workflows"])
    n_orch = sum(1 for t in tags if out[t]["orch"])
    print(
        f"tags {len(tags)} | producers {n_prod} | py {n_py} | r {n_r} | db {n_db} | workflows {n_wf} | orch {n_orch}"
    )
    print(
        "no producer:",
        ", ".join(
            t for t in tags if not out[t]["producers"] and not out[t]["workflows"]
        ),
    )
    print("no py loader:", ", ".join(t for t in tags if not out[t]["py"]))


if __name__ == "__main__":
    main()
