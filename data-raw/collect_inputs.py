#!/usr/bin/env python3
"""Collect every input the release-note renderer joins on.

Each step writes one JSON file next to this script and is independently
re-runnable. A step that cannot reach its source (a missing sibling checkout, a
missing venv, no network) warns and leaves the previous file in place rather
than truncating it, so a partial refresh degrades to a stale input instead of an
empty one.

    python3 collect_inputs.py            # all steps
    python3 collect_inputs.py releases workflows   # just these
"""

from __future__ import annotations

import json
import os
import re
import shutil
import subprocess
import sys
from collections import defaultdict
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = Path(os.environ.get("SDV_REPOS", "/mnt/sdv_repos"))
REPO = "sportsdataverse/sportsdataverse-data"

R_PKGS = [
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
]
SOURCE_GLOBS = [
    "*.py",
    "*.R",
    "*.r",
    "*.sh",
    "*.yml",
    "*.yaml",
    "*.md",
    "*.ts",
    "*.js",
    "*.Rmd",
    "*.qmd",
    "*.sql",
    "*.toml",
]
# NOT *.json: in this workspace that means every raw ESPN/NCAA payload ever
# captured, which turns an 8-second scan into a 12-minute one.


def warn(msg: str) -> None:
    print(f"  !! {msg}", file=sys.stderr)


def write(name: str, obj: object) -> None:
    (HERE / name).write_text(json.dumps(obj, indent=1))
    print(f"  -> {name}")


# --- 1. the live release + asset inventory ----------------------------------
def step_releases() -> None:
    out = subprocess.run(
        ["gh", "api", "--paginate", f"repos/{REPO}/releases?per_page=100"],
        capture_output=True,
        text=True,
    )
    if out.returncode != 0:
        warn(
            f"gh api failed, keeping existing releases_full.json: {out.stderr.strip()[:200]}"
        )
        return
    releases = json.loads(out.stdout)
    (HERE / "releases_full.json").write_text(json.dumps(releases, indent=1))
    tags = sorted(r["tag_name"] for r in releases)
    (HERE / "tags.txt").write_text("\n".join(tags) + "\n")
    print(f"  -> releases_full.json, tags.txt ({len(tags)} releases)")


# --- 2. every source-file reference to a release tag ------------------------
def step_hits() -> None:
    tagfile = HERE / "tags.txt"
    if not tagfile.exists():
        warn("tags.txt missing — run the releases step first")
        return
    # ripgrep when it is a real binary (it honours .gitignore, which is what keeps
    # this to seconds rather than minutes); GNU grep otherwise.
    rg = shutil.which("rg")
    if rg:
        cmd = [rg, "-F", "-f", str(tagfile), "-n", "--no-heading", "--no-messages"]
        for g in SOURCE_GLOBS:
            cmd += ["-g", g]
        for g in (
            "!.git/**",
            "!**/.venv/**",
            "!**/node_modules/**",
            "!**/logs/**",
            "!**/site-packages/**",
        ):
            cmd += ["-g", g]
        cmd.append(str(ROOT))
    else:
        warn("ripgrep not found — falling back to grep -r (minutes, not seconds)")
        cmd = ["grep", "-rn", "-F", "-f", str(tagfile)]
        for g in SOURCE_GLOBS:
            cmd.append(f"--include={g}")
        # grep honours no .gitignore, so every directory ripgrep skips for free
        # has to be named. `.uv-cache` is the one that matters: the shared uv
        # cache is deliberately kept inside the workspace (it is what keeps ~29
        # venvs near-free instead of 1.5 GB each) and alone accounts for roughly
        # 190k spurious matches.
        for d in (
            ".git",
            ".venv",
            "venv",
            "node_modules",
            "logs",
            "__pycache__",
            "site-packages",
            ".next",
            "dist",
            "build",
            ".uv-cache",
            ".cache",
            ".Rproj.user",
            ".omc",
            ".remember",
        ):
            cmd.append(f"--exclude-dir={d}")
        cmd.append(str(ROOT))

    out = subprocess.run(cmd, capture_output=True, text=True)
    if out.returncode not in (0, 1):
        warn(f"tag scan failed: {out.stderr.strip()[:200]}")
        return
    # Paths come back absolute; store them repo-relative. Drop this directory's
    # own files: the rendered notes and their backups quote every tag by name,
    # and counting those would make each release look like a consumer of itself.
    try:
        self_dir = str(HERE.relative_to(ROOT)) + "/"
    except ValueError:  # toolchain checked out outside SDV_REPOS
        self_dir = "\0"
    keep = [
        ln
        for ln in out.stdout.replace(str(ROOT) + "/", "").splitlines()
        if not ln.startswith(self_dir)
    ]
    (HERE / "tag_hits.txt").write_text("\n".join(keep) + "\n")
    print(f"  -> tag_hits.txt ({len(keep)} references)")


# --- 3. the warehouse catalog -----------------------------------------------
def step_db_catalog() -> None:
    venv = ROOT / "sdv-db" / "python" / ".venv" / "bin" / "python"
    if not venv.exists():
        warn(f"no sdv-db venv at {venv}, keeping existing db_catalog.json")
        return
    code = (
        "import json,sys;sys.path.insert(0,'python/src');"
        "from sdv_db.catalog import REGISTRY;"
        "print(json.dumps([{'league':d.league,'name':d.name,'source':d.source,"
        "'loader':d.loader,'module':d.module_name,'partition_col':d.partition_col,"
        "'asset_url':d.asset_url,'loader_season_offset':d.loader_season_offset,"
        "'db_season_offset':d.db_season_offset} for d in REGISTRY.values()]))"
    )
    out = subprocess.run(
        [str(venv), "-c", code], cwd=ROOT / "sdv-db", capture_output=True, text=True
    )
    if out.returncode != 0:
        warn(f"sdv-db catalog import failed: {out.stderr.strip()[-200:]}")
        return
    write("db_catalog.json", json.loads(out.stdout))


# --- 4. the orchestrator's pipeline registry --------------------------------
def step_orch() -> None:
    orch = ROOT / "sdv-orch"
    if not (orch / "sdv_orch" / "registry.py").exists():
        warn("no sdv-orch checkout, keeping existing orch_pipelines.json")
        return
    code = (
        "import json,sys;sys.path.insert(0,'.');"
        "from sdv_orch.registry import PIPELINES;"
        "print(json.dumps({k:{'sport':p.sport,'label':p.label,'season_min':p.season_min,"
        "'season_max':p.season_max,'default_stages':list(p.default_stages),"
        "'crons':list(p.crons),'schedule_active':p.schedule_active,"
        "'packages':list(p.packages),'warehouse_sport':p.warehouse_sport,"
        "'stages':{sk:{'label':s.label,'repo':s.repo,'script':s.script,"
        "'deps':list(s.deps),'rate_classes':list(s.rate_classes),'note':s.note}"
        " for sk,s in p.stages.items()}} for k,p in PIPELINES.items()}))"
    )
    out = subprocess.run(
        [sys.executable, "-c", code], cwd=orch, capture_output=True, text=True
    )
    if out.returncode != 0:
        warn(f"sdv-orch registry import failed: {out.stderr.strip()[-200:]}")
        return
    write("orch_pipelines.json", json.loads(out.stdout))


# --- 5. GitHub Actions workflows and their schedules ------------------------
# The cron value carries a trailing comment in most of these workflows, so the
# expression has to be captured rather than "everything to end of line".
CRON = re.compile(
    r"""cron:\s*(?:["']([^"']+)["']|([^\s#][^#\n]*?))\s*(?:#.*)?$""", re.M
)
WF_NAME = re.compile(r"^name:\s*[\"']?(.+?)[\"']?\s*$", re.M)


def step_workflows() -> None:
    out: dict[str, list[dict]] = {}
    for wf in sorted(ROOT.glob("*/.github/workflows/*.y*ml")):
        repo = wf.relative_to(ROOT).parts[0]
        body = wf.read_text(errors="replace")
        nm = WF_NAME.search(body)
        out.setdefault(repo, []).append(
            {
                "file": wf.name,
                "name": nm.group(1).strip() if nm else wf.stem,
                "crons": [(a or b).strip() for a, b in CRON.findall(body)],
                "dispatch": "workflow_dispatch" in body,
            }
        )
    write("workflows.json", out)


# --- 6. sportsdataverse-py loaders ------------------------------------------
# Every generated loader opens with `"""Load <tag> (sportsdataverse-data
# release).`, which is a far tighter join than "the tag appears in this file":
# a URL-template constant at module scope would otherwise be attributed to
# whatever function happened to sit above it.
PY_LOADER = re.compile(
    r'^def (load_[A-Za-z0-9_]+)\(([^)]*)\):\s*\n\s*"""Load ([a-zA-Z0-9_]+) '
    r"\(sportsdataverse-data release\)",
    re.M,
)
#: Loaders that read a tag through a `source=` switch instead of naming it in
#: the docstring. Verified by reading each one — `load_nfl_players` defaults to
#: nflverse's asset and only reads this release when asked for the SDV source.
PY_EXTRA = {
    "nfl_players": [["sportsdataverse.nfl", "load_nfl_players", 'source="sdv"']],
    "nfl_rosters": [
        ["sportsdataverse.nfl", "load_nfl_rosters", 'seasons=[SEASON], source="sdv"']
    ],
    "nfl_player_stats": [
        ["sportsdataverse.nfl", "load_nfl_player_stats", 'source="sdv"']
    ],
    "nfl_team_stats": [
        ["sportsdataverse.nfl", "load_nfl_team_stats", 'seasons=[SEASON], source="sdv"']
    ],
    "nfl_model_pbp": [
        ["sportsdataverse.nfl", "load_nfl_pbp", 'seasons=[SEASON], source="sdv"']
    ],
    "nfl_espn_qbr": [
        ["sportsdataverse.nfl", "load_nfl_espn_qbr", 'seasons=[SEASON], source="sdv"']
    ],
    "nfl_ratings_weekly": [
        ["sportsdataverse.nfl", "load_nfl_ratings_weekly", "seasons=[SEASON]"]
    ],
}


def step_py_loaders() -> None:
    src = ROOT / "sportsdataverse-py" / "sportsdataverse"
    if not src.is_dir():
        warn("no sportsdataverse-py checkout, keeping existing py_tag_loaders.json")
        return
    found: dict[str, list] = defaultdict(list)
    for f in src.rglob("*.py"):
        for fn, _sig, tag in PY_LOADER.findall(f.read_text(errors="replace")):
            mod = str(f.relative_to(src.parent)).replace("/", ".")[:-3]
            found[tag].append([".".join(mod.split(".")[:-1]), fn, ""])

    # A name in a docstring is not proof the function is reachable: import the
    # subpackage and confirm the attribute is really there before publishing it.
    venv = ROOT / "sportsdataverse-py" / ".venv" / "bin" / "python"
    if venv.exists():
        code = (
            "import json,sys,importlib\n"
            "found=json.load(sys.stdin)\n"
            "out={};cache={}\n"
            "for t,v in found.items():\n"
            "    keep=[]\n"
            "    for pkg,fn,sig in v:\n"
            "        if pkg not in cache:\n"
            "            try: cache[pkg]=importlib.import_module(pkg)\n"
            "            except Exception as e: cache[pkg]=e\n"
            "        m=cache[pkg]\n"
            "        if not isinstance(m,Exception) and hasattr(m,fn): keep.append([pkg,fn,sig])\n"
            "    if keep: out[t]=keep\n"
            "print(json.dumps(out))"
        )
        out = subprocess.run(
            [str(venv), "-c", code],
            input=json.dumps(found),
            capture_output=True,
            text=True,
        )
        if out.returncode == 0:
            found = json.loads(out.stdout)
        else:
            warn(f"import verification skipped: {out.stderr.strip()[-200:]}")
    else:
        warn("no sportsdataverse-py venv — loaders NOT import-verified")

    for tag, v in PY_EXTRA.items():
        found.setdefault(tag, v)
    write("py_tag_loaders.json", dict(found))


# --- 7. R package loaders ---------------------------------------------------
R_DEF = re.compile(r"^([A-Za-z_.][A-Za-z0-9_.]*)\s*(?:<-|=)\s*function\s*\(")
#: Tag prefixes whose R loader is named for the league shorthand instead.
ALIAS = [
    ("espn_mens_college_basketball", "mbb"),
    ("espn_womens_college_basketball", "wbb"),
    ("espn_cfb", "cfb"),
    ("espn_nba", "nba"),
    ("espn_wnba", "wnba"),
    ("cfbfastR_cfb", "cfb"),
]


def _norm(tag: str) -> str:
    for a, b in ALIAS:
        if tag.startswith(a):
            return b + tag[len(a) :]
    return tag


def _toks(s: str) -> set[str]:
    return {w[:-1] if len(w) > 3 and w.endswith("s") else w for w in s.split("_")}


def step_r_loaders() -> None:
    tagfile = HERE / "tags.txt"
    if not tagfile.exists():
        warn("tags.txt missing — run the releases step first")
        return
    tags = tagfile.read_text().split()
    tag_re = re.compile(
        "|".join(re.escape(t) for t in sorted(tags, key=len, reverse=True))
    )
    hits: dict[str, set] = defaultdict(set)
    for pkg in R_PKGS:
        d = ROOT / pkg / "R"
        if not d.is_dir():
            continue
        for f in d.glob("*.R"):
            cur = None
            for line in f.read_text(errors="replace").splitlines():
                m = R_DEF.match(line)
                if m:
                    cur = m.group(1)
                if line.lstrip().startswith("#"):
                    continue  # roxygen mentions are documentation, not a read
                if cur and cur.startswith("load_"):
                    for t in set(tag_re.findall(line)) & set(tags):
                        hits[t].add((pkg, cur, f.name))

    def score(tag: str, fn: str) -> float:
        tt, ft = _toks(_norm(tag)), _toks(fn.replace("load_", ""))
        return len(tt & ft) / max(1, len(tt | ft))

    # NAMESPACE is the gate: a function defined in R/ but not exported is not
    # callable as pkg::fn(), so promising one would be wrong.
    exports = {}
    for pkg in R_PKGS:
        ns = ROOT / pkg / "NAMESPACE"
        exports[pkg] = (
            set(re.findall(r"^export\(([^)]+)\)", ns.read_text(errors="replace"), re.M))
            if ns.exists()
            else set()
        )

    out = {}
    for t, v in hits.items():
        ranked = sorted(v, key=lambda x: (-score(t, x[1]), x[1]))
        top = score(t, ranked[0][1])
        keep = [
            list(x)
            for x in ranked
            if score(t, x[1]) >= max(0.34, top - 0.2)
            and x[1] in exports.get(x[0], set())
        ][:4]
        if keep:
            out[t] = keep
    write("r_tag_loaders.json", out)


STEPS = {
    "releases": step_releases,
    "hits": step_hits,
    "db_catalog": step_db_catalog,
    "orch": step_orch,
    "workflows": step_workflows,
    "py_loaders": step_py_loaders,
    "r_loaders": step_r_loaders,
}


def main() -> None:
    wanted = sys.argv[1:] or list(STEPS)
    unknown = [w for w in wanted if w not in STEPS]
    if unknown:
        sys.exit(f"unknown step(s): {', '.join(unknown)}\nknown: {', '.join(STEPS)}")
    for name in wanted:
        print(f"[{name}]")
        STEPS[name]()


if __name__ == "__main__":
    main()
