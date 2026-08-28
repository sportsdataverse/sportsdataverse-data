# Release-note generator

Rebuilds the description on every release in this repository from the sibling
repos themselves, so the notes stay true as the pipelines move.

Each release description ends up with six sections: what the files are, how to
load them (Python / R / SQL / HTTP / plain URL), the production chain, the
automation driving it, what depends on it downstream, and its sibling releases.

## Running it

```sh
cd data-raw
python3 collect_inputs.py     # gather every input (see below)
python3 build_inventory.py    # join them into one per-tag lineage record
python3 render_notes.py       # -> bodies/*.md
DRY=1 bash push_notes.sh      # preview what would change
bash push_notes.sh            # publish
```

`collect_inputs.py` takes step names if you only need part of a refresh, e.g.
`python3 collect_inputs.py releases workflows`. Steps are `releases`, `hits`,
`db_catalog`, `orch`, `workflows`, `py_loaders`, `r_loaders`. A step that cannot
reach its source warns and leaves the previous file alone, so a partial refresh
degrades to a stale input rather than an empty one.

Set `SDV_REPOS` if the sibling checkouts are not under `/mnt/sdv_repos`.

`push_notes.sh` compares each live body to the rendered one and sends only what
differs, so it is safe to re-run and safe to interrupt.

## What is derived vs. curated

Derived from the repos, and therefore self-correcting:

| Fact | Source |
|---|---|
| Producing repo, build script, publish step | the tag scan (`tag_hits.txt`) |
| Pipeline stages, scripts, season windows | `sdv-orch/sdv_orch/registry.py` |
| GitHub Actions schedules and current health | each repo's workflows + `bin/workflow_health.log` |
| Warehouse table and partition column | the `sdv-db` catalog `REGISTRY` |
| Python loaders | the `"""Load <tag> (sportsdataverse-data release)` docstring, then verified by importing each one |
| R loaders | the enclosing `load_*` function, then verified against `NAMESPACE` exports |
| Asset counts, formats, season ranges, sizes | the GitHub releases API |

Curated in `families.py`, because the producing code assembles these tag names
at runtime (`_T + "pbp"`, `f"{sport}_model_artifacts"`) and no literal string
exists to find:

- the league title, upstream provider and season-key convention per family
- the NCAA and model-publish producer chains
- the `KIND` glossary that turns a tag suffix into a sentence

## Rules the generator enforces

1. **No dead links.** Every repo path is checked against the local checkout
   before it is emitted; anything unprovable is dropped rather than published.
2. **No false claims.** A loader is only named if it is exported (R) or
   importable (Python). `load_nfl_players` looks like the obvious loader for
   `nfl_players` but reads nflverse's asset by default, so it is listed with the
   `source="sdv"` switch that actually reaches this release.
3. **Hand-written notes survive.** Any prior description over 250 characters is
   kept verbatim under `## Dataset notes`. Twenty-five releases carry column
   dictionaries, model cards and methodology warnings that must not be
   regenerated away; `backup_bodies/` holds every prior description.
4. **Idempotent publish.** Re-running sends only what changed.

## Traps worth remembering

- **Never add `*.json` to the tag scan globs.** In this workspace that means
  every raw ESPN and NCAA payload ever captured, which turns an 8-second scan
  into a 12-minute one.
- **`.uv-cache` lives inside the workspace on purpose** (it keeps ~29 venvs
  near-free). ripgrep skips it via gitignore rules; plain `grep -r` does not,
  and it alone contributes ~190k spurious matches. The fallback path excludes it
  explicitly.
- **This directory excludes itself from the scan.** The rendered notes and their
  backups quote every tag by name, so counting them would make each release look
  like a consumer of itself.

## Restoring a release description

```sh
gh release edit <tag> -R sportsdataverse/sportsdataverse-data \
  --notes-file backup_bodies/<tag>.md
```
