# CLAUDE.md — sportsdataverse-data

Central GitHub-release host for all SportsDataverse datasets: producer
`*-data` repos publish here as **release assets keyed by tag**, and the
R packages' `load_*()` helpers consume from those tags. This repo is ALSO
the small R package `sportsdataversedata` that ships the upload helpers,
plus the automation status hub (the README badges track every pipeline).

When this guide differs from the repo, treat `README.Rmd`, `R/*.R`, and
the live release list as authoritative.

- **License:** CC BY 4.0 (data) / package code per `DESCRIPTION`.
- **Default repo target:** `sportsdataverse/sportsdataverse-data`.

## What's in the git tree vs in releases

**Git tree (this repo) holds NO data.** It is a thin R package + status page:

- `R/upload.R` — `sportsdataverse_upload()`, `sportsdataverse_save()` + helpers.
- `R/gh_cli.R` — `gh`-CLI wrappers (`gh_cli_release_upload/_tags/_assets/_available/_rate_limits`).
- `R/zzz.R` — `.onLoad` retry wrapping (`purrr::insistently`) + `GITHUB_PAT`→`GH_TOKEN` reconciliation.
- `DESCRIPTION` / `NAMESPACE` / `man/` — roxygen-generated package scaffolds.
- `README.Rmd` → `README.md` — **generated**; the "Data releases" section enumerates
  releases live via the GitHub API (re-render needs network). Don't hand-edit `README.md`.
- `.github/workflows/cran-checks.yaml` — the only workflow here; polls CRAN status of the
  consumer packages (hoopR, wehoop, fastRhockey, baseballr, worldfootballR, sportyR, oddsapiR).

**Releases hold the data.** Every processed dataset is a GitHub release tagged by dataset
name; the CSV/RDS/Parquet/qs assets live there, not in the tree.

## Release-tag scheme + producer→consumer map

A tag is one dataset family. Each upload also attaches `timestamp.json` / `timestamp.txt`
(drives the README "Last updated" badges) and optional `package_function.json`.

Confirmed tag families (verify the live list before assuming others exist):

- **ESPN WNBA** (`wehoop-wnba-data`): `espn_wnba_pbp`, `espn_wnba_schedules`, `espn_wnba_rosters`,
  `espn_wnba_game_rosters`, `espn_wnba_officials`, `espn_wnba_player_boxscores`, `espn_wnba_player_season_stats`.
- **WNBA Stats** (`wehoop-wnba-stats-data`): `wnba_stats_pbp`, `wnba_stats_player_game_logs`, `wnba_stats_schedules`.
- **CFB** (`cfbfastR-data`): `espn_cfb_pbp`, `espn_cfb_rosters`, `cfbfastR_cfb_pbp`.
- **NHL** (`fastRhockey-nhl-data`): `nhl_pbp_full`, `nhl_pbp_lite`, `nhl_game_info`,
  `nhl_game_rosters`, `nhl_goalie_boxscores`, `nhl_linescore`.
- Basketball also covers NBA (`hoopR-nba-data`) and MBB (`hoopR-mbb-data`); baseball via `baseballr-data`.

**Pipeline shape:** a `*-raw` repo scrapes the source API on a seasonal schedule and pushes →
a `repository_dispatch` trigger fires the matching `*-data` repo → the `*-data` creation scripts
clean the data and call `sportsdataversedata::sportsdataverse_save()` to upload assets to the tag
(overwriting that season's assets) → the R package's `load_*()` downloads the release assets on demand.

## Conventions

- **Tag names are a load-bearing contract.** A `load_*()` helper hard-codes its release tag;
  renaming or deleting a tag silently breaks the consumer. Add new tags; do not rename existing ones.
- **Upload backend is the GitHub CLI, not piggyback.** `sportsdataverse_upload(files, tag, pkg_function=NULL, repo=..., overwrite=TRUE)`
  shells out to `gh release upload <tag> <files> -R <repo> --clobber`. `overwrite=TRUE` (`--clobber`)
  is the default, so re-uploads replace same-named assets in place. (`.onLoad` still patches
  `GITHUB_PAT`→`GH_TOKEN` — a piggyback-era quirk, since `gh` reads `GH_TOKEN`.)
- **`sportsdataverse_save()`** serializes a data frame to multiple formats (`file_types`,
  default subset of `c("rds","csv","parquet","qs","csv.gz")`), tags metadata, then uploads — the
  standard entrypoint producers call. Prefer it over calling `_upload()` directly.
- **Producers depend on this package as `sportsdataversedata`** (no hyphen in the namespace call).
- The release/automation surface is driven from `README.Rmd`; new pipelines should be added there
  (status table + release table) and the README re-rendered, not patched in `README.md`.

## Gotchas

- **`gh release upload` silently drops large assets on a single multi-file call.** Upload
  per-file (loop) for large or numerous assets rather than one batched invocation.
- **A stale "Last updated" badge means the pipeline stopped, not that the data is wrong.** The
  badge reads the tag's `timestamp.json`; a release with assets but no `timestamp.json` (pre-timestamp
  uploads) falls back to the release date.
- **`README.md` is generated** — edits there are clobbered on the next re-render. Edit `README.Rmd`.
- Uploads need a token: `gh` uses `GH_TOKEN`; CI workflows that set `GITHUB_PAT` are bridged in `.onLoad`.
- **Never add AI co-author trailers** (Claude/Copilot/etc.) to commits or PRs.

## Reference

- Producer repos: `wehoop-{wnba,wbb}-{raw,data}`, `wehoop-wnba-stats-{raw,data}`,
  `hoopR-{nba,mbb}-{raw,data}`, `cfbfastR-{raw,data}`, `fastRhockey-{nhl,pwhl}-{raw,data}`,
  `baseballr-data`, `nfl-{raw,data}`, `hoopR-kp-data` (KenPom, paywalled, committed in-repo).
- Consumer packages: hoopR, wehoop, cfbfastR, fastRhockey, baseballr (R `load_*()`); sportsdataverse-py (`load_*`).
- Live releases: <https://github.com/sportsdataverse/sportsdataverse-data/releases>
