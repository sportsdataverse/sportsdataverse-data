"""Curated per-family metadata for sportsdataverse-data release notes.

Source of truth for WHICH repo produces a tag: ``status/producers.json`` in
sportsdataverse/.github (its ordered ``rules``; the nightly ``status/summary.json``
resolves every live tag). This file only adds release-note prose, and each family's
``build`` repo must agree with that map (``repo: null`` there = no public producer,
so no ``raw``/``build`` here). ``python3 families.py`` checks the agreement.

Everything here is checked against the repos: the raw/data repo names and stage
scripts come from ``sdv-orch/sdv_orch/registry.py``, the workflow filenames from
each repo's ``.github/workflows/``, and the package names from the R DESCRIPTION
files. What the greps CANNOT recover is here because the producing code builds
the tag at runtime (``_T + "pbp"``, ``f"{sport}_model_artifacts"``), so no
literal tag string exists to find.
"""

from __future__ import annotations

ORG = "https://github.com/sportsdataverse"
DATA_REPO = f"{ORG}/sportsdataverse-data"
DL = f"{DATA_REPO}/releases/download"

# --- families ---------------------------------------------------------------
# Matched longest-prefix-first. Keys:
#   title       league/provider prose used in the header
#   provider    upstream data source
#   raw         (repo, human description of the capture step)
#   build       (repo, human description of the build step)
#   publish     how assets reach this tag
#   orch        sdv-orch pipeline key (registry.PIPELINES)
#   workflows   [(repo, workflow file)] that drive it on GitHub Actions
#   r_pkg       R package that ships the loader
#   py_mod      sportsdataverse-py subpackage
#   note        anything a consumer must know (season key, gaps, freezes)

# --- conference / division reference ({league}_groups), built by sdv-reference-data. Listed
# --- first: family_of() takes the first matching prefix, and `nba_` / `mlb_` families follow.
_GROUP_LEAGUES = {
    "cfb": ("College football", "the **starting** year (2025 = fall 2025)"),
    "mbb": ("Men's college basketball", "the **ending** year (2025 = 2024-25)"),
    "wbb": ("Women's college basketball", "the **ending** year (2025 = 2024-25)"),
    "nfl": ("NFL", "the **starting** year (2025 = the 2025 season)"),
    "nba": ("NBA", "the **ending** year (2025 = 2024-25)"),
    "wnba": ("WNBA", "the single calendar year"),
    "mlb": ("MLB", "the single calendar year"),
    "nhl": ("NHL", "the **ending** year (2025 = 2024-25)"),
    "ncaa_baseball": ("College baseball", "the single (spring) year"),
    "ncaa_softball": ("College softball", "the single (spring) year"),
}
_GROUPS: list[tuple[str, dict]] = [
    (
        f"{lg}_groups",
        {
            "title": f"{title} conference, division and subdivision reference",
            "provider": "the league's membership source and every source's names and ids for its groups (see the build repo)",
            "raw": ("sdv-reference-data", f"`fetch()` in `sdv_reference/leagues/{lg}.py` snapshots the sources into `raw/{lg}/`"),
            "build": ("sdv-reference-data", f"`scripts/pipeline/20_build_tables.sh {lg}` (offline, from `raw/` and cited `curated/` rows)"),
            "publish": f"`scripts/pipeline/30_publish_releases.sh {lg}` in sdv-reference-data",
            "season_key": key,
            "note": (
                "A **reference table**, not observations. Four tables: `groups` (one row per SDV group lineage, "
                "`{league}:{slug}`), `group_seasons` (each group's name, abbreviation and parent **as of that season**), "
                "`group_aliases` (every source's ids and names for a group, with validity windows) and "
                "`team_group_seasons` (each team's subdivision, conference and division by season, one file per season). "
                "Historical names come from dated, cited rows: ESPN, stats.ncaa.org and CFBD all show today's names for "
                "past seasons. Schema: sdv-reference-data `CONTRACT.md`."
            ),
        },
    )
    for lg, (title, key) in _GROUP_LEAGUES.items()
]

_PARKS: list[tuple[str, dict]] = [
    (
        "mlb_parks",
        {
            "title": "MLB ballpark dimensions by venue and season",
            "provider": "the MLB Stats API's per-season venue `fieldInfo`, with cited curated corrections where it lags a fence move",
            "raw": ("sdv-reference-data", "`fetch()` in `sdv_reference/parks/mlb.py` snapshots the venues into `raw/mlb_parks/`"),
            "build": ("sdv-reference-data", "`scripts/pipeline/20_build_tables.sh mlb_parks` (offline, from `raw/` and `curated/mlb_park_overrides.csv`)"),
            "publish": "`scripts/pipeline/30_publish_releases.sh mlb_parks` in sdv-reference-data",
            "season_key": "the single calendar year",
            "note": (
                "A **reference table**, not observations: one row per MLB-used venue per season (2001 on; the API repeats one undated "
                "record per venue before that), with the seven outfield fence distances in feet, capacity, turf, roof, azimuth, "
                "elevation, location and the Retrosheet park id. Where the API lags a real fence change (Camden 2022, Petco 2013-14, "
                "T-Mobile, Comerica), a cited correction wins. Schema: sdv-reference-data `CONTRACT.md`."
            ),
        },
    ),
]

# --- ESPN league-wide snapshots, published for every league by cfbfastR-cfb-data. Listed
# --- before the per-league `espn_*_` families below, which would otherwise claim them.
_SNAPSHOT_LEAGUES = {
    "cfb": "college football",
    "nfl": "NFL",
    "nba": "NBA",
    "wnba": "WNBA",
    "mbb": "men's college basketball",
    "wbb": "women's college basketball",
    "nhl": "NHL",
    "mlb": "MLB",
}
_SNAPSHOTS: list[tuple[str, dict]] = [
    (
        f"espn_{lg}_{kind}",
        {
            "title": f"ESPN {_SNAPSHOT_LEAGUES[lg]} {what} (daily snapshot)",
            "provider": "ESPN's league-wide endpoint, one request per league",
            "build": (
                "cfbfastR-cfb-data",
                f"`python/espn_{kind}_daily_snapshot.py`, run daily for every league by `espn_daily_snapshots.yml`",
            ),
            "publish": "the same snapshot script with `--publish`",
            "orch": None,
            "r_pkg": None,
            "py_mod": None,
            "note": "ESPN reports **current state only**, so history exists only as these daily snapshots; each row carries the `as_of_date` it was captured.",
        },
    )
    for kind, what, leagues in (
        ("injuries", "injury report", tuple(_SNAPSHOT_LEAGUES)),
        ("depthcharts", "depth charts", ("nfl", "nba", "mlb")),
    )
    for lg in leagues
]

# --- legacy ESPN box-score tags of the CFBD-side cfbfastR-data build (its stages are switched off).
_CFB_LEGACY_BOX: list[tuple[str, dict]] = [
    (
        f"espn_cfb_{side}_boxscores",
        {
            "title": f"ESPN college football {side} box scores (legacy)",
            "provider": "ESPN college-football API",
            "build": ("cfbfastR-data", "the legacy R box-score stages, currently switched off"),
            "publish": "`piggyback::pb_upload()` from the R build",
            "orch": None,
            "r_pkg": "cfbfastR",
            "py_mod": "sportsdataverse.cfb",
        },
    )
    for side in ("player", "team")
]

FAMILIES: list[tuple[str, dict]] = _GROUPS + _PARKS + _SNAPSHOTS + _CFB_LEGACY_BOX + [
    # --- identity crosswalks: reference tables, not models. Matched first so the
    # --- broader `nba_` / `mbb_` model prefixes below do not claim them.
    (
        "nba_crosswalk",
        {
            "title": "NBA identity crosswalks",
            "provider": "the ESPN and provider ids already captured for this league",
            "raw": ("hoopR-nba-raw", "the league's existing raw capture — no separate scrape"),
            "build": ("hoopR-nba-data", "`R/nba_11_team_crosswalk_creation.R`, `nba_12_schedule_crosswalk_creation.R`, `nba_13_player_crosswalk_creation.R`"),
            "publish": "uploaded to this tag by the same build that writes it",
            "orch": "nba",
            "r_pkg": "hoopR",
            "py_mod": "sportsdataverse.nba",
            "note": "A crosswalk is a **join key table**, not observations: it maps the same team, player or game between ESPN, the league's own stats site and the SportsDataverse ids, so datasets from different providers can be joined without fuzzy name matching.",
        },
    ),
    (
        "wnba_crosswalk",
        {
            "title": "WNBA identity crosswalks",
            "provider": "the ESPN and provider ids already captured for this league",
            "raw": ("wehoop-wnba-raw", "the league's existing raw capture — no separate scrape"),
            "build": ("wehoop-wnba-data", "`R/wnba_11_team_crosswalk_creation.R`, `wnba_12_schedule_crosswalk_creation.R`, `wnba_13_player_crosswalk_creation.R`"),
            "publish": "uploaded to this tag by the same build that writes it",
            "orch": "wnba",
            "r_pkg": "wehoop",
            "py_mod": "sportsdataverse.wnba",
            "note": "A crosswalk is a **join key table**, not observations: it maps the same team, player or game between ESPN, the league's own stats site and the SportsDataverse ids, so datasets from different providers can be joined without fuzzy name matching.",
        },
    ),
    (
        "mbb_crosswalk",
        {
            "title": "Men's college basketball identity crosswalks",
            "provider": "the ESPN and provider ids already captured for this league",
            "raw": ("hoopR-mbb-raw", "the league's existing raw capture — no separate scrape"),
            "build": ("hoopR-mbb-data", "`python/espn_mbb_11_team_crosswalk_creation.py` (team, via the sportsdataverse-py builder), `R/mbb_12_schedule_crosswalk_creation.R`, `R/mbb_13_player_crosswalk_creation.R`"),
            "publish": "uploaded to this tag by the same build that writes it",
            "orch": "mbb",
            "r_pkg": "hoopR",
            "py_mod": "sportsdataverse.mbb",
            "note": "A crosswalk is a **join key table**, not observations: it maps the same team, player or game between ESPN, the league's own stats site and the SportsDataverse ids, so datasets from different providers can be joined without fuzzy name matching.",
        },
    ),
    (
        "wbb_crosswalk",
        {
            "title": "Women's college basketball identity crosswalks",
            "provider": "the ESPN and provider ids already captured for this league",
            "raw": ("wehoop-wbb-raw", "the league's existing raw capture — no separate scrape"),
            "build": ("wehoop-wbb-data", "`python/espn_wbb_13_team_crosswalk_creation.py`, `espn_wbb_14_schedule_crosswalk_creation.py`, `espn_wbb_15_player_crosswalk_creation.py` (the R scripts remain as a fallback)"),
            "publish": "uploaded to this tag by the same build that writes it",
            "orch": "wbb",
            "r_pkg": "wehoop",
            "py_mod": "sportsdataverse.wbb",
            "note": "A crosswalk is a **join key table**, not observations: it maps the same team, player or game between ESPN, the league's own stats site and the SportsDataverse ids, so datasets from different providers can be joined without fuzzy name matching.",
        },
    ),
    (
        "cfb_crosswalk",
        {
            "title": "College football identity crosswalks",
            "provider": "the ESPN and provider ids already captured for this league",
            "raw": ("cfbfastR-cfb-raw", "the league's existing raw capture — no separate scrape"),
            "build": ("cfbfastR-cfb-data", "`python/build_cfb_crosswalk.py`"),
            "publish": "uploaded to this tag by the same build that writes it",
            "orch": "cfb",
            "r_pkg": "cfbfastR",
            "py_mod": "sportsdataverse.cfb",
            "note": "A crosswalk is a **join key table**, not observations: it maps the same team, player or game between ESPN, the league's own stats site and the SportsDataverse ids, so datasets from different providers can be joined without fuzzy name matching.",
        },
    ),
    (
        "espn_cfb_",
        {
            "title": "ESPN college football",
            "provider": "ESPN college-football API (`site.api` + `core.api`)",
            "raw": (
                "cfbfastR-cfb-raw",
                "per-game JSON captured into `cfb/json/{season}/`",
            ),
            "build": (
                "cfbfastR-cfb-data",
                "R creation scripts in `R/` plus the Python shadow in `python/cfb_data_build/`",
            ),
            "publish": "`python/cfb_data_build/publish.py` (parquet + csv + rds per season)",
            "orch": "cfb",
            "r_pkg": "cfbfastR",
            "py_mod": "sportsdataverse.cfb",
            "season_key": "season = the STARTING year of the season (2024 = the 2024-25 bowl cycle).",
        },
    ),
    (
        "cfbfastR_cfb_",
        {
            "title": "cfbfastR CFBD-sourced college football",
            "provider": "collegefootballdata.com (CFBD) API",
            "raw": ("cfbfastR-data", "weekly CFBD pulls driven by `week.R`"),
            "build": ("cfbfastR-data", "`R/` creation scripts"),
            "publish": "`piggyback::pb_upload()` from the R build",
            "orch": None,
            "r_pkg": "cfbfastR",
            "py_mod": "sportsdataverse.cfb",
            "note": "This is the **CFBD-sourced** play-by-play, distinct from the ESPN-sourced `espn_cfb_pbp`. It carries `cfbfastR`'s EPA/WPA columns.",
        },
    ),
    (
        "cfb_",
        {
            "title": "College football models and reference tables",
            "provider": "CFBD + ESPN, scored through the cfbfastR model stack",
            "raw": (
                "cfbfastR-cfb-raw",
                "the same ESPN capture that feeds `espn_cfb_*`",
            ),
            "build": (
                "cfbfastR-cfb-data",
                "`python/cfb_model_build/` (ratings, FPI, recruiting, model artifacts)",
            ),
            "publish": "`python/cfb_model_build/cfb_model_publish/cli.py --tag <this tag>`",
            "orch": "cfb_models",
            "r_pkg": "cfbfastR",
            "py_mod": "sportsdataverse.cfb",
        },
    ),
    (
        "espn_mens_college_basketball_",
        {
            "title": "ESPN men's college basketball",
            "provider": "ESPN men's-college-basketball API",
            "raw": (
                "hoopR-mbb-raw",
                "per-game JSON captured into `mbb/json/{season}/`",
            ),
            "build": (
                "hoopR-mbb-data",
                "`R/` creation scripts plus `python/mbb_data_build/`",
            ),
            "publish": "`piggyback` from the R build / `publish.py` from the Python build",
            "orch": "mbb",
            "r_pkg": "hoopR",
            "py_mod": "sportsdataverse.mbb",
            "season_key": "season = the ENDING year of the season (2025 = 2024-25).",
        },
    ),
    (
        "espn_womens_college_basketball_",
        {
            "title": "ESPN women's college basketball",
            "provider": "ESPN women's-college-basketball API",
            "raw": (
                "wehoop-wbb-raw",
                "per-game JSON captured into `wbb/json/{season}/`",
            ),
            "build": (
                "wehoop-wbb-data",
                "`R/` creation scripts plus `python/wbb_data_build/`",
            ),
            "publish": "`piggyback` from the R build / `publish.py` from the Python build",
            "orch": "wbb",
            "r_pkg": "wehoop",
            "py_mod": "sportsdataverse.wbb",
            "season_key": "season = the ENDING year of the season (2025 = 2024-25).",
        },
    ),
    (
        "espn_nba_",
        {
            "title": "ESPN NBA",
            "provider": "ESPN NBA API",
            "raw": (
                "hoopR-nba-raw",
                "per-game JSON captured into `nba/json/{season}/`",
            ),
            "build": (
                "hoopR-nba-data",
                "`R/` creation scripts plus `python/nba_data_build/`",
            ),
            "publish": "`piggyback` from the R build / `publish.py` from the Python build",
            "orch": "nba",
            "r_pkg": "hoopR",
            "py_mod": "sportsdataverse.nba",
            "season_key": "season = the ENDING year of the season (2025 = 2024-25).",
        },
    ),
    (
        "espn_wnba_",
        {
            "title": "ESPN WNBA",
            "provider": "ESPN WNBA API",
            "raw": (
                "wehoop-wnba-raw",
                "per-game JSON captured into `wnba/json/{season}/`",
            ),
            "build": (
                "wehoop-wnba-data",
                "`R/` creation scripts plus `python/wnba_data_build/`",
            ),
            "publish": "`piggyback` from the R build / `publish.py` from the Python build",
            "orch": "wnba",
            "r_pkg": "wehoop",
            "py_mod": "sportsdataverse.wnba",
            "season_key": "WNBA plays inside one calendar year, so season = that year.",
        },
    ),
    (
        "nba_stats_",
        {
            "title": "NBA Stats (stats.nba.com)",
            "provider": "stats.nba.com official endpoints",
            "raw": (
                "hoopR-nba-stats-raw",
                "endpoint JSON archived per game / per parameter combination",
            ),
            "build": (
                "hoopR-nba-stats-data",
                "`python/nba_data_build/` (and the R processor for the legacy tags)",
            ),
            "publish": "`python/nba_data_build/publish.py`",
            "orch": "nba_stats",
            "r_pkg": "hoopR",
            "py_mod": "sportsdataverse.nba",
            "loader_season": -1,
            "season_key": "Assets are named with the season's **ENDING** year since the 2026-08-13 re-key (`2025` = 2024-25). `hoopR` and `sportsdataverse-py` still take the STARTING year as their `seasons` argument and translate.",
            "note": "stats.nba.com blocks datacenter IPs, so this pipeline runs from a residential connection rather than GitHub-hosted runners.",
        },
    ),
    (
        "wnba_stats_",
        {
            "title": "WNBA Stats (stats.wnba.com)",
            "provider": "stats.wnba.com official endpoints",
            "raw": (
                "wehoop-wnba-stats-raw",
                "endpoint JSON archived per game / per parameter combination",
            ),
            "build": (
                "wehoop-wnba-stats-data",
                "`python/wnba_data_build/` plus the R processor for the legacy tags",
            ),
            "publish": "`python/wnba_data_build/publish.py`",
            "orch": "wnba_stats",
            "r_pkg": "wehoop",
            "py_mod": "sportsdataverse.wnba",
            "season_key": "WNBA plays inside one calendar year, so season = that year.",
        },
    ),
    (
        "nhl_xg_models",
        {
            "title": "NHL expected-goals model artifacts",
            "provider": "NHL play-by-play, fitted into an expected-goals model",
            "publish": "no publishing code exists in any public repository; the assets are read by `fastRhockey-nhl-raw` and `sportsdataverse-py`",
            "orch": None,
            "r_pkg": None,
            "py_mod": "sportsdataverse.nhl",
        },
    ),
    (
        "nhl_",
        {
            "title": "NHL",
            "provider": "NHL public API (`api-web.nhle.com`)",
            "raw": (
                "fastRhockey-nhl-raw",
                "per-game JSON captured into `nhl/json/{season}/`",
            ),
            "build": (
                "fastRhockey-nhl-data",
                "`python/nhl_data_build/` (Python is canonical; the R build is the legacy path)",
            ),
            "publish": "`python/nhl_data_build/publish.py`",
            "orch": "nhl",
            "r_pkg": "fastRhockey",
            "py_mod": "sportsdataverse.nhl",
            "season_key": "season = the ENDING year of the season (2025 = 2024-25).",
        },
    ),
    (
        "pwhl_",
        {
            "title": "PWHL",
            "provider": "PWHL / HockeyTech `lscluster` feeds",
            "raw": (
                "fastRhockey-pwhl-raw",
                "per-game JSON captured into `pwhl/json/{season}/`",
            ),
            "build": ("fastRhockey-pwhl-data", "`python/pwhl_data_build/`"),
            "publish": "`python/pwhl_data_build/publish.py`",
            "orch": "pwhl",
            "r_pkg": "fastRhockey",
            "py_mod": "sportsdataverse.pwhl",
            "season_key": "season = the ENDING year of the season. The league's first season is 2024.",
        },
    ),
    (
        "phf_",
        {
            "title": "PHF / NWHL (archived)",
            "provider": "PHF (formerly NWHL) HockeyTech feeds",
            "publish": "frozen — the assets are final",
            "orch": None,
            "r_pkg": "fastRhockey",
            "py_mod": None,
            "archived": True,
            "note": "The PHF ceased operations in 2023 and its assets were folded into the PWHL. No public producer code writes these tags. These files are a **frozen archive**: they will not be updated. `fastRhockey`'s `load_phf_*` and `phf_*` functions are formally deprecated. For current women's professional hockey use the `pwhl_*` releases.",
        },
    ),
    (
        "ncaa_mbb_",
        {
            "title": "NCAA men's basketball (stats.ncaa.org)",
            "provider": "stats.ncaa.org official box scores and play-by-play",
            "raw": (
                "ncaa-mbb-hoops-raw",
                "`scripts/run_discover.sh` → `run_capture.sh` → `run_parse.sh`",
            ),
            "build": (
                "ncaa-mbb-hoops-data",
                "numbered creation scripts in `python/`, registered in `python/ncaa_mbb_data_build/config.py`",
            ),
            "publish": "`scripts/run_publish.sh` → `python/ncaa_mbb_data_build/publish.py`",
            "orch": "ncaa_mbb",
            "r_pkg": "hoopR",
            "py_mod": "sportsdataverse.mbb",
            "season_key": "season = the ENDING year of the season (2025 = 2024-25).",
            "note": "stats.ncaa.org bans aggressive clients, so capture runs from the SportsDataverse droplet under a rate budget rather than on GitHub-hosted runners.",
            "creation_dir": ("ncaa-mbb-hoops-data", "python", "ncaa_mbb_"),
        },
    ),
    (
        "ncaa_wbb_",
        {
            "title": "NCAA women's basketball (stats.ncaa.org)",
            "provider": "stats.ncaa.org official box scores and play-by-play",
            "raw": (
                "ncaa-wbb-hoops-raw",
                "`scripts/run_discover.sh` → `run_capture.sh` → `run_parse.sh`",
            ),
            "build": (
                "ncaa-wbb-hoops-data",
                "numbered creation scripts in `python/`, registered in `python/ncaa_wbb_data_build/config.py`",
            ),
            "publish": "`scripts/run_publish.sh` → `python/ncaa_wbb_data_build/publish.py`",
            "orch": "ncaa_wbb",
            "r_pkg": "wehoop",
            "py_mod": "sportsdataverse.wbb",
            "season_key": "season = the ENDING year of the season (2025 = 2024-25).",
            "note": "stats.ncaa.org bans aggressive clients, so capture runs from the SportsDataverse droplet under a rate budget rather than on GitHub-hosted runners.",
            "creation_dir": ("ncaa-wbb-hoops-data", "python", "ncaa_wbb_"),
        },
    ),
    (
        "ncaa_mfb_",
        {
            "title": "NCAA football (stats.ncaa.org)",
            "provider": "stats.ncaa.org official box scores and play-by-play",
            "raw": (
                "ncaa-mfb-football-raw",
                "`scripts/run_mfb_capture.sh` (FBS and FCS) then `scripts/run_05_datasets.sh`",
            ),
            "build": (
                "ncaa-mfb-football-data",
                "numbered creation scripts in `python/`, registered in `python/ncaa_mfb_data_build/config.py`",
            ),
            "publish": "`scripts/run_publish.sh` → `python/ncaa_mfb_data_build/publish.py`",
            "orch": "ncaa_mfb",
            "r_pkg": None,
            "py_mod": None,
            "season_key": "season = the STARTING year (2025 = the fall-2025 season). Only the `-raw` repo speaks the NCAA academic year, which is season + 1.",
            "note": "Coverage floor is fall 2013: stats.ncaa.org publishes no football box scores or play-by-play before that. ESPN game ids are joined in at stage 06 and reach roughly 88% of games.",
            "creation_dir": ("ncaa-mfb-football-data", "python", "ncaa_mfb_"),
        },
    ),
    (
        "ncaa_baseball_",
        {
            "title": "NCAA baseball (stats.ncaa.org)",
            "provider": "stats.ncaa.org official box scores and play-by-play",
            "raw": (
                "baseballr-data",
                "`scripts/run_01_schedules_scrape.sh` → `run_02_games_scrape.sh` → `run_04_rosters_scrape.sh`",
            ),
            "build": (
                "baseballr-data",
                "`scripts/run_03_games_parse.sh` → `run_05_datasets_build.sh` → `run_06_xwalk_build.sh`",
            ),
            "publish": "`scripts/run_07_datasets_publish.sh` → `python/ncaa_baseball_data_build/publish.py`",
            "orch": "ncaa_baseball",
            "r_pkg": "baseballr",
            "py_mod": "sportsdataverse.mlb",
            "season_key": "season = the calendar year the season is played in.",
            "creation_dir": ("baseballr-data", "python", "ncaa_baseball_"),
        },
    ),
    (
        "mlb_",
        {
            "title": "MLB model datasets",
            "provider": "Statcast / MLB StatsAPI, scored through the baseballr model stack",
            "raw": (
                "baseballr-data",
                "Statcast pull driven by the daily Statcast workflow",
            ),
            "build": ("baseballr-data", "`python/mlb_model_publish/builders.py`"),
            "publish": "`python/mlb_model_publish/cli.py`",
            "orch": None,
            "r_pkg": "baseballr",
            "py_mod": "sportsdataverse.mlb",
            "season_key": "season = the calendar year.",
        },
    ),
    (
        "nfl_ngs_",
        {
            "title": "NFL Next Gen Stats",
            "provider": "the public NFL Next Gen Stats API",
            "raw": ("nfl-ngs-raw", "`scripts/daily_ngs_scraper.sh`, run by `scrape_ngs_raw.yml`"),
            "build": ("nfl-ngs-data", "`scripts/daily_ngs_data_processor.sh`, run by `daily_ngs.yml`"),
            "publish": "`scripts/daily_ngs_data_processor.sh` (publishes by default)",
            "orch": None,
            "r_pkg": None,
            "py_mod": "sportsdataverse.nfl",
            "season_key": "season = the STARTING year of the season (2024 = the 2024-25 playoffs).",
            "note": "Read these with `load_nfl_ngs(seasons, dataset=)`. They are a different source from `load_nfl_nextgen_stats`, which reads the nflverse release.",
        },
    ),
    (
        "espn_nfl_",
        {
            "title": "ESPN NFL",
            "provider": "ESPN NFL API",
            "raw": ("nfl-raw", "`scripts/daily_nfl_scraper.sh` into `nfl/json/{season}/`"),
            "build": ("nfl-data", "`scripts/espn_nfl_data.sh`, run by `espn_nfl_cron.yml`"),
            "publish": "`PUBLISH=1 bash scripts/espn_nfl_data.sh` uploads each season to its `espn_nfl_*` tag",
            "orch": None,
            "r_pkg": None,
            "py_mod": "sportsdataverse.nfl",
            "season_key": "season = the STARTING year of the season (2024 = the 2024-25 playoffs).",
        },
    ),
    (
        "nfl_",
        {
            "title": "NFL",
            "provider": "ESPN NFL API and nflverse inputs, scored through the sportsdataverse NFL model stack",
            "raw": (
                "nfl-raw",
                "`scripts/daily_nfl_scraper.sh` into `nfl/json/{season}/`",
            ),
            "build": (
                "nfl-data",
                "`python/nfl_model_publish/builders.py` and the dataset builders alongside it",
            ),
            "publish": "`python/nfl_model_publish/cli.py`",
            "orch": None,
            "r_pkg": None,
            "py_mod": "sportsdataverse.nfl",
            "season_key": "season = the STARTING year of the season (2024 = the 2024-25 playoffs).",
            "note": "There is no SportsDataverse R loader for these; the R-side equivalent lives in the nflverse (`nflreadr`, `nflfastR`).",
        },
    ),
    (
        "mbb_",
        {
            "title": "Men's college basketball models",
            "provider": "ESPN and NCAA inputs, scored through the hoopR model stack",
            "raw": (
                "hoopR-mbb-raw",
                "the same ESPN capture that feeds `espn_mens_college_basketball_*`",
            ),
            "build": ("hoopR-mbb-data", "`python/mbb_model_publish/builders.py`"),
            "publish": "`python/mbb_model_publish/cli.py`",
            "orch": "mbb",
            "r_pkg": "hoopR",
            "py_mod": "sportsdataverse.mbb",
        },
    ),
    (
        "wbb_",
        {
            "title": "Women's college basketball models",
            "provider": "ESPN and NCAA inputs, scored through the wehoop model stack",
            "raw": (
                "wehoop-wbb-raw",
                "the same ESPN capture that feeds `espn_womens_college_basketball_*`",
            ),
            "build": ("wehoop-wbb-data", "`python/wbb_model_publish/builders.py`"),
            "publish": "`python/wbb_model_publish/cli.py`",
            "orch": "wbb",
            "r_pkg": "wehoop",
            "py_mod": "sportsdataverse.wbb",
        },
    ),
    (
        "nba_",
        {
            "title": "NBA models",
            "provider": "stats.nba.com inputs, scored through the hoopR model stack",
            "raw": (
                "hoopR-nba-stats-raw",
                "the same endpoint archive that feeds `nba_stats_*`",
            ),
            "build": ("hoopR-nba-stats-data", "`python/nba_model_publish/builders.py`"),
            "publish": "`python/nba_model_publish/cli.py`",
            "orch": "nba_stats",
            "r_pkg": "hoopR",
            "py_mod": "sportsdataverse.nba",
        },
    ),
    (
        "wnba_",
        {
            "title": "WNBA models",
            "provider": "stats.wnba.com inputs, scored through the wehoop model stack",
            "raw": (
                "wehoop-wnba-stats-raw",
                "the same endpoint archive that feeds `wnba_stats_*`",
            ),
            "build": (
                "wehoop-wnba-stats-data",
                "`python/wnba_model_publish/builders.py`",
            ),
            "publish": "`python/wnba_model_publish/cli.py`",
            "orch": "wnba_stats",
            "r_pkg": "wehoop",
            "py_mod": "sportsdataverse.wnba",
        },
    ),
]

# --- what the trailing dataset token means ----------------------------------
KIND = {
    "leaguedash": "the LeagueDash parameter cube: every measure type (base, advanced, misc, scoring, usage, defense) for players, teams, lineups and tracking, one asset per measure per season",
    "pbp": "play-by-play, one row per play",
    "pbp_full": "full-detail play-by-play, one row per event with every parsed field",
    "pbp_lite": "trimmed play-by-play carrying the columns most analyses actually use",
    "pbp_cfbfastr": "play-by-play reshaped into the `cfbfastR` column contract",
    "player_box": "player box scores, one row per player per game",
    "team_box": "team box scores, one row per team per game",
    "player_boxscores": "player box scores, one row per player per game",
    "team_boxscores": "team box scores, one row per team per game",
    "goalie_boxscores": "goaltender box scores, one row per goalie per game",
    "skater_boxscores": "skater box scores, one row per skater per game",
    "schedule": "the game schedule, one row per game",
    "schedules": "the game schedule, one row per game",
    "rosters": "rosters, one row per player",
    "team_rosters": "team-season rosters, one row per player per team-season",
    "game_rosters": "game rosters, one row per player per game (who actually dressed)",
    "teams": "the team reference table",
    "team_ids": "the team id reference table used to join every other dataset",
    "team_info": "team reference information (conference, division, venue, colors)",
    "officials": "game officials, one row per official per game",
    "shots": "shot events with location, one row per shot",
    "shots_by_period": "shot totals by period, one row per team per period",
    "lineups": "lineup units and their on-court results",
    "game_lineups": "per-game lineup stints",
    "matchup_stints": "matchup stints, one row per contiguous on-court matchup",
    "possessions": "derived possessions, one row per possession",
    "standings": "standings, one row per team per season",
    "player_season_stats": "season-level player statistics",
    "team_season_stats": "season-level team statistics",
    "player_game_logs": "player game logs, one row per player per game",
    "player_core": "the core player reference table (biographical and identity fields)",
    "player_stats": "player statistics",
    "team_stats": "team statistics",
    "situational_stats": "situational splits",
    "linescore": "line scores, one row per team per period",
    "linescores": "line scores, one row per team per period",
    "drives": "drives, one row per drive",
    "penalties": "penalties, one row per penalty",
    "penalty_summary": "the penalty summary, one row per penalty",
    "scoring": "scoring plays, one row per goal",
    "scoring_summary": "the scoring summary, one row per goal",
    "shootout": "shootout attempts, one row per attempt",
    "scratches": "healthy scratches, one row per scratched player per game",
    "shifts": "shift data, one row per shift",
    "three_stars": "the three stars of the game",
    "game_info": "game-level metadata",
    "injuries": "the injury report",
    "betting": "closing betting lines and odds",
    "draft": "draft results, one row per pick",
    "crosswalk": "identity crosswalks that map teams, players and games between providers",
    "coaches": "coaches, one row per coach per game",
    "power_index": "ESPN's power index (FPI) ratings",
    "percentiles": "team percentile ranks across the summary measures",
    "league_averages": "season baselines: the mean, median, standard deviation and n of every published team and player metric, per level, over the same qualified population the ranks and percentiles use",
    "team_opponent_splits": "by-opponent team splits: one row per team per game with the opponent, EPA per play, success rate, points for and against and plays; college covers every game (FCS opponents and bowls included), NFL the regular season only, where game_id is empty before 2002 and nflverse_game_id identifies every game",
    "rolling_windows": "rolling event-count windows: each player's and team's form over its last N events vs the previous N, the season start and its career baseline, with delta ranks and sample sizes, one asset per season",
    "poll_analytics": "weekly AP, Coaches and CFP poll history with each team's rank, previous rank, move, entry and exit flags, weeks ranked, points and first-place votes, one asset per season (2004+; every run refetches the season from ESPN)",
    "poll_week_summary": "per-poll week summaries: volatility (population sd of rank change with unranked = 26), chaos (sum of absolute rank changes), entries and exits, one asset per season (2004+)",
    "metric_curves": "rate curves along a continuous axis: FG% by kick distance, completion% and EPA by air-yards bucket, 4th-down conversion by yards to go and success by down x distance, each bucket with attempts, successes, rate and EPA per attempt, for the league, every team and every credited player, one asset per season (FG curves 2004+, air-yards curves 2025+)",
    "team_summaries": "team season summaries",
    "team_summaries_weekly": "team season summaries snapshotted weekly",
    "ratings": "team ratings",
    "ratings_weekly": "team ratings snapshotted weekly",
    "player_value": "player value estimates",
    "player_impact": "player impact estimates",
    "rapm": "regularized adjusted plus-minus ratings",
    "rapm_within_team": "regularized adjusted plus-minus estimated within teams",
    "model_artifacts": "serialized model artifacts (the fitted objects themselves, not observations)",
    "model_pbp": "play-by-play with the model's scored columns attached",
    "xg_models": "expected-goals model artifacts",
    "xg_pbp": "play-by-play with expected-goals columns attached",
    "4th_down_models": "fourth-down decision model artifacts",
    "espn_qbr": "ESPN's Total QBR",
    "game_state": "game-state reference tables (run expectancy, win expectancy, WPA)",
    "hitting_models": "hitting model outputs (expected stats, expected home runs, projections)",
    "pitching_models": "pitching model outputs (xERA, Stuff+, Command+)",
    "fielding_models": "fielding model outputs (outs above average, catcher framing)",
    "fpi_weekly": "ESPN's Football Power Index snapshotted weekly",
    "recruits": "recruiting classes, one row per recruit",
    "recruiting_proj": "recruiting projections",
    "returning_production": "returning production estimates",
    "team_talent": "247Sports team talent composite",
    "players": "the player reference table",
    "adv_team": "advanced team box score",
    "adv_passing": "advanced passing box score",
    "adv_rushing": "advanced rushing box score",
    "adv_receiving": "advanced receiving box score",
    "adv_defensive": "advanced defensive box score",
    "adv_defensive_players": "advanced defensive box score, split to the player level",
    "adv_drives": "advanced drive-level box score",
    "adv_situational": "advanced situational box score (down, distance, field position)",
    "adv_specialists": "advanced special-teams box score",
    "adv_turnover": "advanced turnover box score",
    "adv_team_gamelog": "advanced team game logs",
    "play_participants": "play participants, one row per player per play",
    "passing": "passing statistics",
    "rushing": "rushing statistics",
    "receiving": "receiving statistics",
    "games": "game-level records, one row per game",
}


if __name__ == "__main__":
    # Check every live tag's family against the nightly status snapshot, the source of truth.
    # Exit codes: 0 = every tag agrees, 1 = at least one mismatch, 2 = the snapshot could not be read
    # (so a network or parse failure is never mistaken for a producer mismatch).
    import json
    import sys
    import urllib.error
    import urllib.request

    url = "https://raw.githubusercontent.com/sportsdataverse/.github/main/status/summary.json"
    try:
        with urllib.request.urlopen(url, timeout=30) as r:
            tags = json.load(r)["release_tags"]
    except (urllib.error.URLError, TimeoutError, json.JSONDecodeError, KeyError) as e:
        print(f"could not read the status snapshot at {url}: {e!r}", file=sys.stderr)
        raise SystemExit(2) from e
    bad = []
    for t in tags:
        meta = next((m for prefix, m in FAMILIES if t["tag"].startswith(prefix)), None)
        build = meta.get("build") if meta else None
        mine = f"sportsdataverse/{build[0]}" if build else None
        if mine != t["producer"]:
            bad.append(f"{t['tag']}: families.py={mine} status={t['producer']}")
    print("\n".join(bad) or f"OK: {len(tags)} tags agree with status/summary.json")
    raise SystemExit(1 if bad else 0)
