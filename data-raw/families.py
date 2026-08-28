"""Curated per-family metadata for sportsdataverse-data release notes.

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

FAMILIES: list[tuple[str, dict]] = [
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
            "build": ("hoopR-mbb-data", "`R/mbb_11_team_crosswalk_creation.R`, `mbb_12_schedule_crosswalk_creation.R`, `mbb_13_player_crosswalk_creation.R`"),
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
            "build": ("wehoop-wbb-data", "`R/wbb_13_team_crosswalk_creation.R`, `wbb_14_schedule_crosswalk_creation.R`, `wbb_15_player_crosswalk_creation.R`"),
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
            "raw": ("fastRhockey-nhl-raw", "historical capture, no longer running"),
            "build": ("fastRhockey-nhl-data", "historical build, no longer running"),
            "publish": "frozen — the assets are final",
            "orch": None,
            "r_pkg": "fastRhockey",
            "py_mod": None,
            "archived": True,
            "note": "The PHF ceased operations in 2023 and its assets were folded into the PWHL. These files are a **frozen archive**: they will not be updated. `fastRhockey`'s `load_phf_*` and `phf_*` functions are formally deprecated. For current women's professional hockey use the `pwhl_*` releases.",
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
