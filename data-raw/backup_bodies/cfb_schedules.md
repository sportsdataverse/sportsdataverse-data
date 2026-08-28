College football schedules (CFBD-sourced), one asset trio per season.

Seasons 2001-2026, `cfb_schedules_{season}.{parquet,rds,csv.gz}`.

Rebuilt 2026-08-27 to fix sportsdataverse-py#9: seasons 2001-2022 had been built when `cfbfastR::cfbd_game_info()` defaulted to `season_type = "regular"`, so every bowl and playoff game was missing. The current default is "both"; all seasons are now refetched with it.

Schema note: CFBD's nested `playoff` bracket object is flattened to `playoff_*` scalar columns (competition, format, round, roundName, bracketSlot, homeSeed, awaySeed, bowlName), so all 26 seasons share one 39-column schema.