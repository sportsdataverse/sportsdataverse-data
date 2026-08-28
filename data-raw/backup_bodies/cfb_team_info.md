College Football team information from the CollegeFootballData API
(`cfbfastR::cfbd_team_info(year, only_fbs = FALSE)`), **one asset per season**.

Grain: one row per (season, team). Team identity is as-of that season, so a
team's `conference` / `classification` reflects where it played THAT year --
this is not an all-time dimension table.

Seasons 2001-2026. Parquet + RDS per season.

Rebuilt 2026-08-27: the previous `cfbfastR-data` files were a single all-time
snapshot copied under per-season filenames (1,840 rows for every year 2001-2024,
`classification` 63% null). These assets are genuine per-season API output.