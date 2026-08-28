NCAA College Football season ROSTERS, ESPN-native.

One asset naming pattern across every season: `cfb_rosters_{season}.{parquet,rds,csv}`.

Grain: one row per (season, team_id, athlete_id). An athlete who transfers
mid-season legitimately appears twice in a season, once per team -- do NOT
de-duplicate on athlete_id alone. Attribute values come from the athlete's LAST
game roster of the season; `games_rostered` counts the appearances.

Columns (78, identical in every season):
* identity -- season, team_id, athlete_id, division (fbs/fcs)
* position -- position_id, position, position_abbreviation, position_name,
  position_leaf, position_parent_id. RESOLVED against ESPN's 74-entry league
  position reference; earlier releases shipped only `position_href` (a URL).
  `position_leaf = false` marks a grouping node (Offense/Defense/Special
  Teams/Athlete/Unknown), not an assignable position. Ids 0 and 99 are both
  "Unknown" -- a non-null position does not mean ESPN knew the position.
* the full ESPN athlete + team field union (names, jersey, height/weight, age,
  DOB, birthplace, hand, citizenship, experience/class, status, draft, headshot,
  team identity/colors/logos, and the source hrefs).

Coverage: 2004-2025, FBS **and** FCS. Pre-2014 rosters are roughly 4x thinner
than 2014+ -- an ESPN per-game participant coverage cliff, not a scrape gap.
2020 is short (COVID cancellations).

Built by cfbfastR-cfb-data `python -m cfb_data_build --dataset cfb_rosters`
from the per-game roster blocks committed in cfbfastR-cfb-raw
(`cfb/game_rosters/json/`), read over HTTP. Strategy notes, the full column
union and every gotcha: cfbfastR-cfb-raw `docs/ESPN_ROSTERS.md`.

SUPERSEDES the previous assets on this tag, which were two datasets under one
name: `roster_2004..2022` were mirrors of the CFBD-sourced rosters (still served
by `sportsdataverse.cfb.load_cfb_rosters` from cfbfastR-data), and
`rosters_2023..2025` were ESPN-derived, FBS-only, and had no usable position
column. Both have been removed in favour of one consistent ESPN-native series.
