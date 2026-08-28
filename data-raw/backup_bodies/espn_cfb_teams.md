College Football team + conference reference, one asset per season (2001-present).

One row per (season, team): ESPN team identity, conference membership, branding and venue **as of that season** — not a static dimension table.

- `division` is ESPN group membership: group 80 = FBS, group 81 = FCS.
- Those groups also carry bowl all-star / exhibition squads (12 of 2023 group-80 entries). ESPN's own `isAllStar` catches only a third of them, so filter on the derived `is_exhibition` for a real-program count.
- `conference_*` is the conference, not the division within it (`team_group_*` keeps the immediate group, e.g. "SEC - West" under "Southeastern Conference").
- `team_id` / `conference_id` / `venue_id` are Int64 in every season.

Built by cfbfastR-cfb-data `python -m cfb_data_build --dataset teams` from the raw season bundles in cfbfastR-cfb-raw (`cfb/teams/json/{season}.json`).

**CFBD backport (2026-08-27).** 17 columns from the per-season `cfb_team_info`
release now travel with the row: `school`, `mascot`, `alt_name1..3`,
`cfbd_conference`, `classification`, `city`, `state`, `country_code`,
`timezone`, `latitude`, `longitude`, `elevation`, `capacity`, `dome`, `grass`.

- The join is LEFT on `team_id`. ESPN's universe is much larger than CFBD's
  (2023: 857 vs 672), so a large unmatched tail — almost entirely D-II / D-III /
  NAIA — carries nulls in these 17 and is **not** dropped. Per-season match rate
  runs 35% (pre-2014, when CFBD covered only D-I) to ~87%.
- `classification` is CFBD's independent opinion and coexists with ESPN's
  `division` / `is_fbs`; neither replaces the other. Comparing
  `is_fbs & !is_exhibition` against `classification == "fbs"` they agree exactly
  in 15 of the 22 seasons 2004-2025.
- `cfbd_conference` is named apart from the `conference_*` family on purpose:
  ESPN resolves a team to its conference GROUP, CFBD to its own string, and they
  disagree.
- No previously published column changed: all 37 are byte-identical to the prior
  build for every season.