NCAA MBB **within-team** RAPM, seasons 2010-2026, from the stats.ncaa.org lineup stack.

## Read this before comparing to any public RAPM

**This is WITHIN-TEAM RAPM.** It apportions one team's performance across that
team's own players using that team's lineup splits. It is **not** the
league-wide estimand that regresses every lineup in the league jointly, and the
numbers are **not** comparable to league-wide RAPM (Torvik, EvanMiya, etc.).
The tag name says `within_team` for exactly this reason; `ncaa_mbb_rapm`
is reserved for a future league-wide (Path B) dataset.

## Columns

| column | notes |
|---|---|
| `season` | ending year (2024 = the 2023-24 season) |
| `team`, `team_id` | team_id joined from `ncaa_mbb_team_rosters` |
| `player_code` | the box-score DISPLAY name ("Surname, First"); not stable across seasons |
| `player_id` | provider per-season roster-entry id |
| `person_id` | synthetic key, **stable across seasons and teams** |
| `rapm_off`, `rapm_def`, `rapm_net` | the estimates |
| `team_off_poss`, `num_players` | team-season context |

## Identity resolution

`player_code` is a display name, and NCAA retro-updates roster pages to a
player's *current* surname while the play-by-play keeps the surname as of the
game — so neither name nor per-season id is stable across seasons. Use
`person_id` for anything cross-season.

id match rate: **mbb 99.82% / wbb 99.72%**. Unmatched rows keep their RAPM
values with null ids. **Ambiguity is nulled, never guessed** — a name matching
more than one unclaimed roster row, or a match that would assign one roster
player to two rows in a team-season, is left null. A missing id is recoverable;
a wrong attribution silently is not.

Built by `ops/publish_rapm.py` in sportsdataverse/ncaa-mbb-hoops-data.
