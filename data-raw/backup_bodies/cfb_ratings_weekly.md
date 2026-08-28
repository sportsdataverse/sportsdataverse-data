College Football opponent-adjusted team ratings as of the END OF EACH REGULAR-SEASON WEEK. LONG FORMAT: one asset per season carrying a `through_week` column with every week's cumulative snapshot stacked. The ridge is refit on everything up to week W, so this is NOT derivable by summing per-game rows.

**As-of semantics -- read before using this for projections.**

`through_week == W` is **inclusive of week W**: the snapshot contains games
PLAYED in week W. To project week W, use the `through_week == W - 1` row.
Filtering `through_week == W` and predicting week W leaks that week's results.

Verified empirically (2024, delta between consecutive snapshots vs games
actually played): 97.0% consistent with the inclusive reading, 58.7% with the
exclusive one.
