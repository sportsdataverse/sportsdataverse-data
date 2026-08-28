#!/usr/bin/env bash
# Push the rendered release bodies to sportsdataverse/sportsdataverse-data.
#
# Resumable: a tag whose live body already matches the rendered file is skipped,
# so re-running after an interruption only sends what is still outstanding.
# Every prior body is already saved under backup_bodies/ -- restore with
#   gh release edit <tag> -R $REPO --notes-file backup_bodies/<tag>.md
set -u

SP="$(cd "$(dirname "$0")" && pwd)"
REPO="sportsdataverse/sportsdataverse-data"
LOG="$SP/push.log"
DRY="${DRY:-0}"

ok=0; skip=0; fail=0
echo "=== push started $(date -u +%FT%TZ) DRY=$DRY ===" >> "$LOG"

for f in "$SP"/bodies/*.md; do
    tag="$(basename "$f" .md)"
    live="$(gh release view "$tag" -R "$REPO" --json body --jq .body 2>/dev/null)"
    # GitHub normalises trailing whitespace, so compare on the trimmed text
    if [ "$(printf '%s' "$live" | sed -e 's/[[:space:]]*$//')" = "$(sed -e 's/[[:space:]]*$//' "$f")" ]; then
        skip=$((skip + 1)); continue
    fi
    if [ "$DRY" = "1" ]; then
        echo "WOULD UPDATE $tag ($(wc -c < "$f") chars)" >> "$LOG"
        ok=$((ok + 1)); continue
    fi
    if gh release edit "$tag" -R "$REPO" --notes-file "$f" >/dev/null 2>>"$LOG"; then
        ok=$((ok + 1)); echo "OK   $tag" >> "$LOG"
    else
        fail=$((fail + 1)); echo "FAIL $tag" >> "$LOG"
    fi
done

echo "updated=$ok skipped=$skip failed=$fail" | tee -a "$LOG"
echo "EXIT=$fail"
