#!/usr/bin/env bash
# Put a release description back to what it was before the generator first ran.
#
#   ./restore_body.sh espn_cfb_pbp        # restore one tag
#   ./restore_body.sh --list              # tags with a stored prior description
#
# Prior descriptions live in backup_bodies.json, keyed by tag.
set -euo pipefail

SP="$(cd "$(dirname "$0")" && pwd)"
REPO="sportsdataverse/sportsdataverse-data"
BACKUP="$SP/backup_bodies.json"

if [ $# -ne 1 ]; then
    sed -n '2,7p' "$0" | sed 's/^# \{0,1\}//'
    exit 2
fi

if [ "$1" = "--list" ]; then
    python3 -c "import json;print('\n'.join(sorted(json.load(open('$BACKUP')))))"
    exit 0
fi

tag="$1"
tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT

# Absent and empty are different: an empty prior description is a real stored
# value, while a missing key means this tag was never backed up.
python3 - "$BACKUP" "$tag" "$tmp" <<'PY'
import json, sys
backup, tag, out = sys.argv[1], sys.argv[2], sys.argv[3]
data = json.load(open(backup))
if tag not in data:
    sys.exit(f"no stored description for {tag!r} — try --list")
open(out, "w").write(data[tag])
PY

gh release edit "$tag" -R "$REPO" --notes-file "$tmp"
echo "restored $tag"
