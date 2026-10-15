#!/bin/bash
# Copyright © 2026 Genworks International
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.  Distributed WITHOUT
# ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
#
# CI stress: push a repository that carries a ci.sexp (or .gitlab-ci.yml)
# to N new projects at once, then follow every pipeline to its end.  The
# runner's parallelism shows in how many run at a time.
#
#   ANVIL_BASE=https://forge.example ANVIL_USER=me \
#   ANVIL_TOKEN_FILE=~/.secrets/forge-token SRC=/path/to/a/checkout ./ci-stress.sh 3
#
# The projects are left for a look; `DELETE /api/v4/projects/<id>` clears them.
set -eu -o pipefail
B=${ANVIL_BASE:?set ANVIL_BASE to the address of the forge}
USER_NAME=${ANVIL_USER:?set ANVIL_USER}
TOKEN=$(tr -d '[:space:]' < "${ANVIL_TOKEN_FILE:?set ANVIL_TOKEN_FILE to a file holding an api-scope token}")
SRC=${SRC:?set SRC to a checkout with a CI file to push}
N=${1:-1}
AUTH="Authorization: Basic $(printf '%s:%s' "$USER_NAME" "$TOKEN" | base64 -w0)"
stamp=$(date +%s)
ids=()
for i in $(seq 1 "$N"); do
  name="ci-$stamp-$i"
  id=$(curl -4 -s -H "PRIVATE-TOKEN: $TOKEN" -H 'Content-Type: application/json' \
        -d "{\"name\":\"$name\",\"path\":\"$name\",\"visibility\":\"public\"}" "$B/api/v4/projects" \
        | python3 -c "import json,sys; print(json.load(sys.stdin)['id'])")
  ids+=("$id"); echo "project $name id=$id"
done
echo "## $N pushes at once -> $N pipelines"
t0=$(date +%s)
for i in $(seq 1 "$N"); do
  ( s=$(date +%s.%N); git -C "$SRC" -c "http.extraHeader=$AUTH" push -q "$B/$USER_NAME/ci-$stamp-$i.git" +HEAD:refs/heads/master 2>&1 | tail -2
    echo "push $i done in $(echo "$(date +%s.%N) - $s" | bc) s" ) &
done
wait
echo "## following the pipelines"
for round in $(seq 1 240); do
  all_done=1; line=""
  for id in "${ids[@]}"; do
    st=$(curl -4 -s --max-time 20 "$B/api/v4/projects/$id/pipelines" | python3 -c "
import json,sys
d=json.load(sys.stdin)
print(d[0]['status'] if d else 'none')" 2>/dev/null || echo "?")
    line="$line $id:$st"
    case "$st" in success|failed|canceled|skipped) ;; *) all_done=0;; esac
  done
  echo "t+$(( $(date +%s) - t0 ))s$line"
  [ $all_done = 1 ] && break
  sleep 15
done
echo "## jobs"
for id in "${ids[@]}"; do
  pid=$(curl -4 -s "$B/api/v4/projects/$id/pipelines" | python3 -c "import json,sys; print(json.load(sys.stdin)[0]['id'])")
  curl -4 -s "$B/api/v4/projects/$id/pipelines/$pid/jobs" | python3 -c "
import json,sys
for j in json.load(sys.stdin): print('  project $id pipeline $pid job', j['id'], j['name'], j['status'], 'duration', j.get('duration'), 'queued', j.get('queued_duration'))"
done
echo "done: projects ${ids[*]}"
