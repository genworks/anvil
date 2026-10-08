#!/bin/sh
# worker.sh -- the apps host's worker, stood in for: every queued hosting
# request in the lab is taken, its check reported as passed, its review
# (held: the gate holds no reviewer key here) released through the admin
# door, its deploy reported as up.  Runs in a curl container on the rig's
# network (`run worker`).  The forge's Deployments follow within a minute.
L=http://lab:9080/prompt-lab; HS=$(cat /e2e/hosting-secret); AS=$(cat /e2e/admin-secret)
lab() { curl -s -H 'Host: lab' -H "X-Hosting-Secret: $HS" "$@"; }
adm() { curl -s -H 'Host: lab' -H "X-Hosting-Admin-Secret: $AS" "$@"; }
state() { curl -s -H 'Host: lab' "$L/api/hosting/state?id=$1" | sed -n 's/.*"state":"\([^"]*\)".*/\1/p'; }
for i in 1 2 3 4 5 6; do
  N=$(lab $L/api/hosting/next); ID=$(echo "$N" | sed -n 's/.*"id":"\([^"]*\)".*/\1/p'); [ -n "$ID" ] || { echo "nothing queued"; break; }
  SHA=$(echo "$N" | sed -n 's/.*"sha":"\([0-9a-f]\{8\}\).*/\1/p'); echo "== $ID at $SHA: $(echo "$N" | sed -n 's/.*"action":"\([^"]*\)".*/\1/p')"
  lab -X POST $L/api/hosting/report -H 'Content-Type: application/json' -d "{\"id\":\"$ID\",\"action\":\"check\",\"ok\":true,\"lines\":[\"PASS built\",\"PASS /healthz 200\"],\"name\":\"hello-toll\",\"image\":\"apps/hello-toll:$SHA\",\"pages\":[{\"path\":\"/\",\"body\":\"<h1>Hello toll</h1>\"}]}" > /dev/null
  echo "after the check: $(state $ID)"; sleep 25; echo "after the review: $(state $ID)"
  adm -X POST $L/api/hosting/decide -H 'Content-Type: application/json' -d "{\"id\":\"$ID\",\"release\":true,\"note\":\"released by the rig\"}" > /dev/null
  N=$(lab $L/api/hosting/next); echo "the worker is handed: $(echo "$N" | sed -n 's/.*"action":"\([^"]*\)".*/\1/p')"
  lab -X POST $L/api/hosting/report -H 'Content-Type: application/json' -d "{\"id\":\"$ID\",\"action\":\"deploy\",\"ok\":true,\"lines\":[\"up\"]}" > /dev/null
  echo "state: $(state $ID)"
done
