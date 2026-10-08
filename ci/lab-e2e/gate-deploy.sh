#!/bin/sh
# gate-deploy.sh -- Deploy both ways: the lab's button through the gate (the
# hosting hand-off to the lab), and Anvil's own Deploy button.  Runs in a curl
# container on the rig's network (`run deploy`).
set -e
G=http://cyclops/_git; F=http://anvil:9095; L=http://lab:9080/prompt-lab; TOKEN=$(cat /e2e/token); AS=$(cat /e2e/admin-secret)
ck() { sed -n 's/^[Ss][Ee][Tt]-[Cc][Oo][Oo][Kk][Ii][Ee]: *\([^;]*\);.*/\1/p' | head -1; }
loc() { sed -n 's/^[Ll][Oo][Cc][Aa][Tt][Ii][Oo][Nn]: *//p' | tr -d '\r' | head -1; }
strip() { sed 's/<style>.*<\/style>//; s/<[^>]*>/ /g' | tr -s ' \n'; }
echo "== the gate's question before it hosts: members/all/<user>"; curl -s -H "PRIVATE-TOKEN: $TOKEN" $F/api/v4/projects/1/members/all/1; echo
echo "== A1 stage-deploy (as the lab), then the visitor's sign-in at the forge"
S=$(curl -s -X POST $G/stage-deploy -H 'Content-Type: application/json' -d '{"session":"e2e-1","project_id":1}'); echo "$S"; ID=$(echo "$S" | sed -n 's/.*"push":"\([^"]*\)".*/\1/p')
H=$(curl -s -D - -o /dev/null "$G/login?push=$ID"); GS=$(echo "$H" | ck); AUTH=$(echo "$H" | loc); Q=${AUTH#*\?}
H=$(curl -s -D - -o /dev/null -X POST $F/users/sign_in --data-urlencode login=dave --data-urlencode password=password123 --data-urlencode "return_to=/oauth/authorize?$Q"); FS=$(echo "$H" | ck)
H=$(curl -s -D - -o /dev/null -H "Cookie: $FS" -X POST $F/oauth/authorize -d "$Q&decision=allow"); CB=$(echo "$H" | loc)
H=$(curl -s -D - -o /dev/null -H "Cookie: $GS" "$CB"); GU=$(echo "$H" | ck)
echo "== A2 the gate's consent page"
P=$(curl -s -H "Cookie: $GS; $GU" "$G/approve?push=$ID"); CSRF=$(echo "$P" | sed -n 's/.*name="token" value="\([^"]*\)".*/\1/p'); echo "$P" | strip | grep -o 'Signed in as.*apps host' | head -c 300; echo
echo "== A3 Host this project"
curl -s -H "Cookie: $GS; $GU" -X POST $G/approve -d "push=$ID&token=$CSRF" | strip | grep -o 'Sent to the apps host.*' | head -c 300; echo
echo "== A4 the lab's queue (admin door)"
curl -s -H 'Host: lab' -H "X-Hosting-Admin-Secret: $AS" $L/api/hosting/admin | sed 's/"history":\[[^]]*\]//g' | head -c 600; echo
echo "== B1 Anvil's own Deploy button"
H=$(curl -s -D - -o /dev/null -X POST $F/users/sign_in --data-urlencode login=dave --data-urlencode password=password123 --data-urlencode "return_to=/"); FS=$(echo "$H" | ck)
curl -s -D - -o /dev/null -H "Cookie: $FS" -X POST $F/dave/hello-toll/-/deployments -d "house=common-lisp.app" | loc
echo "== B2 the forge's deployments"
curl -s -H "PRIVATE-TOKEN: $TOKEN" $F/api/v4/projects/1/deployments | sed 's/},{/}\n{/g' | sed -n 's/.*"sha":"\([0-9a-f]\{8\}\).*"status":"\([^"]*\)".*"id":\([0-9]*\).*/deployment \3 \1 \2/p'
echo "== C the :deploy job: DEPLOY=on, then a commit on master"
curl -s -H "PRIVATE-TOKEN: $TOKEN" -X POST $F/api/v4/projects/1/variables -H 'Content-Type: application/json' -d '{"key":"DEPLOY","value":"on"}' > /dev/null
curl -s -H "PRIVATE-TOKEN: $TOKEN" -X POST $F/api/v4/projects/1/repository/commits -H 'Content-Type: application/json' -d '{"branch":"master","commit_message":"e2e: a push that deploys","actions":[{"action":"update","file_path":"NOTES.md","content":"Pushed, then deployed by the pipeline.\n"}]}' | sed -n 's/.*"short_id":"\([^"]*\)".*/commit \1/p'
sleep 20
PL=$(curl -s -H "PRIVATE-TOKEN: $TOKEN" "$F/api/v4/projects/1/pipelines?per_page=1" | sed -n 's/.*"id":\([0-9]*\).*/\1/p')
for J in $(curl -s -H "PRIVATE-TOKEN: $TOKEN" "$F/api/v4/projects/1/pipelines/$PL/jobs" | sed 's/},{/}\n{/g' | sed -n 's/.*"name":"deploy".*"id":\([0-9]*\).*/\1/p'); do echo "-- trace of the deploy job $J"; curl -s -H "PRIVATE-TOKEN: $TOKEN" "$F/api/v4/projects/1/jobs/$J/trace" | tail -5; done
