#!/bin/sh
# gate-push.sh -- the prompt lab's push, made as the lab and the visitor make it:
# staged at the gate, the visitor signed in at the forge (OAuth with PKCE),
# the consent given, the push approved, the merge request merged by a maintainer.
# Runs in a curl container on the rig's network (`run push`); /e2e is the work
# directory.  Cookies are carried by hand: curl's jar drops cookies for a host
# name without a dot.
set -e
G=http://cyclops/_git; F=http://anvil:9095; TOKEN=$(cat /e2e/token)
ck() { sed -n 's/^[Ss][Ee][Tt]-[Cc][Oo][Oo][Kk][Ii][Ee]: *\([^;]*\);.*/\1/p' | head -1; }
loc() { sed -n 's/^[Ll][Oo][Cc][Aa][Tt][Ii][Oo][Nn]: *//p' | tr -d '\r' | head -1; }
strip() { sed 's/<style>.*<\/style>//; s/<[^>]*>/ /g' | tr -s ' \n'; }
echo "== 1 stage (as the lab)"
S=$(curl -s -X POST $G/stage -H 'Content-Type: application/json' -d '{"session":"e2e-1","project_id":1,"title":"From the e2e lab","description":"e2e push","files":[{"path":"NOTES.md","content":"Pushed through the gate at e2e.\n"}]}')
echo "$S"; ID=$(echo "$S" | sed -n 's/.*"push":"\([^"]*\)".*/\1/p')
echo "== 2 the approve page, not signed in, sends to login; login sends to the forge"
curl -s -D - -o /dev/null "$G/approve?push=$ID" | loc
H=$(curl -s -D - -o /dev/null "$G/login?push=$ID"); GS=$(echo "$H" | ck); AUTH=$(echo "$H" | loc); echo "gate cookie: $GS"; echo "to: ${AUTH%%&*}..."
Q=${AUTH#*\?}
echo "== 3 forge: authorize without a session sends to sign-in"
curl -s -D - -o /dev/null "$AUTH" | loc | cut -c1-60
echo "== 4 forge: sign in"
H=$(curl -s -D - -o /dev/null -X POST $F/users/sign_in --data-urlencode login=dave --data-urlencode password=password123 --data-urlencode "return_to=/oauth/authorize?$Q"); FS=$(echo "$H" | ck); echo "forge cookie: $FS"
echo "== 5 forge: the consent page, then Authorize"
curl -s -H "Cookie: $FS" "$AUTH" | strip | grep -o 'asks to act on.*scope [a-z]*' | head -1
H=$(curl -s -D - -o /dev/null -H "Cookie: $FS" -X POST $F/oauth/authorize -d "$Q&decision=allow"); CB=$(echo "$H" | loc); echo "callback: ${CB%%code=*}code=..."
echo "== 6 gate: the callback"
H=$(curl -s -D - -o /dev/null -H "Cookie: $GS" "$CB"); GU=$(echo "$H" | ck); echo "user cookie: $GU; then: $(echo "$H" | loc)"
echo "== 7 gate: the approve page"
P=$(curl -s -H "Cookie: $GS; $GU" "$G/approve?push=$ID"); CSRF=$(echo "$P" | sed -n 's/.*name="token" value="\([^"]*\)".*/\1/p'); echo "$P" | strip | grep -o 'Signed in as.*merge request:' | head -1
echo "== 8 gate: push"
curl -s -H "Cookie: $GS; $GU" -X POST $G/approve -d "push=$ID&token=$CSRF" | strip | grep -oE '(Pushed|Not pushed).*' | head -c 400; echo
echo "== 9 forge: the merge request, merged by the maintainer"
MR=$(curl -s -H "PRIVATE-TOKEN: $TOKEN" "$F/api/v4/projects/1/merge_requests?state=opened&source_branch=prompt-lab-e2e-1" | sed -n 's/.*"iid":\([0-9]*\).*/\1/p' | head -1)
echo "merge request $MR"; curl -s -H "PRIVATE-TOKEN: $TOKEN" -X PUT "$F/api/v4/projects/1/merge_requests/$MR/merge" | head -c 200; echo
echo "== 10 master after the merge"
curl -s "$F/api/v4/projects/1/repository/files/NOTES.md/raw?ref=master"
curl -s "$F/api/v4/projects/1/repository/commits?ref_name=master" | sed 's/},{/}\n{/g' | sed -n 's/.*"title":"\([^"]*\)".*/\1/p' | head -3
