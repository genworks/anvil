#!/bin/bash
# browser.sh -- the lab's own buttons against Anvil, in headless chromium: open
# dave/hello-toll in a new lab session, press Deploy, follow the link to the
# gate, sign in at Anvil, authorize the gate, press Host.  Run where chromium is,
# on the rig's network (a Basilisk ready room: docker network connect
# anvil-e2e_default <ready-room>).  PROBE names the sluice probe driver; SHOTS
# where the screenshots go.
PROBE=${PROBE:-/projects/tmp-shots/sluice/probe.js}
S=${SHOTS:-/projects/tmp-shots/anvil-e2e}
mkdir -p "$S"
node "$PROBE" http://lab:9080/prompt-lab 5000 \
  --eval="(()=>{const i=document.querySelector('input[data-bind\\\\:gitproject]'); i.value='dave/hello-toll'; i.dispatchEvent(new Event('input',{bubbles:true})); return 'set '+i.value})()" \
  --wait=800 \
  --eval="(()=>{const b=[...document.querySelectorAll('button.pl-build')].find(b=>b.textContent.trim()==='Open'); if(!b) return 'no Open button'; b.disabled=false; b.click(); return 'clicked Open'})()" \
  --wait=10000 \
  --shot=$S/02-opened.png \
  --eval="'LAB '+location.href" \
  --eval="[...document.querySelectorAll('.pl-line, .pl-notice, .pl-error')].map(e=>e.textContent.trim()).filter(Boolean).join(' | ').slice(0,900)" \
  --eval="(()=>{const b=[...document.querySelectorAll('button.pl-build')].find(b=>b.textContent.trim().startsWith('Deploy to')); if(!b) return 'no Deploy button'; b.click(); return 'clicked '+b.textContent.trim()})()" \
  --wait=5000 \
  --shot=$S/03-deploy-staged.png \
  --eval="'HOSTURL '+((document.querySelector('a[data-attr\\\\:href=\"\$hosturl\"]')||{}).href)" \
  --eval="(()=>{const a=document.querySelector('a[data-attr\\\\:href=\"\$hosturl\"]'); if(!a||!a.href) return 'no host link'; location.href=a.href; return 'going to '+a.href})()" \
  --wait=4000 \
  --shot=$S/04-forge-signin.png \
  --eval="'AT '+location.href+' :: '+document.title" \
  --eval="(()=>{const l=document.querySelector('input[name=login]'); if(!l) return 'no sign-in form'; l.value='dave'; document.querySelector('input[name=password]').value='password123'; document.querySelector('form[action=\"/users/sign_in\"]').submit(); return 'signed in'})()" \
  --wait=4000 \
  --shot=$S/05-consent.png \
  --eval="'AT '+location.href+' :: '+document.body.innerText.replace(/\\s+/g,' ').slice(0,300)" \
  --eval="(()=>{const b=document.querySelector('button[name=decision][value=allow]'); if(!b) return 'no Authorize button'; b.click(); return 'authorized'})()" \
  --wait=4000 \
  --shot=$S/06-gate-host.png \
  --eval="'AT '+location.href+' :: '+document.body.innerText.replace(/\\s+/g,' ').slice(0,400)" \
  --eval="(()=>{const b=[...document.querySelectorAll('button')].find(b=>/Host this project/.test(b.textContent)); if(!b) return 'no Host button'; b.click(); return 'pressed Host'})()" \
  --wait=6000 \
  --shot=$S/07-hosted.png \
  --eval="'AT '+location.href+' :: '+document.body.innerText.replace(/\\s+/g,' ').slice(0,500)"
