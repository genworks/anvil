# The lab end to end

A rig that runs common-lisp.net's prompt lab, its Cyclops gate and a
fresh Anvil on one Docker network, so that everything the lab and the
gate ask of GitLab is asked of Anvil instead: opening a project, the
push through the gate (OAuth with PKCE, a commit through the API, a
merge request), hosting through the gate (the lab's Deploy button),
Anvil's own Deploy button and `:deploy` job, and the lab's hosting
state machine driven by a stand-in for the apps host's worker.  First
run 2026-10-08; every step passed once the fixes of that day were in.

It is a second Compose project (`anvil-e2e`) built from the clnet
deployment's own `docker-compose.yml`, so the preview deployment on
the same machine is not touched.  Nothing here is for production.

## What it needs

- The checkouts: this repository, `monocle`, `genworks/demos` (the
  lab) and `clo/cl-site` (the lab's start-up file and compose file).
- A Gendl image (`genworks/gendl:devo-ccl`) and a Cyclops image of
  2026-10-05 or later (`gornskew/cyclops:master`; an older one has no
  `/_git/stage-deploy` door).
- Docker, with its socket: the forge's runner executes jobs through it.

## Running it

```console
$ ci/lab-e2e/run up        # the forge, then the gate and the lab (a few minutes)
$ ci/lab-e2e/run push      # the lab's push, as the lab and the visitor make it
$ ci/lab-e2e/run deploy    # Deploy both ways: through the gate, and Anvil's own
$ ci/lab-e2e/run worker    # the apps host's worker, every queued request to live
$ ci/lab-e2e/run browser   # the lab's own buttons in headless chromium (see below)
$ ci/lab-e2e/run down
```

`run` reads `ANVIL_DIR`, `INGOT_DIR`, `MONOCLE_DIR`, `DEMOS_DIR`, `CLSITE_DEPLOY`
(the clnet `deploy/` directory) and `WORK` (where it keeps the
secrets, the token and the test repository; `/tmp/anvil-lab-e2e`),
each with a default beside this checkout.  The forge answers on
`127.0.0.1:19795` for a `git push` from the host and for a look; the
lab on `127.0.0.1:19782`, but only to its own name, `lab`.

The flows run in a throwaway `curlimages/curl` container on the rig's
network, because the names (`anvil`, `cyclops`, `lab`) resolve there
and nowhere else.  They carry cookies by hand: curl's cookie jar drops
cookies for a host name without a dot.

The browser pass needs chromium on that network; `browser.sh` is
written for the ready room of a Basilisk ship (`docker network connect
anvil-e2e_default <ready-room>`, then run it there).  It opens the
project in a new lab session, presses Deploy, follows the link to the
gate, signs in at Anvil, authorizes the gate, and presses Host.

## What stands in for production

- `lab-shim.lisp` loads the lab's real start-up file, then points the
  lab's approve links at the gate's own name: in production Apache
  forwards `/_git/` on the lab's name to the gate.
- The apps host and its worker are not here; `worker.sh` answers the
  lab's `hosting/next` and `hosting/report` doors as the worker would.
- The gate holds no API key, so the lab's review of a request cannot
  reach its reviewer and every request is held for a person;
  `worker.sh` releases it through the admin door.

## The rig's parts

| File | |
| --- | --- |
| `run` | the commands above |
| `compose.override.yml` | the `anvil` service, and the lab's and the gate's mounts and settings |
| `anvil-init.lisp` | the forge: data under `/tmp`, Monocle loaded, the house `common-lisp.app` at the lab, the user `dave`, the project `dave/hello-toll`, the gate's OAuth application; idempotent over a restart |
| `lab-shim.lisp`, `cyclops.sexp` | the lab's and the gate's settings for the rig |
| `gate-push.sh`, `gate-deploy.sh`, `worker.sh` | the flows, run in the curl container |
| `browser.sh` | the lab's buttons, run where chromium is |
