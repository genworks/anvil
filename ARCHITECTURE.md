# Anvil: the architecture

Anvil is a software forge written in Common Lisp: repositories served
over Git's smart HTTP protocol, a web face for browsing them, issues,
merge requests, continuous integration that runs a project's existing
`.gitlab-ci.yml` or a native s-expression pipeline, a REST API with
GitLab's shapes, an OAuth2 provider, and a push-button deployment of a
project into a house that hosts applications (common-lisp.net's prompt
lab and `*.common-lisp.app`).  It is meant first for common-lisp.net,
whose services lean on exactly these features of GitLab, and is built
so that what already talks to GitLab there keeps working when pointed
at Anvil.

This file is the design.  `README.md` says how to run it; each source
file's opening comment says what that file does.

## The shape

Two systems, in load order:

| System | What | Depends on |
| --- | --- | --- |
| `ingot` | Git in Common Lisp: objects, packs, refs, trees, commits, diffs, merges, the smart HTTP protocol on both sides, archives.  No web server, no Gendl.  Usable on its own. | ironclad, chipz, salza2, babel, bordeaux-threads |
| `anvil` | The forge: the model and its storage, accounts and permissions, the web pages (Gendl), the REST API, OAuth2, CI (the YAML reader, the native pipeline, the runner), webhooks, deployment. | ingot, gendl (gwl), cl-who, cl-ppcre, yason, drakma, 3bmd |

Ingot is plain Common Lisp with a hand-written `ingot.asd`, so it loads
into any Lisp with ASDF alone.  Anvil is a cl-lite codebase: its `.asd`
is generated from the `.sexp` metadata files (`(cl-lite "/path/to/anvil/"
:create-asd-file? t)`; never edited by hand).  Both load on SBCL and Clozure CL; the only
implementation-specific code is the Unix-socket connection to the
Docker daemon (`source/docker.lisp`), behind one function with a TCP
fallback.

## Why Git is in Lisp

The images Anvil is deployed in carry no `git` binary, and the house
rule for these programs is that nothing in a request thread starts a
subprocess (a child process is a dependency on the host's process
machinery that the Lisp cannot see fail).  So Ingot reads and
writes the repository itself:

- **Objects.** Loose objects (zlib) and packfiles with their `.idx`
  (version 2), including `ofs-delta` and `ref-delta` entries; SHA-1
  through ironclad.  Objects are found loose first, then in every pack
  of the repository, with the pack directory re-read when it changes.
- **Refs.** Loose refs, `packed-refs`, `HEAD` as a symbolic ref.  An
  update writes the loose ref through a temporary file and a rename,
  compare-and-swap against the old value under the repository's lock.
- **Trees and history.** Tree walks, a path's entry at a commit, the
  commit walk newest first (by committer date, with a path filter for
  a file's history), merge bases, the set of objects reachable from
  some commits and not from others (what a fetch sends).
- **Writing.** Blobs, trees and commits as loose objects; a commit made
  from a base tree and a list of file actions (create, update, delete,
  move), which is what the web editor and the API's commit endpoint
  use, and what a merge writes.
- **Diff and merge.** Tree diffs by path, a Myers line diff for text,
  a binary check; a fast-forward merge where one is possible, else a
  three-way tree merge with a per-file diff3; a conflict stops the
  merge and says where.
- **The protocol.** `pkt-line`; the refs advertisement for both
  services; `upload-pack` (wants, haves, `done`; `side-band-64k`,
  `ofs-delta` not used in what we send, `no-progress`, `include-tag`,
  `shallow` refused politely) answering a pack written on the spot,
  undeltified; `receive-pack` (the commands, the pack, which may be
  thin, indexed and resolved into loose objects, the refs updated, the
  `report-status`).  Both are functions from octets to octets, so the
  HTTP layer is thin and the same code could serve ssh one day.
- **Archives.** A tree at a commit as a tar (ustar) or tar.gz, which is
  also how the CI runner hands a job its working tree.

A pack written by Anvil has no deltas, so a clone is larger than one
from `git`; the client re-packs on its side.  If a deployment ever has
`git` available and wants its packs, `ingot:*external-git*` is the
seam to delegate the two services to it.

## The model and its storage

Everything the forge knows apart from the repositories is a record: a
plist with a `:kind`, kept in memory in tables and written to disk as
one s-expression file per record under `<data>/db/<kind>/<id>.sexp`
(written whole to a temporary file, then renamed).  Counters (`id`
sequences, per-project issue and merge-request numbers) live in
`<data>/db/counters.sexp`.  At start every file is read; at common-
lisp.net's scale (hundreds of projects, thousands of issues) that is a
second.  There is no database server to run and nothing to migrate: a
record that lacks a field gets the field's default.

The kinds:

- **user** -- username, name, email, password (PBKDF2-SHA256, salted),
  admin?, state, ssh keys (kept for the day an ssh front exists),
  personal access tokens (the token itself hashed; scopes `api`,
  `read_api`, `read_repository`, `write_repository`), created.
- **group** -- a namespace: path, name, description, visibility,
  parent (one level of subgroups), members.
- **project** -- path, namespace (a user's or a group's), name,
  description, visibility (`public`, `internal`, `private`), default
  branch, members, CI variables (masked ones never shown again),
  webhooks, the deploy settings, the mirror settings, created and
  last activity.  Its repository is `<data>/repositories/<namespace>/<path>.git`.
- **issue**, **merge request** -- per project with its own `iid`;
  title, description (Markdown), author, assignee, labels, milestone
  (a string), state, notes (comments, each with author and time), and
  for a merge request the source and target branches, the merge
  status and the merge commit.
- **pipeline**, **job** -- see CI below.
- **deployment** -- a request to the hosting house and what became of
  it (Deployment below).
- **session**, **oauth application**, **oauth grant** -- sign-in and
  the OAuth2 provider.

Permissions are GitLab's access levels (Guest 10, Reporter 20,
Developer 30, Maintainer 40, Owner 50), inherited from the group; a
public project reads for anyone, an internal one for anyone signed in.
Developers push to unprotected branches; the default branch is
protected (Maintainers push and merge).  Admins see everything.

## The web face (Gendl)

The pages are Gendl objects.  One shared root object, `anvil-site`,
holds the chrome (the bar, the footer, the class lists) and the
settings every page reads; it is published at the site's hosts as a
`base-html-page` tree the way cl-site's root is.  The forge's own
addresses are dynamic (`/<namespace>/<project>/-/tree/<ref>/<path>`),
so they are served by prefix handlers that find or make the object
that renders them and ask it for HTML:

- A **project object** (`project-view`) per project, kept in a table
  for the life of the image: its computed slots are the facts that
  cost something and change only on a push -- the branches with their
  tips, the tags, the README rendered, the languages, the default
  branch's tree -- and a push sets `:head-stamp` on it, which empties
  what depends on it (Gendl's dependency tracking, nothing more).
- A **ref view** (`tree-view`, `blob-view`, `commits-view`,
  `commit-view`) is made per request as a child of the project object
  and thrown away: cheap, and what it computes it computes once per
  request even when three parts of the page ask.
- Issues, merge requests, pipelines, settings and the account pages are
  functions of the site object taking the records they show.

The pages are plain HTML with one stylesheet of Anvil's own
(`static/anvil.css`: custom properties, a light and a dark theme, no
build step), and little script: the job trace page polls, the merge
button confirms, the editor is a `<textarea>`.  Markdown is rendered
by 3bmd with code blocks; HTML in Markdown is escaped.  Every page
answers under its GitLab-shaped address, so links that exist today
(`/<group>/<project>/-/blob/master/README.md`, `/-/issues/12`) keep
working when a site moves from GitLab to Anvil.

## The REST API

`/api/v4/`, the subset that common-lisp.net's own programs use, with
GitLab's names and shapes so that they work unchanged:

| | |
| --- | --- |
| `GET /version`, `GET /user` | who is asking |
| `GET /projects?search=&simple=&membership=&visibility=`, `GET /projects/:id` (id or `group%2Fproject`), `POST /projects`, `PUT /projects/:id`, `DELETE /projects/:id` | projects |
| `GET /groups/:id`, `GET /groups/:id/projects?search=` | groups |
| `GET /projects/:id/members/all` | who may do what |
| `GET /projects/:id/repository/tree?ref=&path=&recursive=&per_page=&page=` (with `X-Next-Page`), `GET /repository/blobs/:sha/raw`, `GET /repository/files/:path/raw?ref=`, `GET /repository/branches[/:name]`, `GET /repository/tags`, `GET /repository/commits?ref_name=`, `GET /repository/commits/:sha`, `POST /repository/commits` (branch, start_branch, commit_message, actions), `GET /repository/archive[.tar.gz|.zip]?sha=` | the repository |
| `GET/POST /projects/:id/issues`, `GET/PUT /issues/:iid`, `GET/POST /issues/:iid/notes` | issues |
| `GET/POST /projects/:id/merge_requests?state=&source_branch=`, `GET/PUT /merge_requests/:iid`, `PUT /merge_requests/:iid/merge` | merge requests |
| `GET /projects/:id/pipelines?per_page=`, `POST /projects/:id/pipeline?ref=`, `GET /pipelines/:id`, `GET /pipelines/:id/jobs`, `GET /jobs/:id`, `GET /jobs/:id/trace`, `POST /jobs/:id/retry`, `GET /jobs/:id/artifacts`, `GET /jobs/artifacts/:ref/download?job=`, `GET /jobs/artifacts/:ref/raw/:path?job=` | CI |
| `GET/POST /projects/:id/variables` | CI variables (values shown to Maintainers only) |
| `GET /projects/:id/hooks`, `POST /projects/:id/hooks` | webhooks |

Authentication: `PRIVATE-TOKEN: <personal access token>`,
`Authorization: Bearer <token>` (a personal token or an OAuth token),
the browser's session cookie, or none for what is public.  Errors are
`{"message": "..."}` with GitLab's status codes.  Lists page with
`per_page` and `page` and answer `X-Total`, `X-Page`, `X-Next-Page`.

## OAuth2

Anvil is an OAuth2 provider (authorization code with PKCE, the only
flow GitLab's clients of ours use): `/oauth/authorize`, `/oauth/token`,
and `/api/v4/user` for who the token is.  An administrator registers
an application (name, redirect URI, scopes) and gets its id and
secret.  This is what lets common-lisp.net's git gate -- the Cyclops
rule that pushes a merge request and stages a deployment with the
visitor's own consent -- work against Anvil without a change.

## Git over HTTP

`/<namespace>/<project>.git/info/refs?service=git-upload-pack`,
`/git-upload-pack`, `/git-receive-pack`, and the dumb-protocol files
are answered by the forge's HTTP layer from Ingot's functions.
Reading a public repository needs no credential; anything else takes
HTTP Basic with a username and a password or a personal access token
(the username is ignored for a token, as on GitLab).  A push is
checked ref by ref against the pusher's access level and the protected
branches, then the hooks run: the pipeline for the new commit, the
webhooks, the mirror push, the project's activity stamp.

Request bodies may be gzip-encoded (git does that for large
negotiations); the response is streamed in chunks.

## Continuous integration

Three parts: the specification, the pipeline, the runner.

### The specification

A project may carry either of two files at its root:

- **`ci.sexp`**, Anvil's own: one form, `(:pipeline ...)`, read with
  evaluation off into keywords and checked against a closed list of
  fields.  It has what `.gitlab-ci.yml` has, in Lisp syntax, plus two
  conveniences: a `:lisp` job (an image, and the files to load in it,
  without writing the shell line) and a `:deploy` job (handled by the
  forge itself; Deployment below).

  ```lisp
  (:pipeline
   :stages (:test :deploy)
   :default (:image "genworks/gendl:devo-ccl" :tags ("common-lisp.net"))
   :variables (("GIT_DEPTH" "1"))
   :jobs
   ((:load-and-smoke :stage :test
                     :lisp (:load ("clnet/ci/test.lisp")))
    (:stylesheet :stage :test :image "node:22-slim"
                 :script ("cd clnet/tailwind" "npm ci" "npm run check"))
    (:deploy :stage :deploy
             :rules ((:if (:and (:branch "master") (:variable "CLNET_DEPLOY" "on"))))
             :deploy (:house :common-lisp.app))))
  ```

- **`.gitlab-ci.yml`**, read by Anvil's own YAML reader (`yaml.lisp`:
  block and flow collections, scalars of every style, anchors,
  aliases and merge keys; enough for every CI file we have seen and
  refusing, with a line number, what it does not understand).  The
  keys honored: `stages`, `variables`, `default`, `image` (a string or
  `{name, entrypoint}`), `services` (started beside the job on the
  same network), `before_script`, `script`, `after_script`, `tags`,
  `when`, `allow_failure`, `needs`, `dependencies`, `artifacts`
  (`paths`, `exclude`, `expire_in`, `when`, `name`), `cache` (kept per
  project and key), `rules` (`if` with `==`, `!=`, `=~`, `&&`, `||`
  and `$VAR`; `changes`; `exists`; `when`; `allow_failure`), `only` and
  `except` (branches, tags, `refs`, `variables`), `extends`, `include`
  (`local` only), `resource_group`, `environment` (`name`, `url`,
  recorded), `timeout`, `retry`, `interruptible`, `trigger` and
  `parallel` refused with a message.  `GIT_STRATEGY: none` means no
  tree is handed to the job.

Both compile to the same thing: a list of job plists with their stage,
image, scripts, variables, rules evaluated against the push (branch,
tag, pipeline source, variables) and so on.  `ci.sexp` wins when both
files exist.

### The pipeline

A pipeline is a Gendl object: `pipeline` with a sequence of `stage`s,
each with a sequence of `job`s, built from the compiled specification
and the commit.  Its slots are the questions the pages and the runner
ask -- which jobs may start now (their stage's predecessors done, or
their `needs` done), the pipeline's status from its jobs', whether a
failed job was allowed to fail -- and they recompute when a job's
status is set.  The record on disk is the same data, written through
the model's store after every change, so a restart finds the pipeline
where it was (running jobs are failed with a note, as GitLab does on a
runner loss).

Pipeline sources: `push`, `web` (the Run pipeline button), `api`,
`schedule` (not yet).  Predefined variables as GitLab names them:
`CI`, `CI_PROJECT_DIR`, `CI_PROJECT_ID`, `CI_PROJECT_PATH`,
`CI_PROJECT_NAME`, `CI_PROJECT_URL`, `CI_COMMIT_SHA`,
`CI_COMMIT_SHORT_SHA`, `CI_COMMIT_REF_NAME`, `CI_COMMIT_BRANCH`,
`CI_COMMIT_TAG`, `CI_DEFAULT_BRANCH`, `CI_PIPELINE_ID`,
`CI_PIPELINE_SOURCE`, `CI_JOB_ID`, `CI_JOB_NAME`, `CI_JOB_STAGE`,
`CI_SERVER_URL`, `CI_API_V4_URL`, `CI_REGISTRY` (empty),
`GITLAB_CI` (set, for scripts that test it), `ANVIL_CI`.

### The runner

The runner is in the same image as the forge, a thread of its own with
a queue, and it executes a job in a container through the **Docker
Engine API** over the daemon's Unix socket (`/var/run/docker.sock`
mounted into the forge's container; `DOCKER_HOST=tcp://...` works
too).  No `docker` command, no `git` in the job's image:

1. `POST /containers/create` with the job's image, the command
   `sh -c <script>`, the variables, a working directory, no network
   unless the job asks (`services` get a network of their own), a
   memory limit, and the labels that name the job.
2. The working tree is a tar made from the commit's tree by
   Ingot, uploaded with `PUT /containers/:id/archive` into
   `CI_PROJECT_DIR`; the cache, if any, the same way.
3. `POST /containers/:id/start`, then `GET /containers/:id/logs?follow=1`
   is read as the trace, written to `<data>/ci/traces/<job>.log` as it
   comes (the job page and `/jobs/:id/trace` read the file, so a trace
   is live).
4. `POST /containers/:id/wait` for the status; artifacts are fetched
   with `GET /containers/:id/archive?path=` per declared path and
   written to `<data>/ci/artifacts/<job>.tar`; the container is
   removed.

Images are pulled when missing (`POST /images/create`), with the
registry credentials the forge is given.  A job's `tags` must be among
the runner's (`*runner-tags*`), as on GitLab; a job no runner matches
is `stuck`.  A **shell** executor exists for a deployment that trusts
its projects (`*shell-executor?*`, off by default): the script runs in
a scratch directory with the tree unpacked, through `uiop:run-program`
on the runner's own thread.  Jobs run `*runner-parallelism*` at a
time.  A `:deploy` job runs in no container: the forge does the
deployment itself.

## Deployment: the push-button flow

GitLab's story is environments and deploy jobs: a job with an
`environment:` becomes a deployment of that environment, listed on an
Environments page with its URL.  Anvil keeps that shape (a job with
`environment` is recorded as a deployment of it) and adds the thing
common-lisp.net wanted, which is shorter because the forge already
knows who is who:

- A project page has a **Deploy** button for its Maintainers.  Pressed,
  the forge reads `image.sexp` and `monetize.sexp` from the default
  branch's tree, checks them in process with Monocle (when the
  `monocle` system is loaded; otherwise the house's check does it
  alone), and hands the house's lab a hosting request at its
  `hosting/request` door with the shared secret: `{project_id,
  project, sha, web_url, requester}`.  No OAuth round trip through the
  gate: that dance exists because the lab holds no credential, and
  the forge is the credential.  The request's state is polled from
  the lab's `hosting/state` door and shown on the project's
  **Deployments** page (queued, checking, reviewing, approved, held,
  live, failed), with the application's address once it is up.
- A `:deploy` job in `ci.sexp` (or, in `.gitlab-ci.yml`, a job whose
  `environment.name` is the house, `common-lisp.app`) does the same
  at the end of a green pipeline: push to the default branch, tests
  pass, the house takes it.  That is the streamlined flow: `git
  push` is the button.
- The lab's own Deploy button, through the gate, keeps working:
  Anvil answers the API calls and the OAuth the gate makes.

The settings are a project's: the house (one of `*deploy-houses*`,
each a name, a lab address, a secret file and a domain), and whether
a green pipeline deploys.  The forge-wide ones are the houses.  The
project page also shows, read from the tree, whether the project
meets the profile (the two files there, no faults), so that an author
sees what is missing before pressing anything.

## Webhooks and mirrors

A webhook is an address, a secret token and a list of events (`push`,
`tag_push`, `issue`, `merge_request`, `pipeline`, `job`), posted as
GitLab posts them (`X-Gitlab-Event`, `X-Gitlab-Token`, and the JSON
body's shape for each event) from a thread of the forge's, with the
last delivery's status kept on the hook.  A push mirror is an address
with a credential; after a push the forge pushes every branch and tag
there through Ingot's client side of `receive-pack` (a small
thing once the server side exists), so a project on Anvil can be
live-mirrored to GitHub as projects are from GitLab today.

## What is not here yet

Git over ssh (keys are kept; no ssh front yet).  Wikis, snippets,
Pages, releases beyond tags, LFS, the container registry (the kit
builds that push an image into the project's registry will need one),
subgroups below one level, pipeline schedules, parent-child
pipelines, `include` from other projects, forks with cross-project
merge requests (a fork is a clone and a merge request in the fork, or
a push to a branch), full-text search of code, email notifications,
two-factor sign-in.  Each is a seam rather than a wall; the order is
whatever common-lisp.net needs next.

## Where things are

```
anvil/
  ARCHITECTURE.md   this file
  README.md         running it, configuring it, moving a site from GitLab
  anvil.asd         generated by cl-lite from the .isc files
  ingot/            Ingot, the git library, its own system
    source/         package, octets, zlib, pkt-line, objects, loose,
                    pack, refs, repository, write, diff, merge,
                    packwriter, protocol, archive
    test/test.lisp  its checks, against a real repository
    ingot.asd       written by hand
  source/           package, parameters, util, store, model, auth,
                    permissions, markdown, yaml, ci-spec, ci-model,
                    docker, runner, hooks, git-http, api, oauth,
                    pages-*, deploy, initialize
  static/           anvil.css, the logo, a little script
  ci/test.lisp      the cold load and smoke test
  dev.lisp          load and start in a development image
```
