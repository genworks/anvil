# Anvil

A software forge in Common Lisp and Gendl: repositories served over
Git's smart HTTP protocol, a web face for browsing them (trees, files,
blame, a file finder, search, commits with their patches and feeds,
releases, the badges a README shows), issues, merge
requests, continuous integration that runs a project's `.gitlab-ci.yml`
or a native s-expression pipeline (`ci.sexp`), a REST API in GitLab's
shapes, an OAuth2 provider, and a push-button deployment of a project
into a house that hosts applications.  Built first for
[common-lisp.net](https://common-lisp.net), whose services lean on
exactly these features of GitLab, so that what talks to GitLab there
keeps working when pointed at Anvil.

Git itself is **[Ingot](https://gitlab.common-lisp.net/clo/ingot)**,
a Git implementation in plain Common Lisp and a repository of its own,
which Anvil takes as a dependency: loose and packed objects (deltas
included), refs, trees, commits, diffs, three-way merges, the smart
protocol on both sides, archives.  No subprocess and no `git` binary
anywhere.  Anvil finds it in `INGOT_DIR`, else in a checkout beside
this one (`../ingot/`), else in `ingot/` inside it, where the pipeline
unpacks it.

[ARCHITECTURE.md](ARCHITECTURE.md) is the design.

## Running it

In a Gendl image (the `genworks/gendl:devo-ccl` or `devo-sbcl` images,
or any image with Gendl loaded) that can see this checkout:

```lisp
(load "/path/to/anvil/dev.lisp")
```

Anvil answers at `http://localhost:9095/`, with its state under
`ANVIL_DATA_DIR` (else `/tmp/anvil-dev/`).  Register the first account
at `/users/sign_up`: the first account is an administrator.  Make a
project, then:

```console
$ git clone http://localhost:9095/you/project.git
$ git push http://you:<token>@localhost:9095/you/project.git master
```

A personal access token (`/-/profile/tokens`) stands in for the
password with git and with the API (`PRIVATE-TOKEN: <token>`).

### Configuring a site

Everything a site sets is a special variable of the `anvil` package,
set before `initialize!` or `start!`:

| Variable | What |
| --- | --- |
| `*site-name*`, `*site-url*` | The forge's name and public address (clone addresses and API links are made from it). |
| `*hosts*` | Host names to answer to, or nil for all. |
| `*data-directory*` | Where everything lives: `db/` (records, one s-expression file each), `repositories/`, `ci/`. |
| `*registration-open?*` | Whether anyone may register. |
| `*docker-socket*` | The Docker daemon's Unix socket (mount it into the container), or `tcp://host:port`; nil switches the executor off. |
| `*runner-tags*`, `*runner-parallelism*` | What jobs this image's runner takes, and how many at once. |
| `*deploy-houses*` | The houses a project may be deployed to: `(:key :common-lisp.app :name "..." :lab-url "https://lab.common-lisp.app/prompt-lab" :secret-file #p"..." :domain "common-lisp.app")`. |
| `*push-mirrors?*` | Whether a project's push mirrors are pushed to; nil for a forge that must write to no other (a test instance beside the GitLab it imports from). |
| `*pull-mirror-interval*`, `*pull-mirror-token*` | Seconds between rounds of the pull-mirror thread (nil: none), and the read_api token it reads a GitLab's API with (or a file holding it; by default `gitlab-token` in the data directory).  Set from `ANVIL_PULL_MIRROR_SECONDS` by the start-up files. |
| `*read-only?*` | A forge that only shows; see below.  Set from `ANVIL_READ_ONLY`. |
| `*robots-txt*` | What `/robots.txt` answers: by default project pages, issues and merge requests yes, the per-commit history, archives and the API no. |

`initialize!` publishes the forge on an existing AllegroServe server
(for a Gendl image that serves other things too, scoped by `*hosts*`);
`start!` starts a server of Anvil's own on a port.

### Read-only

With the environment variable `ANVIL_READ_ONLY` set to anything
non-empty (`ANVIL_READ_ONLY=1`), the forge only shows.  Browsing works
as before, clones and fetches included.  Every change over HTTP is
refused with a 403, whichever page or program asks: signing up or in,
tokens and keys, new projects, groups, issues and merge requests,
comments, edits, settings, the API's writing verbs, OAuth2 (no token is
issued) and pushes (git prints the refusal).  Refusals come back as a
page, as JSON for the API and OAuth, and as plain text for git.  The
pages keep every control that would change something but grey it out,
with a title saying the forge is read-only for now.

`dev.lisp`, `ci/runner-smoke.lisp` and the lab rig's
`ci/lab-e2e/anvil-init.lisp` read the variable into `*read-only?*`,
and `initialize!` sets it as well whenever the variable is there, so
a start-up file of your own can't leave such a forge writable.
Functions called at the REPL still write.  That is how a read-only forge
gets loaded: `make-user!`, `import-from-gitlab!`.  It is kept current by
the pull mirrors (below): with `*pull-mirror-interval*` set, a thread
follows the GitLab it was filled from.

## Continuous integration

A project with a `ci.sexp` or a `.gitlab-ci.yml` at its root runs a
pipeline on every push.  Both compile to the same thing; `ci.sexp` is
read first.  Jobs run in containers through the Docker Engine API,
their working tree handed in as a tar made from the commit (no `git`
in the job image), their output followed into a trace, their artifacts
fetched out.  A job tagged `shell` may run on the image's own host when
`*shell-executor?*` is on.

```lisp
(:pipeline
 :stages (:test :deploy)
 :default (:image "genworks/gendl:devo-ccl")
 :variables (("GIT_DEPTH" "1"))
 :jobs
 ((:load-and-smoke :stage :test :lisp (:load ("ci/test.lisp")))
  (:stylesheet :stage :test :image "node:22-slim"
               :script ("cd tailwind" "npm ci" "npm run check"))
  (:deploy :stage :deploy
           :rules ((:if (:and (:default-branch) (:variable "DEPLOY" "on"))))
           :deploy (:house :common-lisp.app))))
```

## The API

`/api/v4/`, in GitLab's names and shapes: projects, groups, members,
repository trees, blobs, files and their blame, branches, tags, commits
(reading and making), archives, languages, releases, search, issues and
notes, merge requests (reading, making, merging), pipelines, jobs,
traces, artifacts (by job, or the latest of a ref), CI variables,
webhooks.  See ARCHITECTURE.md for the list.

## Deployment

A Maintainer of a public project that meets the hosting house's
profile (an `image.sexp` and a `monetize.sexp` at its root; checked in
process when Monocle is loaded) presses **Deploy**, and the forge hands
the house's lab a hosting request with the shared secret; the
Deployments page follows the request to its address.  A `:deploy` job
does the same at the end of a green pipeline, and a project may ask to
be deployed on every green pipeline of its default branch.

## Importing from GitLab

A group with its projects, or single projects, can be brought over
from a GitLab: the repositories through Ingot's fetch (every branch
and tag, HEAD too), the issues and merge requests with their notes
through GitLab's API, their numbers kept so that a link which worked
there works here.

```lisp
(anvil:import-from-gitlab! "https://gitlab.common-lisp.net"
                           :token "<a personal access token with read_api>"  ; public things need none
                           :groups '("clo" ("alexandria" "alexandria"))      ; a group whole, or some of its projects
                           :projects '("someone/a-project"))                   ; single projects, into the namespace of that path
```

People are not imported.  What has no author here is authored by the
importing user (the first administrator, unless `:importer` names
another), and its text begins with a line saying who wrote it there
and when; a user of the same username here is matched instead.
Importing again brings a repository up to date and adds only the
issues and merge requests not here yet.

### Pull mirrors

Every imported project is a **pull mirror** of where it came from,
GitLab's way: `mirror` and `import_url` in its API record, a *Pull
mirror* card in its settings, `POST /api/v4/projects/:id/mirror/pull`
to pull now.  `pull-mirror!` fetches the repository (every branch and
tag, HEAD too) and brings over the issues and merge requests not here
yet; `pull-mirrors!` does that for every mirrored project and then
looks at every imported group for projects new there.  With
`*pull-mirror-interval*` set (the kits set it from
`ANVIL_PULL_MIRROR_SECONDS`; GitLab's own cadence is 30 minutes) a
thread runs `pull-mirrors!` on that cadence, so a read-only forge
filled from a GitLab follows it without anyone at the REPL.  A project
that was not imported can be given a pull mirror too (its settings, or
`PUT /projects/:id` with `mirror=true&import_url=...`), with a user and
token for a repository that is not public.  `:into "a-group"` puts single
projects under one group here; `:auth '("oauth2" . "<token>")` fetches
repositories that are not public.

On a read-only forge, which serves the public, the importer brings only
what the public sees on the GitLab, whatever the token can see: a
project that is private or internal there is left out (its report says
`:skipped`), and so are a public project's issues or merge requests when
they are open to members only.  `:public-only? nil` brings everything.
A project's issues or merge requests that fail to come are reported
(`:issues-error`, `:merge-requests-error`) and the import goes on with
the rest.  The fetch calls itself
`git/anvil-import`, because common-lisp.net's bot challenge passes git
clients by their user agent and challenges anything else.

## Checks

Ingot's own, in its repository: `test/test.lisp` into an image that
has the system loaded (`INGOT_REPO=/path/to/some/.git` makes it read a
real repository too), or its `ci/test.lisp` cold.  Anvil's cold load
and smoke test is `ci/test.lisp`,
run by the pipeline inside a Gendl image that has never seen the
checkout.

## License

GNU Affero General Public License, version 3 or later; see `LICENSE`.
