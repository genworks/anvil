# Anvil

A software forge in Common Lisp and Gendl: repositories served over
Git's smart HTTP protocol, a web face for browsing them, issues, merge
requests, continuous integration that runs a project's `.gitlab-ci.yml`
or a native s-expression pipeline (`ci.sexp`), a REST API in GitLab's
shapes, an OAuth2 provider, and a push-button deployment of a project
into a house that hosts applications.  Built first for
[common-lisp.net](https://common-lisp.net), whose services lean on
exactly these features of GitLab, so that what talks to GitLab there
keeps working when pointed at Anvil.

Git itself is **[Ingot](ingot/)**, a Git implementation in plain Common
Lisp that lives in this repository as a system of its own: loose and
packed objects (deltas included), refs, trees, commits, diffs,
three-way merges, the smart protocol's server side, archives.  No
subprocess and no `git` binary anywhere.

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

`initialize!` publishes the forge on an existing AllegroServe server
(for a Gendl image that serves other things too, scoped by `*hosts*`);
`start!` starts a server of Anvil's own on a port.

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
repository trees, blobs, files, branches, tags, commits (reading and
making), archives, issues and notes, merge requests (reading, making,
merging), pipelines, jobs, traces, artifacts (by job, or the latest of
a ref), CI variables, webhooks.  See ARCHITECTURE.md for the list.

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
issues and merge requests not here yet.  `:into "a-group"` puts single
projects under one group here; `:auth '("oauth2" . "<token>")` fetches
repositories that are not public.  The fetch calls itself
`git/anvil-import`, because common-lisp.net's bot challenge passes git
clients by their user agent and challenges anything else.

## Checks

Ingot's own: load `ingot/test/test.lisp` into an image that has the
system loaded (`INGOT_REPO=/path/to/some/.git` makes it read a real
repository too).  Anvil's cold load and smoke test is `ci/test.lisp`,
run by the pipeline inside a Gendl image that has never seen the
checkout.

## License

GNU Affero General Public License, version 3 or later; see `LICENSE`.
