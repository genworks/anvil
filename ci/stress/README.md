# Stress and correctness suite

Load and correctness checks for a running Anvil, written for the
2026-10-15 field test of anvil-rw.common-lisp.net beside
gitlab.common-lisp.net.  Nothing here is run by the pipeline; the
scripts are for a person (or an agent) with a forge to test and, for
the write side, an `api`-scope personal access token on it.  Python
3 and a `git` binary on the client side; nothing else.

| Script | What it does |
| --- | --- |
| `stress.py` | N concurrent streams over a list of paths for D seconds: status histogram, latency percentiles, throughput, and a SHA-256 of every 200 body per path, so a page that answers differently under load is caught (`--save-variants DIR` keeps each distinct body). |
| `compare.py` | One project on two GitLab-shaped forges, compared: every commit's fields, branches, tags, the recursive tree, every blob by id and by path (and each blob's id against git's own hash of its bytes), the tar.gz archive's members, modes and contents against each other and against `git archive` of a local clone, issues, merge requests, paging headers. |
| `writestress.py` | The write side, phase by phase: `setup` (register, make a token), `projects` (N created at once), `pushes` (N pushes of a repository into N projects at once), `branches` (N branches pushed into ONE project at once), `commits` (T threads committing through the API to one branch), `issues` (T threads opening issues), `mixed` (readers hammering a project while pushes land), `verify` (clone every pushed project back, `fsck --strict`, compare history and tree). |
| `ci-stress.sh` | Pushes a repository that carries a `ci.sexp` to N new projects at once and follows every pipeline to its end, printing the runner's parallelism as it shows. |

## Running it

```console
$ python3 stress.py --base https://forge.example --paths paths.txt --conc 32 --seconds 60
$ python3 compare.py --a https://gitlab.example --a-id 1671 --a-token "$PAT" \
                     --b https://forge.example --b-id 1 --clone /path/to/a/clone
$ python3 writestress.py --base https://forge.example --state state.json --token-file ~/.secrets/forge-token --user me projects --n 8 --conc 8
$ ANVIL_BASE=https://forge.example ANVIL_USER=me ANVIL_TOKEN_FILE=~/.secrets/forge-token SRC=/path/to/anvil ./ci-stress.sh 3
```

`writestress.py` keeps what it made in the state file (the token, the
projects) so the phases build on each other; `setup` is for a forge
with registration open, `--token-file` for one where an administrator
made the token.  Everything it makes is under the user's namespace;
`DELETE /api/v4/projects/<id>` clears it afterwards.  The client
forces IPv4 (the host it was written on has no IPv6 route); drop the
two `getaddrinfo` lines where that is wrong.

The API commits phase expects compare-and-swap losers: a 400
`refs/heads/master is X now, not Y` is correct, a 500 or a stall is
not.  Readers' 404s in `mixed` for `source/` or `README.md` mean the
pushed repository has neither; the check there is for 5xx and stalls.

## What it found, 2026-10-15

Against the devo-ccl image of 2026-10-08, at 8 to 32 concurrent
streams:

- Two threads making the same new directory (a namespace's first
  repository, a loose object's fanout directory) gave 500s on 2 of 24
  project creations and 1 in 40 API commits.  Fixed in Ingot's
  `ensure-directory`.
- A few requests per thousand answered an empty 400, a 500 "Array
  index 6 out of bounds for close", lost their Authorization header,
  or never answered: zacl's `without-scheduling` was a `progn`, so
  AllegroServe's shared header-parse pools were unprotected on this
  port.  Fixed in the Quicklisp fork's zacl; the images carry it since
  the gendl build of 2026-10-15.

After both fixes the same suite against anvil-rw: zero faults in
3,701 reads at 32 streams, 8/8 projects, 8/8 pushes, 8/8 branches,
200 API commits with no stall or 500, 200/200 issues, three pipelines
at once all green (the runner takes two at a time).  Content against
GitLab: identical everywhere; the differences are shape (dates in UTC
where GitLab keeps the committer's offset, no `trailers`, `queued_duration`
null, ~90 GitLab project keys absent).
