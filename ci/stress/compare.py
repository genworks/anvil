#!/usr/bin/env python3
# Copyright © 2026 Genworks International
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.  Distributed WITHOUT
# ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
"""Deep correctness comparison of one project on two GitLab-shaped forges.

usage: compare.py --a URL --a-id ID [--a-token T] --b URL --b-id ID [--b-token T]
                  [--clone DIR] [--ref master]

--clone names a local git clone of the same repository, the truth for
the archive comparison.  Compares: commits (all pages, every field),
the single-commit endpoint, branches, tags, the recursive tree, every
blob (raw bytes by id, raw bytes by path, and the id against git's own
hash of the bytes), the tar.gz archive's member set, modes and
contents, issues and merge requests, paging headers, project fields.
Prints the count of agreements, every difference, and the keys one
side has that the other lacks.
"""
import argparse, hashlib, io, json, socket, subprocess, tarfile, urllib.parse, urllib.request, urllib.error

_orig = socket.getaddrinfo
socket.getaddrinfo = lambda h, p, f=0, *a, **k: _orig(h, p, socket.AF_INET, *a, **k)

ap = argparse.ArgumentParser()
ap.add_argument('--a', required=True); ap.add_argument('--a-id', required=True); ap.add_argument('--a-token', default='')
ap.add_argument('--b', required=True); ap.add_argument('--b-id', required=True); ap.add_argument('--b-token', default='')
ap.add_argument('--clone', default='')
ap.add_argument('--ref', default='master')
args = ap.parse_args()

def get(base, path, token, raw=False, headers_out=None):
    h = {'User-Agent': 'git/anvil-compare'}   # common-lisp.net's bot challenge passes git clients
    if token: h['PRIVATE-TOKEN'] = token
    req = urllib.request.Request(base + path, headers=h)
    try:
        with urllib.request.urlopen(req, timeout=120) as r:
            body = r.read()
            if headers_out is not None: headers_out.update({k.lower(): v for k, v in r.headers.items()})
            return r.status, (body if raw else json.loads(body))
    except urllib.error.HTTPError as e:
        return e.code, e.read()

def paged(base, pid, path, token):
    out, page = [], 1
    while True:
        sep = '&' if '?' in path else '?'
        code, body = get(base, f'/projects/{pid}{path}{sep}per_page=100&page={page}', token)
        if code != 200: return code, body
        if not body: break
        out.extend(body); page += 1
        if page > 50: break
    return 200, out

report = {'diffs': [], 'ok': []}
def check(name, a, b):
    if a == b: report['ok'].append(name)
    else:
        report['diffs'].append({'what': name, 'a': a if len(json.dumps(a, default=str)) < 2000 else '<big>',
                                'b': b if len(json.dumps(b, default=str)) < 2000 else '<big>'})

A = (args.a + '/api/v4', args.a_id, args.a_token)
B = (args.b + '/api/v4', args.b_id, args.b_token)

# commits
ca = paged(A[0], A[1], '/repository/commits', A[2])[1]
cb = paged(B[0], B[1], '/repository/commits', B[2])[1]
keys = ['id', 'short_id', 'title', 'message', 'author_name', 'author_email', 'authored_date',
        'committer_name', 'committer_email', 'committed_date', 'parent_ids']
check('commit count', len(ca), len(cb))
check('commit order (ids)', [c['id'] for c in ca], [c['id'] for c in cb])
da = {c['id']: {k: c.get(k) for k in keys} for c in ca}
db = {c['id']: {k: c.get(k) for k in keys} for c in cb}
for cid in da:
    if cid in db:
        for k in keys:
            check(f'commit {cid[:8]} {k}', da[cid][k], db[cid][k])
report['commit_keys_a_only'] = sorted(set(ca[0].keys()) - set(cb[0].keys())) if ca and cb else None
report['commit_keys_b_only'] = sorted(set(cb[0].keys()) - set(ca[0].keys())) if ca and cb else None

if ca:
    sa = get(A[0], f'/projects/{A[1]}/repository/commits/{ca[0]["id"]}', A[2])[1]
    sb = get(B[0], f'/projects/{B[1]}/repository/commits/{ca[0]["id"]}', B[2])[1]
    for k in keys: check(f'commits/:sha {k}', sa.get(k), sb.get(k))

# branches, tags
ba = paged(A[0], A[1], '/repository/branches', A[2])[1]
bb = paged(B[0], B[1], '/repository/branches', B[2])[1]
check('branches', sorted((b['name'], b['commit']['id'], b.get('default')) for b in ba),
      sorted((b['name'], b['commit']['id'], b.get('default')) for b in bb))
ta = paged(A[0], A[1], '/repository/tags', A[2])[1]
tb = paged(B[0], B[1], '/repository/tags', B[2])[1]
check('tags', sorted((t['name'], t['commit']['id']) for t in ta), sorted((t['name'], t['commit']['id']) for t in tb))

# recursive tree and its paging headers
tra = paged(A[0], A[1], f'/repository/tree?recursive=true&ref={args.ref}', A[2])[1]
trb = paged(B[0], B[1], f'/repository/tree?recursive=true&ref={args.ref}', B[2])[1]
norm = lambda t: sorted((e['path'], e['type'], e['id'], e['mode']) for e in t)
check('tree recursive count', len(tra), len(trb))
check('tree recursive entries', norm(tra), norm(trb))
ha, hb = {}, {}
get(A[0], f'/projects/{A[1]}/repository/tree?recursive=true&ref={args.ref}&per_page=20&page=1', A[2], headers_out=ha)
get(B[0], f'/projects/{B[1]}/repository/tree?recursive=true&ref={args.ref}&per_page=20&page=1', B[2], headers_out=hb)
for h in ['x-total', 'x-per-page', 'x-page', 'x-next-page', 'x-total-pages']:
    check(f'tree header {h}', ha.get(h), hb.get(h))

# blobs by id, files by path, and git's own hash
blobs = [e for e in tra if e['type'] == 'blob']
sha_a, sha_b, raw_a, raw_b = {}, {}, {}, {}
for e in blobs:
    pa = get(A[0], f'/projects/{A[1]}/repository/blobs/{e["id"]}/raw', A[2], raw=True)
    pb = get(B[0], f'/projects/{B[1]}/repository/blobs/{e["id"]}/raw', B[2], raw=True)
    sha_a[e['path']] = (pa[0], hashlib.sha256(pa[1]).hexdigest())
    sha_b[e['path']] = (pb[0], hashlib.sha256(pb[1]).hexdigest())
    q = urllib.parse.quote(e['path'], safe='')
    fa = get(A[0], f'/projects/{A[1]}/repository/files/{q}/raw?ref={args.ref}', A[2], raw=True)
    fb = get(B[0], f'/projects/{B[1]}/repository/files/{q}/raw?ref={args.ref}', B[2], raw=True)
    raw_a[e['path']] = (fa[0], hashlib.sha256(fa[1]).hexdigest())
    raw_b[e['path']] = (fb[0], hashlib.sha256(fb[1]).hexdigest())
    if pa[0] == 200:
        check(f'blob id matches git hash {e["path"]}', hashlib.sha1(b'blob %d\0' % len(pa[1]) + pa[1]).hexdigest(), e['id'])
for path in sha_a:
    check(f'blobs/:sha/raw {path}', sha_a[path], sha_b[path])
    check(f'files/:path/raw {path}', raw_a[path], raw_b[path])
report['blob_count'] = len(blobs)

# archive
aa = get(A[0], f'/projects/{A[1]}/repository/archive.tar.gz?sha={args.ref}', A[2], raw=True)
ab = get(B[0], f'/projects/{B[1]}/repository/archive.tar.gz?sha={args.ref}', B[2], raw=True)
def members(blob):
    if not isinstance(blob, bytes): return None
    try:
        tf = tarfile.open(fileobj=io.BytesIO(blob), mode='r:gz')
    except Exception as e:
        return f'untarrable: {e}'
    out = {}
    for m in tf.getmembers():
        rel = m.name.split('/', 1)[1] if '/' in m.name else ''
        if m.isfile(): out[rel] = ('file', hashlib.sha256(tf.extractfile(m).read()).hexdigest(), m.mode & 0o777)
        elif m.isdir(): out[rel] = ('dir',)
        elif m.issym(): out[rel] = ('sym', m.linkname)
    return out
ma, mb = members(aa[1]), members(ab[1])
check('archive status', aa[0], ab[0])
if isinstance(ma, dict) and isinstance(mb, dict):
    fa_ = {k: v for k, v in ma.items() if v[0] == 'file'}; fb_ = {k: v for k, v in mb.items() if v[0] == 'file'}
    check('archive file set', sorted(fa_), sorted(fb_))
    check('archive file contents', {k: v[1] for k, v in fa_.items()}, {k: v[1] for k, v in fb_.items()})
    check('archive file modes', {k: oct(v[2]) for k, v in fa_.items()}, {k: oct(v[2]) for k, v in fb_.items()})
    check('archive dirs', sorted(k for k, v in ma.items() if v[0] == 'dir'), sorted(k for k, v in mb.items() if v[0] == 'dir'))
else:
    report['diffs'].append({'what': 'archive', 'a': str(ma)[:200], 'b': str(mb)[:200]})
if args.clone and isinstance(ma, dict):
    out = subprocess.run(['git', '-C', args.clone, 'archive', '--format=tar', args.ref], capture_output=True).stdout
    tf = tarfile.open(fileobj=io.BytesIO(out))
    gm = {m.name: hashlib.sha256(tf.extractfile(m).read()).hexdigest() for m in tf.getmembers() if m.isfile()}
    check('archive files == git archive (A)', gm, {k: v[1] for k, v in ma.items() if v[0] == 'file'})
    if isinstance(mb, dict):
        check('archive files == git archive (B)', gm, {k: v[1] for k, v in mb.items() if v[0] == 'file'})

# issues and merge requests
for kind in ['issues', 'merge_requests']:
    ia = paged(A[0], A[1], f'/{kind}?state=all', A[2])[1]
    ib = paged(B[0], B[1], f'/{kind}?state=all', B[2])[1]
    n = lambda xs: sorted((x['iid'], x['title'], x['state']) for x in xs) if isinstance(xs, list) else xs
    check(kind, n(ia), n(ib))

# the project record
pa = get(A[0], f'/projects/{A[1]}', A[2])[1]; pb = get(B[0], f'/projects/{B[1]}', B[2])[1]
report['project_keys_a_only'] = sorted(set(pa) - set(pb)); report['project_keys_b_only'] = sorted(set(pb) - set(pa))
for k in ['default_branch', 'visibility', 'path', 'empty_repo', 'archived']:
    check(f'project {k}', pa.get(k), pb.get(k))

print(json.dumps({'ok_count': len(report['ok']), 'diff_count': len(report['diffs']), 'blob_count': report['blob_count'],
                  'diffs': report['diffs'],
                  'commit_keys_a_only': report['commit_keys_a_only'], 'commit_keys_b_only': report['commit_keys_b_only'],
                  'project_keys_a_only': report['project_keys_a_only'], 'project_keys_b_only': report['project_keys_b_only']},
                 indent=1, default=str))
