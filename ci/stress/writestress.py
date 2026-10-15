#!/usr/bin/env python3
# Copyright © 2026 Genworks International
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.  Distributed WITHOUT
# ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
"""Write-side stress for an Anvil forge, phase by phase.  The state file
carries the token and what the phases made, so they build on each other.

  writestress.py --base URL --state state.json [--user NAME] [--token-file F] PHASE ...

  setup                                  register (registration must be open) and make a token
  projects --n 24 --conc 8               projects created at once
  pushes --src DIR --n 8                 the repository at DIR pushed into N projects at once
  branches --src DIR --n 8               N branches pushed into ONE project at once
  commits --threads 8 --each 5           threads committing through the API to one branch
  issues --threads 8 --each 25           threads opening issues
  mixed --src DIR --readers 8 --seconds 30   readers hammer a project while a pusher pushes
  verify --src DIR [--work DIR]          clone every pushed project back, fsck --strict, compare

--project NAME picks the project a phase works on (default: the first made).
A --token-file with an existing api-scope token stands in for setup on a
forge whose registration is closed.
"""
import argparse, base64, json, os, re, socket, subprocess, sys, tempfile, threading, time, urllib.parse, urllib.request, urllib.error
from collections import Counter
from http.cookiejar import CookieJar

_orig_gai = socket.getaddrinfo   # force IPv4; drop where the host has an IPv6 route
socket.getaddrinfo = lambda h, p, f=0, *a, **k: _orig_gai(h, p, socket.AF_INET, *a, **k)

ap = argparse.ArgumentParser()
ap.add_argument('--base', required=True); ap.add_argument('--state', required=True)
ap.add_argument('phase')
ap.add_argument('--n', type=int, default=8); ap.add_argument('--conc', type=int, default=8)
ap.add_argument('--threads', type=int, default=8); ap.add_argument('--each', type=int, default=5)
ap.add_argument('--src', default=''); ap.add_argument('--readers', type=int, default=8)
ap.add_argument('--seconds', type=float, default=30); ap.add_argument('--user', default='stress')
ap.add_argument('--project', default=''); ap.add_argument('--token-file', default='')
ap.add_argument('--work', default='', help='verify: where the clones go (default: a temporary directory)')
args = ap.parse_args()
B = args.base.rstrip('/')
state = json.load(open(args.state)) if os.path.exists(args.state) else {}
if args.token_file:
    state['token'] = open(args.token_file).read().strip(); state['user'] = args.user
if 'user' in state: args.user = state['user']
def save(): json.dump(state, open(args.state, 'w'), indent=1)

def req(method, path, data=None, headers=None, token=None, raw=False, timeout=30):
    h = {'User-Agent': 'anvil-writestress/0.1'}
    if token: h['PRIVATE-TOKEN'] = token
    if headers: h.update(headers)
    body = None
    if data is not None:
        if isinstance(data, dict):
            body = json.dumps(data).encode(); h['Content-Type'] = 'application/json'
        else:
            body = data
    r = urllib.request.Request(B + path, data=body, method=method, headers=h)
    t0 = time.time()
    try:
        with urllib.request.urlopen(r, timeout=timeout) as resp:
            out = resp.read(); code = resp.status; hdrs = dict(resp.headers)
    except urllib.error.HTTPError as e:
        out = e.read(); code = e.code; hdrs = dict(e.headers)
    except Exception as e:  # a stalled or dropped connection: counted as status 0, not hung on
        out = f'{type(e).__name__}: {e}'.encode(); code = 0; hdrs = {}
    dt = time.time() - t0
    if raw: return code, out, hdrs, dt
    try: return code, json.loads(out), hdrs, dt
    except Exception: return code, out[:300], hdrs, dt

def git(*a, cwd=None):
    p = subprocess.run(['git', *a], cwd=cwd, capture_output=True, text=True)
    return p.returncode, p.stdout + p.stderr

def auth_header():
    tok = base64.b64encode(f'{args.user}:{state["token"]}'.encode()).decode()
    return f'Authorization: Basic {tok}'

def pct(xs, q):
    xs = sorted(xs); return round(xs[min(len(xs) - 1, int(q * len(xs)))] * 1000) if xs else None

def pick_project():
    return {'name': args.project} if args.project else state['projects'][0]

def pid_of(p): return urllib.parse.quote(f'{args.user}/{p["name"]}', safe='')

def outcome(code, body, ok_key):
    if isinstance(body, dict): return body.get(ok_key) if code == 201 else body.get('message')
    return str(body)[:120]

def run_all(threads):
    for t in threads: t.start()
    for t in threads: t.join()

# ---------------------------------------------------------------- setup
if args.phase == 'setup':
    form = urllib.parse.urlencode({'username': args.user, 'name': 'Stress Tester', 'email': f'{args.user}@example.invalid',
                                   'password': 'stress-password-1'}).encode()
    opener = urllib.request.build_opener(urllib.request.HTTPCookieProcessor(CookieJar()))
    r = urllib.request.Request(B + '/users/sign_up', data=form, method='POST',
                               headers={'Content-Type': 'application/x-www-form-urlencoded', 'User-Agent': 'anvil-writestress/0.1'})
    try:
        resp = opener.open(r, timeout=120); print('sign_up ->', resp.status, resp.url)
    except urllib.error.HTTPError as e:
        print('sign_up ->', e.code, e.read()[:300]); sys.exit(1)
    form = urllib.parse.urlencode([('name', 'stress'), ('scopes', 'api'), ('expires', '')]).encode()
    r = urllib.request.Request(B + '/-/profile/tokens', data=form, method='POST',
                               headers={'Content-Type': 'application/x-www-form-urlencoded', 'User-Agent': 'anvil-writestress/0.1'})
    page = opener.open(r, timeout=120).read().decode()
    m = re.search(r'<pre>(anvil-[A-Za-z0-9_-]+)</pre>', page)
    if not m: print('no token in page', page[:500]); sys.exit(1)
    state['token'] = m.group(1); state['user'] = args.user; save()
    code, me, _, _ = req('GET', '/api/v4/user', token=state['token'])
    print('token ok, /api/v4/user ->', code, me.get('username') if isinstance(me, dict) else me)

# ---------------------------------------------------------------- projects
elif args.phase == 'projects':
    results = []; lock = threading.Lock(); names = [f'p{int(time.time()) % 100000}-{i}' for i in range(args.n)]
    def mk(name):
        code, body, _, dt = req('POST', '/api/v4/projects', {'name': name, 'path': name, 'visibility': 'public',
                                                            'initialize_with_readme': False}, token=state['token'])
        with lock: results.append((name, code, outcome(code, body, 'id'), dt))
    ts = [threading.Thread(target=mk, args=(n,)) for n in names]
    t0 = time.time()
    for i in range(0, len(ts), args.conc):
        run_all(ts[i:i + args.conc])
    el = time.time() - t0
    codes = Counter(c for _, c, _, _ in results); ids = [i for _, c, i, _ in results if c == 201]
    print(json.dumps({'created': len(ids), 'codes': dict(codes), 'unique_ids': len(set(ids)) == len(ids),
                      'elapsed_s': round(el, 2), 'p50_ms': pct([d for *_, d in results], .5), 'max_ms': pct([d for *_, d in results], 1.0),
                      'failures': [(n, c, b) for n, c, b, _ in results if c != 201][:5]}, indent=1, default=str))
    state.setdefault('projects', []).extend([{'name': n, 'id': i} for n, c, i, _ in results if c == 201]); save()

# ---------------------------------------------------------------- pushes
elif args.phase == 'pushes':
    targets = state['projects'][:args.n]; out = []; lock = threading.Lock()
    def push(p):
        url = f'{B}/{args.user}/{p["name"]}.git'
        t0 = time.time(); rc, log = git('-c', f'http.extraHeader={auth_header()}', 'push', '-q', url, '+HEAD:refs/heads/master', cwd=args.src)
        with lock: out.append((p['name'], rc, round(time.time() - t0, 2), log.strip()[-200:]))
    t0 = time.time()
    run_all([threading.Thread(target=push, args=(p,)) for p in targets])
    print(json.dumps({'pushed': sum(1 for _, rc, _, _ in out if rc == 0), 'of': len(out), 'wall_s': round(time.time() - t0, 2),
                      'each_s': [(n, s) for n, rc, s, _ in out], 'failures': [(n, l) for n, rc, _, l in out if rc]}, indent=1))
    state['pushed'] = [n for n, rc, _, _ in out if rc == 0]; save()

# ---------------------------------------------------------------- branches
elif args.phase == 'branches':
    p = pick_project()
    url = f'{B}/{args.user}/{p["name"]}.git'; out = []; lock = threading.Lock()
    head = git('rev-parse', 'HEAD', cwd=args.src)[1].strip()
    def push(i):
        t0 = time.time(); rc, log = git('-c', f'http.extraHeader={auth_header()}', 'push', '-q', url, f'+HEAD:refs/heads/stress-{i}', cwd=args.src)
        with lock: out.append((i, rc, round(time.time() - t0, 2), log.strip()[-200:]))
    run_all([threading.Thread(target=push, args=(i,)) for i in range(args.n)])
    rc, lsr = git('ls-remote', url)
    refs = dict(line.split('\t')[::-1] for line in lsr.strip().splitlines() if '\t' in line)
    good = [i for i in range(args.n) if refs.get(f'refs/heads/stress-{i}') == head]
    print(json.dumps({'project': p['name'], 'pushed_ok': sum(1 for _, rc, _, _ in out if rc == 0), 'of': args.n,
                      'refs_at_head': len(good), 'each_s': [(i, s) for i, rc, s, _ in out],
                      'failures': [(i, l) for i, rc, _, l in out if rc]}, indent=1))

# ---------------------------------------------------------------- commits
elif args.phase == 'commits':
    p = pick_project(); pid = pid_of(p); out = []; lock = threading.Lock()
    def worker(t):
        for k in range(args.each):
            code, body, _, dt = req('POST', f'/api/v4/projects/{pid}/repository/commits',
                                    {'branch': 'master', 'commit_message': f'stress t{t} k{k}',
                                     'actions': [{'action': 'create', 'file_path': f'stress/t{t}-k{k}.txt', 'content': f'{t} {k}\n'}]},
                                    token=state['token'])
            with lock: out.append((t, k, code, outcome(code, body, 'id'), dt))
    t0 = time.time()
    run_all([threading.Thread(target=worker, args=(t,)) for t in range(args.threads)])
    el = time.time() - t0
    ok = [o for o in out if o[2] == 201]
    code, commits, _, _ = req('GET', f'/api/v4/projects/{pid}/repository/commits?ref_name=master&per_page=100', token=state['token'])
    ids = [c['id'] for c in commits] if isinstance(commits, list) else []
    code2, tree, _, _ = req('GET', f'/api/v4/projects/{pid}/repository/tree?path=stress&per_page=100', token=state['token'])
    files = sorted(e['path'] for e in tree) if isinstance(tree, list) else tree
    print(json.dumps({'attempted': len(out), 'created_201': len(ok), 'codes': dict(Counter(o[2] for o in out)), 'wall_s': round(el, 2),
                      'p50_ms': pct([o[4] for o in out], .5), 'max_ms': pct([o[4] for o in out], 1.0),
                      'all_successful_on_master': all(o[3] in ids for o in ok), 'master_commits': len(ids), 'unique': len(set(ids)) == len(ids),
                      'files_in_tree': len(files) if isinstance(files, list) else files,
                      'files_match_successes': isinstance(files, list) and sorted(files) == sorted(f'stress/t{o[0]}-k{o[1]}.txt' for o in ok),
                      'failures_sample': [o[3] for o in out if o[2] != 201][:3]}, indent=1, default=str))

# ---------------------------------------------------------------- issues
elif args.phase == 'issues':
    p = pick_project(); pid = pid_of(p); out = []; lock = threading.Lock()
    def worker(t):
        for k in range(args.each):
            code, body, _, dt = req('POST', f'/api/v4/projects/{pid}/issues', {'title': f'stress issue t{t} k{k}', 'description': 'x'}, token=state['token'])
            with lock: out.append((code, outcome(code, body, 'iid'), dt))
    t0 = time.time()
    run_all([threading.Thread(target=worker, args=(t,)) for t in range(args.threads)])
    iids = [i for c, i, _ in out if c == 201]
    total, page = [], 1
    while True:
        code, listed, _, _ = req('GET', f'/api/v4/projects/{pid}/issues?state=all&per_page=100&page={page}', token=state['token'])
        if not (isinstance(listed, list) and listed): break
        total.extend(listed); page += 1
    print(json.dumps({'attempted': len(out), 'created': len(iids), 'codes': dict(Counter(c for c, _, _ in out)), 'wall_s': round(time.time() - t0, 2),
                      'p50_ms': pct([d for *_, d in out], .5), 'max_ms': pct([d for *_, d in out], 1.0),
                      'iids_unique': len(set(iids)) == len(iids), 'iids_dense': sorted(iids) == list(range(min(iids), max(iids) + 1)) if iids else None,
                      'listed_total': len(total), 'listed_unique_iids': len({i['iid'] for i in total}),
                      'failures_sample': [i for c, i, _ in out if c != 201][:5]}, indent=1, default=str))

# ---------------------------------------------------------------- mixed
elif args.phase == 'mixed':
    p = pick_project(); pid = pid_of(p)
    url = f'{B}/{args.user}/{p["name"]}.git'
    # the pages read: the project, its history, its API, and one directory and one file of its tree
    paths = [f'/{args.user}/{p["name"]}', f'/{args.user}/{p["name"]}/-/commits/master',
             f'/api/v4/projects/{pid}/repository/tree', f'/api/v4/projects/{pid}/repository/commits', '/explore']
    code, tree, _, _ = req('GET', f'/api/v4/projects/{pid}/repository/tree?per_page=100')
    if isinstance(tree, list):
        d = next((e['path'] for e in tree if e['type'] == 'tree'), None)
        f = next((e['path'] for e in tree if e['type'] == 'blob'), None)
        if d: paths.append(f'/{args.user}/{p["name"]}/-/tree/master/{d}')
        if f: paths.append(f'/{args.user}/{p["name"]}/-/blob/master/{f}')
    stop = time.time() + args.seconds; codes = Counter(); lat = []; lock = threading.Lock(); bad = []
    def reader(i):
        k = i
        while time.time() < stop:
            pth = paths[k % len(paths)]; k += 1
            code, body, _, dt = req('GET', pth, raw=True, timeout=120)
            with lock:
                codes[code] += 1; lat.append(dt)
                if (code >= 500 or code == 0) and len(bad) < 5: bad.append((pth, code, body[:200].decode('utf-8', 'replace')))
    pushes = []
    def pusher():
        i = 0
        while time.time() < stop:
            t0 = time.time(); rc, log = git('-c', f'http.extraHeader={auth_header()}', 'push', '-q', url, f'+HEAD:refs/heads/mixed-{i}', cwd=args.src)
            pushes.append((rc, round(time.time() - t0, 2))); i += 1
    run_all([threading.Thread(target=reader, args=(i,)) for i in range(args.readers)] + [threading.Thread(target=pusher)])
    print(json.dumps({'reads': len(lat), 'codes': dict(codes), 'p50_ms': pct(lat, .5), 'p95_ms': pct(lat, .95), 'max_ms': pct(lat, 1.0),
                      'pushes': len(pushes), 'push_failures': sum(1 for rc, _ in pushes if rc), 'push_s': [s for _, s in pushes][:10],
                      'bad_sample': bad}, indent=1))

# ---------------------------------------------------------------- verify
elif args.phase == 'verify':
    src_commits = set(git('rev-list', 'HEAD', cwd=args.src)[1].split())
    tree_src = git('rev-parse', 'HEAD^{tree}', cwd=args.src)[1].strip()
    work = args.work or tempfile.mkdtemp(prefix='anvil-verify-')
    res = []
    for name in state.get('pushed', []):
        d = os.path.join(work, f'verify-{name}')
        t0 = time.time(); rc, log = git('clone', '-q', f'{B}/{args.user}/{name}.git', d); ct = round(time.time() - t0, 2)
        if rc: res.append((name, 'clone failed', log[-200:])); continue
        frc, flog = git('fsck', '--strict', cwd=d)
        got = set(git('rev-list', 'master', cwd=d)[1].split())
        tree_got = git('rev-parse', 'master^{tree}', cwd=d)[1].strip()
        res.append((name, ct, 'fsck ok' if frc == 0 else flog[-200:], got == src_commits, tree_src == tree_got))
    print(json.dumps({'clones_in': work, 'results': res}, indent=1))
else:
    print('unknown phase'); sys.exit(2)
