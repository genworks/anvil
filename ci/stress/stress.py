#!/usr/bin/env python3
# Copyright © 2026 Genworks International
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.  Distributed WITHOUT
# ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
"""Bounded HTTP load: N workers hit a list of paths round-robin for D
seconds (or K requests), each request on a fresh connection; reports the
status histogram, latency percentiles, throughput, every non-200 with
the start of its body, and whether any path's 200 body changed between
fetches (a correctness check under load).

usage: stress.py --base URL --paths FILE --conc 8 --seconds 30
       [--header 'K: V'] [--label NAME] [--max-requests K] [--timeout S]
       [--json OUT] [--save-variants DIR]
"""
import argparse, hashlib, json, socket, threading, time, urllib.request, urllib.error
from collections import Counter, defaultdict

ap = argparse.ArgumentParser()
ap.add_argument('--base', required=True)
ap.add_argument('--paths', required=True, help='file with one path per line; # comments')
ap.add_argument('--conc', type=int, default=8)
ap.add_argument('--seconds', type=float, default=30)
ap.add_argument('--max-requests', type=int, default=0)
ap.add_argument('--header', action='append', default=[])
ap.add_argument('--label', default='')
ap.add_argument('--timeout', type=float, default=60)
ap.add_argument('--json', default='')
ap.add_argument('--save-variants', default='', help='directory: save each distinct 200 body of a path')
args = ap.parse_args()

# force IPv4 (written on a host with no IPv6 route; drop these two lines where that is wrong)
_orig = socket.getaddrinfo
socket.getaddrinfo = lambda h, p, f=0, *a, **k: _orig(h, p, socket.AF_INET, *a, **k)

paths = [l.strip() for l in open(args.paths) if l.strip() and not l.startswith('#')]
headers = {'User-Agent': 'anvil-stress/0.1'}
for h in args.header:
    k, v = h.split(':', 1); headers[k.strip()] = v.strip()

lock = threading.Lock()
lat = []                       # (path, seconds)
status = Counter()
errors = Counter()
digests = defaultdict(set)     # path -> set of sha256 for 200 bodies
count = 0
stop = time.time() + args.seconds
idx = 0

def worker():
    global idx, count
    while True:
        with lock:
            if time.time() >= stop or (args.max_requests and count >= args.max_requests):
                return
            p = paths[idx % len(paths)]; idx += 1; count += 1
        req = urllib.request.Request(args.base + p, headers=headers)
        t0 = time.time()
        try:
            with urllib.request.urlopen(req, timeout=args.timeout) as r:
                body = r.read(); code = r.status
        except urllib.error.HTTPError as e:
            body = e.read(); code = e.code
        except Exception as e:
            dt = time.time() - t0
            with lock:
                errors[type(e).__name__ + ':' + str(e)[:60]] += 1; lat.append((p, dt))
            continue
        dt = time.time() - t0
        with lock:
            status[code] += 1; lat.append((p, dt))
            if code != 200:
                errors[f'HTTP {code} {p} :: {body[:160]!r}'] += 1
            else:
                h = hashlib.sha256(body).hexdigest()
                if h not in digests[p] and args.save_variants:
                    open(f'{args.save_variants}/variant-{len(digests[p])}-{h[:8]}.html', 'wb').write(body)
                digests[p].add(h)

t_start = time.time()
threads = [threading.Thread(target=worker, daemon=True) for _ in range(args.conc)]
for t in threads: t.start()
for t in threads: t.join()
elapsed = time.time() - t_start

def pct(xs, q):
    if not xs: return float('nan')
    xs = sorted(xs); return xs[min(len(xs) - 1, int(q * len(xs)))]

all_l = [d for _, d in lat]
per = defaultdict(list)
for p, d in lat: per[p].append(d)
summary = {
    'label': args.label, 'base': args.base, 'conc': args.conc, 'elapsed_s': round(elapsed, 2),
    'requests': len(lat), 'rps': round(len(lat) / elapsed, 2) if elapsed else None,
    'status': dict(status), 'errors': dict(errors),
    'p50_ms': round(pct(all_l, .5) * 1000), 'p95_ms': round(pct(all_l, .95) * 1000),
    'p99_ms': round(pct(all_l, .99) * 1000), 'max_ms': round(max(all_l) * 1000) if all_l else None,
    'inconsistent_bodies': {p: len(s) for p, s in digests.items() if len(s) > 1},
    'per_path_p50_ms': {p: round(pct(v, .5) * 1000) for p, v in per.items()},
}
print(json.dumps(summary, indent=1))
if args.json:
    json.dump(summary, open(args.json, 'w'), indent=1)
