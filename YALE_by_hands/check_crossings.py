#!/usr/bin/env python3
"""Detect wires that pass through agent circles in the rendered SVGs.

Parses each SVG in ``processed/``: agent circles (r >= 10) and wires
(polylines + cubic Beziers).  A wire is flagged if any sampled point lies
strictly inside an agent circle (min distance to center < r - 1.0).  Wires
that merely *end* at a port on the circle are fine unless they arrive
obliquely and cut into the interior — the near-endpoint samples catch that.

Usage:
    python3 check_crossings.py              # check all figures
    python3 check_crossings.py rule_11      # check selected figures

Exit status: 0 if no crossings, 1 otherwise.
"""
import glob, math, os, re, sys

PROC = os.path.join(os.path.dirname(os.path.abspath(__file__)), "processed")

def parse_circles(svg):
    out = []
    for m in re.finditer(r'<circle cx="([\d.]+)" cy="([\d.]+)" r="([\d.]+)"', svg):
        x, y, r = (float(m.group(i)) for i in (1, 2, 3))
        if r >= 10:
            out.append((x, y, r))
    return out

def parse_wires(svg):
    wires = []
    for m in re.finditer(r'<polyline points="([^"]+)"', svg):
        pts = [tuple(map(float, p.split(","))) for p in m.group(1).split()]
        wires.append(("poly", pts))
    for m in re.finditer(
        r'<path d="M ([\d.]+),([\d.]+) C ([\d.]+),([\d.]+) ([\d.]+),([\d.]+) ([\d.]+),([\d.]+)"', svg):
        g = m.groups()
        wires.append(("bez", tuple(tuple(map(float, g[i:i+2])) for i in range(0, 8, 2))))
    return wires

def sample_poly(pts, n=60):
    out = []
    for i in range(len(pts) - 1):
        x1, y1 = pts[i]; x2, y2 = pts[i + 1]
        for j in range(n):
            t = j / n
            out.append((x1 + (x2 - x1) * t, y1 + (y2 - y1) * t))
    out.append(pts[-1])
    return out

def sample_bez(p0, c1, c2, p3, n=120):
    out = []
    for i in range(n + 1):
        t = i / n; u = 1 - t
        x = u**3*p0[0] + 3*u*u*t*c1[0] + 3*u*t*t*c2[0] + t**3*p3[0]
        y = u**3*p0[1] + 3*u*u*t*c1[1] + 3*u*t*t*c2[1] + t**3*p3[1]
        out.append((x, y))
    return out

def main():
    files = sorted(glob.glob(os.path.join(PROC, "*.svg")))
    if len(sys.argv) > 1:
        files = [os.path.join(PROC, a) if not a.endswith(".svg") else a for a in sys.argv[1:]]
    bad = 0
    for f in files:
        svg = open(f).read()
        circles = parse_circles(svg)
        wires = parse_wires(svg)
        issues = []
        for wi, w in enumerate(wires):
            pts = sample_poly(w[1]) if w[0] == "poly" else sample_bez(*w[1])
            for (cx, cy, r) in circles:
                mind = min(math.hypot(x - cx, y - cy) for x, y in pts)
                if mind < r - 1.0:
                    issues.append((wi, (cx, cy, r), round(mind, 1)))
        name = os.path.basename(f)
        if issues:
            bad += 1
            print(f"✗ {name}")
            for wi, (cx, cy, r), mind in issues:
                print(f"    wire #{wi} enters circle ({cx:g},{cy:g}) r={r:g}  (min dist {mind})")
        else:
            print(f"✓ {name}")
    sys.exit(1 if bad else 0)

if __name__ == "__main__":
    main()
