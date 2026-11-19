#!/usr/bin/env python3
"""Render YALE rules as clean SVG+PNG diagrams.

Notation (from ``legend`` + ``reduction rule spec``):
  * Agent  – circle with a name.  Secondary (aux) ports are numbered
             *inside* the circle, near the edge.  The primary port is a
             filled triangle sitting on the circle edge.
  * Subnet – dashed rounded-rectangle boundary enclosing a net.  Interface
             ports (I₁, I₂, …) are dots *on the boundary*; wires run from an
             agent port (inside) out to the boundary dot.
  * Active pair – the two primary-port triangles of the meeting agents,
             joined by a wire.
  * Rule   –  LHS subnet  ⇒  RHS subnet  (same interface ports).

Usage:
    python3 render.py              # render all defined rules
    python3 render.py rule_01      # render one rule
    python3 render.py --no-png     # skip PNG (fast iteration)
"""

import math, os, re, subprocess, sys

# ── Layout constants ──────────────────────────────────────────────────
W, H = 900, 470
AR = 32                 # agent radius
TRI_H, TRI_W = 12, 14   # primary-port triangle height / base width
FS, TS, SW, DR = 16, 18, 2, 5
OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "processed")
FF = 'font-family="DejaVu Sans,sans-serif"'

# Standard subnet rectangles (x, y, w, h)
LHS = (40, 55, 350, 370)
RHS = (510, 55, 350, 370)

# ── Low-level helpers ─────────────────────────────────────────────────

def _f(v):
    r = round(v, 1)
    return str(int(r)) if r == int(r) else str(r)

def _p(cx, cy, r, deg):
    """Polar → Cartesian.  0°=right, 90°=down (SVG y-axis)."""
    a = math.radians(deg)
    return (cx + r * math.cos(a), cy + r * math.sin(a))

def _e(s):
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

# ── Primitives ────────────────────────────────────────────────────────

def subnet(x, y, w, h, r=26):
    """Dashed rounded-rectangle subnet boundary."""
    return (f'<rect x="{_f(x)}" y="{_f(y)}" width="{_f(w)}" height="{_f(h)}" '
            f'rx="{r}" ry="{r}" fill="none" stroke="#000" stroke-width="1.5" '
            f'stroke-dasharray="9 7"/>')

def p_tri(x, y, angle):
    """Primary-port triangle on the circle edge at (x,y) r=AR, pointing
    outward at ``angle``.  Returns (svg, apex)."""
    bc = _p(x, y, AR, angle)
    apex = _p(x, y, AR + TRI_H, angle)
    rad = math.radians(angle)
    px, py = -math.sin(rad), math.cos(rad)
    b1 = (bc[0] + px * TRI_W / 2, bc[1] + py * TRI_W / 2)
    b2 = (bc[0] - px * TRI_W / 2, bc[1] - py * TRI_W / 2)
    svg = (f'<polygon points="{_f(b1[0])},{_f(b1[1])} '
           f'{_f(b2[0])},{_f(b2[1])} {_f(apex[0])},{_f(apex[1])}" fill="#000"/>')
    return svg, apex

def agent(x, y, name, ports, pa):
    """Agent circle.

    ports : {port_number: angle_deg}  – secondary ports; the number is drawn
            *inside* the circle near the edge, the wire exits at the edge.
    pa    : angle_deg of the primary port (triangle).

    Returns (svg, pos) where pos[port] = (x, y): the triangle apex for "P",
    the circle-edge exit point for numbered ports.
    """
    o, pos = [], {}
    o.append(f'<circle cx="{_f(x)}" cy="{_f(y)}" r="{AR}" fill="#fff" stroke="#000" stroke-width="{SW}"/>')
    o.append(f'<text x="{_f(x)}" y="{_f(y+6)}" text-anchor="middle" {FF} font-size="{FS+4}" fill="#000">{_e(name)}</text>')
    svg, apex = p_tri(x, y, pa)
    o.append(svg)
    pos["P"] = apex
    for n, a in sorted(ports.items()):
        nx, ny = _p(x, y, AR - 13, a)
        o.append(f'<text x="{_f(nx)}" y="{_f(ny+5)}" text-anchor="middle" {FF} font-size="{FS}" fill="#000">{n}</text>')
        pos[n] = _p(x, y, AR, a)
    return "\n".join(o), pos

def wire(pts):
    """Wire: polyline through the given (x,y) points."""
    s = " ".join(f"{_f(x)},{_f(y)}" for x, y in pts)
    return f'<polyline points="{s}" fill="none" stroke="#000" stroke-width="{SW}"/>'

def iface(x, y, label, dx=0, dy=0, anchor="middle"):
    """Interface dot on the subnet boundary + label."""
    return (f'<circle cx="{_f(x)}" cy="{_f(y)}" r="{DR}" fill="#000"/>\n'
            f'<text x="{_f(x+dx)}" y="{_f(y+dy)}" text-anchor="{anchor}" {FF} font-size="{FS}" fill="#000">{_e(label)}</text>')

def title(t):
    w = max(len(t) * 11 + 24, 80)
    return (f'<rect x="10" y="10" width="{w}" height="32" fill="#fff" stroke="#000" stroke-width="1.5"/>\n'
            f'<text x="{10+w//2}" y="32" text-anchor="middle" {FF} font-size="{TS}" fill="#000">{_e(t)}</text>')

def imply():
    return f'<text x="{W//2}" y="{H//2+16}" text-anchor="middle" {FF} font-size="48" fill="#000">⇒</text>'

def wrap(t, body, w=W, h=H):
    title_svg = title(t) if t else ""
    return (f'<?xml version="1.0" encoding="UTF-8"?>\n'
            f'<svg xmlns="http://www.w3.org/2000/svg" width="{w}" height="{h}" viewBox="0 0 {w} {h}">\n'
            f'<rect width="{w}" height="{h}" fill="#fff"/>\n'
            f'{title_svg}\n{body}\n</svg>\n')

# ── Rule definitions ──────────────────────────────────────────────────

def rule_01():
    """Rule 1:  @ ⋈ λ  ⇒  d + ∩-wire"""
    p = []
    # ── LHS ──
    p.append(subnet(*LHS))
    a1, q1 = agent(215, 150, "@", {1: 180, 2: 0}, 90)
    a2, q2 = agent(215, 320, "λ", {1: 180, 2: 0, 3: 30}, 270)
    p += [a1, a2, wire([q1["P"], q2["P"]])]
    p += [wire([q1[1], (40, 150)]),  iface(40, 150, "I₁", dx=-10, dy=5, anchor="end")]
    p += [wire([q1[2], (390, 150)]), iface(390, 150, "I₂", dx=10, dy=5, anchor="start")]
    p += [wire([q2[1], (40, 320)]),  iface(40, 320, "I₃", dx=-10, dy=5, anchor="end")]
    p += [wire([q2[2], (390, 320)]), iface(390, 320, "I₄", dx=10, dy=5, anchor="start")]
    p += [wire([q2[3], (390, 352)]), iface(390, 352, "I₅", dx=10, dy=5, anchor="start")]
    # ── RHS ──
    p.append(subnet(*RHS))
    p += [wire([(600, 55), (700, 55)]),    # top bar
          wire([(600, 55), (600, 425)]),   # left vertical
          wire([(700, 55), (700, 425)])]   # right vertical
    a3, q3 = agent(780, 320, "d", {1: 0}, 270)
    p.append(a3)
    p.append(wire([q3["P"], (780, 200), (700, 200)]))   # d.P → right vertical
    p.append(wire([q3[1], (860, 320)]))                  # d.1 → I₅
    p += [iface(600, 55, "I₁", dy=-12), iface(700, 55, "I₂", dy=-12),
          iface(600, 425, "I₃", dy=24), iface(700, 425, "I₄", dy=24),
          iface(860, 320, "I₅", dx=10, dy=5, anchor="start")]
    p.append(imply())
    return wrap("Rule 1", "\n".join(p))

def rule_02():
    """Rule 2:  d ⋈ ν  ⇒  ∅  (annihilation, no interface)"""
    p = []
    p.append(subnet(*LHS))
    a1, q1 = agent(150, 240, "d", {}, 0)
    a2, q2 = agent(280, 240, "ν", {}, 180)
    p += [a1, a2, wire([q1["P"], q2["P"]])]
    p.append(imply())
    return wrap("Rule 2", "\n".join(p))

def rule_03():
    """Rule 3:  b ⋈ κ  ⇒  κ + wire(I₂—I₃)"""
    p = []
    # LHS
    p.append(subnet(*LHS))
    a1, q1 = agent(215, 150, "b", {1: 270, 2: 180, 3: 0}, 90)
    a2, q2 = agent(215, 320, "κ", {}, 270)
    p += [a1, a2, wire([q1["P"], q2["P"]])]
    p += [wire([q1[1], (215, 55)]),  iface(215, 55, "I₁", dy=-12)]
    p += [wire([q1[2], (40, 150)]),   iface(40, 150, "I₂", dx=-10, dy=5, anchor="end")]
    p += [wire([q1[3], (390, 150)]),  iface(390, 150, "I₃", dx=10, dy=5, anchor="start")]
    # RHS
    p.append(subnet(*RHS))
    a3, q3 = agent(685, 140, "κ", {}, 90)
    p.append(a3)
    p.append(wire([q3["P"], (685, 425)]))
    p.append(iface(685, 425, "I₁", dy=24))
    p.append(wire([(510, 300), (860, 300)]))
    p += [iface(510, 300, "I₂", dx=-10, dy=5, anchor="end"),
          iface(860, 300, "I₃", dx=10, dy=5, anchor="start")]
    p.append(imply())
    return wrap("Rule 3", "\n".join(p))

def rule_04():
    """Rule 4:  b ⋈ λ  ⇒  λ + b  (with internal wire λ.1↔b.2)"""
    p = []
    # ── LHS ──
    p.append(subnet(*LHS))
    a1, q1 = agent(215, 140, "b", {1: 180, 2: 270, 3: 0}, 90)
    a2, q2 = agent(215, 330, "λ", {1: 150, 2: 90, 3: 30}, 270)
    p += [a1, a2, wire([q1["P"], q2["P"]])]
    p += [wire([q1[2], (215, 55)]),  iface(215, 55, "I₂", dy=-12)]
    p += [wire([q1[1], (40, 140)]),   iface(40, 140, "I₃", dx=-10, dy=5, anchor="end")]
    p += [wire([q1[3], (390, 140)]),  iface(390, 140, "I₁", dx=10, dy=5, anchor="start")]
    p += [wire([q2[1], (120, 425)]),  iface(120, 425, "I₄", dy=24)]
    p += [wire([q2[2], (215, 425)]),  iface(215, 425, "I₅", dy=24)]
    p += [wire([q2[3], (310, 425)]),  iface(310, 425, "I₆", dy=24)]
    # ── RHS ──
    p.append(subnet(*RHS))
    a3, q3 = agent(685, 140, "λ", {1: 180, 2: 90, 3: 0}, 270)
    a4, q4 = agent(685, 330, "b", {1: 180, 2: 270, 3: 0}, 90)
    p += [a3, a4]
    p.append(wire([q3[1], q4[2]]))                      # internal λ.1 ↔ b.2
    p += [wire([q3["P"], (685, 55)]),  iface(685, 55, "I₂", dy=-12)]
    p += [wire([q3[3], (860, 140)]),   iface(860, 140, "I₆", dx=10, dy=5, anchor="start")]
    p += [wire([q3[2], (780, 425)]),   iface(780, 425, "I₁", dy=24)]
    p += [wire([q4[1], (510, 330)]),   iface(510, 330, "I₃", dx=-10, dy=5, anchor="end")]
    p += [wire([q4[3], (860, 330)]),   iface(860, 330, "I₅", dx=10, dy=5, anchor="start")]
    p += [wire([q4["P"], (685, 425)]), iface(685, 425, "I₄", dy=24)]
    p.append(imply())
    return wrap("Rule 4", "\n".join(p))

def rule_05():
    """Rule 5:  b ⋈ ν  ⇒  ν + wire(I₃—I₁)"""
    p = []
    # LHS
    p.append(subnet(*LHS))
    a1, q1 = agent(215, 150, "b", {1: 180, 2: 270, 3: 0}, 90)
    a2, q2 = agent(215, 320, "ν", {}, 270)
    p += [a1, a2, wire([q1["P"], q2["P"]])]
    p += [wire([q1[2], (215, 55)]),  iface(215, 55, "I₂", dy=-12)]
    p += [wire([q1[1], (40, 150)]),   iface(40, 150, "I₃", dx=-10, dy=5, anchor="end")]
    p += [wire([q1[3], (390, 150)]),  iface(390, 150, "I₁", dx=10, dy=5, anchor="start")]
    # RHS
    p.append(subnet(*RHS))
    a3, q3 = agent(685, 140, "ν", {}, 270)
    p.append(a3)
    p += [wire([q3["P"], (685, 55)]), iface(685, 55, "I₂", dy=-12)]
    p.append(wire([(510, 300), (860, 300)]))
    p += [iface(510, 300, "I₃", dx=-10, dy=5, anchor="end"),
          iface(860, 300, "I₁", dx=10, dy=5, anchor="start")]
    p.append(imply())
    return wrap("Rule 5", "\n".join(p))

def rule_06():
    """Rule 6:  c ⋈ κ  ⇒  κ + κ  (duplication)"""
    p = []
    # LHS
    p.append(subnet(*LHS))
    a1, q1 = agent(150, 240, "c", {1: 225, 2: 135}, 0)
    a2, q2 = agent(280, 240, "κ", {}, 180)
    p += [a1, a2, wire([q1["P"], q2["P"]])]
    p += [wire([q1[1], (40, 150)]),  iface(40, 150, "I₁", dx=-10, dy=5, anchor="end")]
    p += [wire([q1[2], (40, 330)]),  iface(40, 330, "I₂", dx=-10, dy=5, anchor="end")]
    # RHS: two separate κ agents
    p.append(subnet(*RHS))
    a3, q3 = agent(700, 150, "κ", {}, 180)
    a4, q4 = agent(700, 330, "κ", {}, 180)
    p += [a3, a4]
    p += [wire([q3["P"], (510, 150)]), iface(510, 150, "I₁", dx=-10, dy=5, anchor="end")]
    p += [wire([q4["P"], (510, 330)]), iface(510, 330, "I₂", dx=-10, dy=5, anchor="end")]
    p.append(imply())
    return wrap("Rule 6", "\n".join(p))

def rule_07():
    """Rule 7:  c ⋈ λ  ⇒  λ + λ + c + δ + δ⁻¹"""
    p = []
    # ── LHS ──
    p.append(subnet(*LHS))
    a1, q1 = agent(215, 140, "c", {1: 330, 2: 210}, 90)
    a2, q2 = agent(215, 330, "λ", {1: 150, 2: 90, 3: 30}, 270)
    p += [a1, a2, wire([q1["P"], q2["P"]])]
    p += [wire([q1[2], (120, 55)]),  iface(120, 55, "I₂", dy=-12)]
    p += [wire([q1[1], (310, 55)]),  iface(310, 55, "I₁", dy=-12)]
    p += [wire([q2[1], (40, 300)]),   iface(40, 300, "I₃", dx=-10, dy=5, anchor="end")]
    p += [wire([q2[2], (215, 425)]),  iface(215, 425, "I₄", dy=24)]
    p += [wire([q2[3], (390, 300)]),  iface(390, 300, "I₅", dx=10, dy=5, anchor="start")]
    # ── RHS ──
    p.append(subnet(*RHS))
    stl, qt = agent(620, 140, "λ", {1: 180, 2: 90, 3: 0}, 270)
    str_, qr = agent(780, 140, "λ", {1: 180, 2: 90, 3: 0}, 270)
    sc, qc = agent(600, 320, "c", {1: 0, 2: 180}, 90)
    sd, qd = agent(700, 320, "δ", {1: 270, 2: 180}, 90)
    si, qi = agent(800, 320, "δ⁻¹", {1: 270, 2: 180}, 90)
    p += [stl, str_, sc, sd, si]
    # P ports → interface
    p += [wire([qt["P"], (620, 55)]),  iface(620, 55, "I₂", dy=-12)]
    p += [wire([qr["P"], (780, 55)]),  iface(780, 55, "I₁", dy=-12)]
    p += [wire([qc["P"], (600, 425)]), iface(600, 425, "I₃", dy=24)]
    p += [wire([qd["P"], (700, 425)]), iface(700, 425, "I₄", dy=24)]
    p += [wire([qi["P"], (800, 425)]), iface(800, 425, "I₅", dy=24)]
    # internal wires
    p += [wire([qt[1], qc[2]]), wire([qt[2], qd[1]]), wire([qt[3], qi[1]]),
          wire([qr[1], qc[1]]), wire([qr[2], qd[2]]), wire([qr[3], qi[2]])]
    p.append(imply())
    return wrap("Rule 7", "\n".join(p))

def rule_08():
    """Rule 8:  c ⋈ ν  ⇒  ν + ν  (duplication)"""
    p = []
    # LHS
    p.append(subnet(*LHS))
    a1, q1 = agent(150, 240, "c", {1: 225, 2: 135}, 0)
    a2, q2 = agent(280, 240, "ν", {}, 180)
    p += [a1, a2, wire([q1["P"], q2["P"]])]
    p += [wire([q1[1], (40, 150)]),  iface(40, 150, "I₁", dx=-10, dy=5, anchor="end")]
    p += [wire([q1[2], (40, 330)]),  iface(40, 330, "I₂", dx=-10, dy=5, anchor="end")]
    # RHS: two separate ν agents
    p.append(subnet(*RHS))
    a3, q3 = agent(700, 150, "ν", {}, 180)
    a4, q4 = agent(700, 330, "ν", {}, 180)
    p += [a3, a4]
    p += [wire([q3["P"], (510, 150)]), iface(510, 150, "I₁", dx=-10, dy=5, anchor="end")]
    p += [wire([q4["P"], (510, 330)]), iface(510, 330, "I₂", dx=-10, dy=5, anchor="end")]
    p.append(imply())
    return wrap("Rule 8", "\n".join(p))

def rule_09():
    """Rule 9:  δ⁻¹ ⋈ δ  ⇒  crossing wires (I₂↔I₄, I₁↔I₃)"""
    p = []
    # LHS
    p.append(subnet(*LHS))
    a1, q1 = agent(215, 140, "δ⁻¹", {1: 330, 2: 210}, 90)
    a2, q2 = agent(215, 330, "δ", {1: 150, 2: 30}, 270)
    p += [a1, a2, wire([q1["P"], q2["P"]])]
    p += [wire([q1[1], (310, 55)]),  iface(310, 55, "I₁", dy=-12)]
    p += [wire([q1[2], (120, 55)]),  iface(120, 55, "I₂", dy=-12)]
    p += [wire([q2[1], (120, 425)]), iface(120, 425, "I₃", dy=24)]
    p += [wire([q2[2], (310, 425)]), iface(310, 425, "I₄", dy=24)]
    # RHS: two crossing wires
    p.append(subnet(*RHS))
    p.append(wire([(600, 55), (780, 425)]))   # I₂ → I₄
    p.append(wire([(780, 55), (600, 425)]))   # I₁ → I₃
    p += [iface(600, 55, "I₂", dy=-12), iface(780, 55, "I₁", dy=-12),
          iface(600, 425, "I₃", dy=24), iface(780, 425, "I₄", dy=24)]
    p.append(imply())
    return wrap("Rule 9", "\n".join(p))

def rule_10():
    """Rule 10:  b ⋈ δ  ⇒  b′ + δ  (b ports shift, b→b′)"""
    p = []
    # LHS
    p.append(subnet(*LHS))
    a1, q1 = agent(215, 140, "b", {1: 180, 2: 270, 3: 0}, 90)
    a2, q2 = agent(215, 330, "δ", {1: 150, 2: 30}, 270)
    p += [a1, a2, wire([q1["P"], q2["P"]])]
    p += [wire([q1[2], (215, 55)]),  iface(215, 55, "I₂", dy=-12)]
    p += [wire([q1[1], (40, 140)]),   iface(40, 140, "I₃", dx=-10, dy=5, anchor="end")]
    p += [wire([q1[3], (390, 140)]),  iface(390, 140, "I₁", dx=10, dy=5, anchor="start")]
    p += [wire([q2[1], (120, 425)]),  iface(120, 425, "I₄", dy=24)]
    p += [wire([q2[2], (310, 425)]),  iface(310, 425, "I₅", dy=24)]
    # RHS
    p.append(subnet(*RHS))
    a3, q3 = agent(685, 140, "b′", {1: 270, 2: 0, 3: 90}, 180)
    a4, q4 = agent(685, 330, "δ", {1: 150, 2: 30}, 270)
    p += [a3, a4, wire([q3[3], q4["P"]])]   # b′.3 ↔ δ.P
    p += [wire([q3["P"], (510, 140)]), iface(510, 140, "I₃", dx=-10, dy=5, anchor="end")]
    p += [wire([q3[1], (685, 55)]),   iface(685, 55, "I₂", dy=-12)]
    p += [wire([q3[2], (860, 140)]),  iface(860, 140, "I₁", dx=10, dy=5, anchor="start")]
    p += [wire([q4[1], (600, 425)]),  iface(600, 425, "I₄", dy=24)]
    p += [wire([q4[2], (780, 425)]),  iface(780, 425, "I₅", dy=24)]
    p.append(imply())
    return wrap("Rule 10", "\n".join(p))

def rule_11():
    """Rule 11:  δ ⋈ b  ⇒  b + b + δ + δ + δ′"""
    p = []
    # ── LHS ──
    p.append(subnet(*LHS))
    a1, q1 = agent(150, 240, "δ", {1: 225, 2: 135}, 0)
    a2, q2 = agent(280, 240, "b", {1: 270, 2: 0, 3: 90}, 180)
    p += [a1, a2, wire([q1["P"], q2["P"]])]
    p += [wire([q1[1], (40, 160)]),  iface(40, 160, "I₁", dx=-10, dy=5, anchor="end")]
    p += [wire([q1[2], (40, 320)]),  iface(40, 320, "I₂", dx=-10, dy=5, anchor="end")]
    p += [wire([q2[1], (280, 55)]),  iface(280, 55, "I₅", dy=-12)]
    p += [wire([q2[2], (390, 240)]), iface(390, 240, "I₄", dx=10, dy=5, anchor="start")]
    p += [wire([q2[3], (280, 425)]), iface(280, 425, "I₃", dy=24)]
    # ── RHS ──
    p.append(subnet(*RHS))
    sbt, qbt = agent(610, 140, "b", {1: 180, 2: 315, 3: 45}, 90)
    sbb, qbb = agent(610, 290, "b", {1: 180, 2: 315, 3: 45}, 90)
    sdt, qdt = agent(790, 100, "δ", {1: 225, 2: 135}, 270)
    sdm, qdm = agent(800, 220, "δ", {1: 225, 2: 135}, 0)
    sdp, qdp = agent(750, 350, "δ′", {1: 315, 2: 225}, 90)
    p += [sbt, sbb, sdt, sdm, sdp]
    # interface wires
    p += [wire([qbt[1], (510, 140)]), iface(510, 140, "I₁", dx=-10, dy=5, anchor="end")]
    p += [wire([qbb[1], (510, 290)]), iface(510, 290, "I₂", dx=-10, dy=5, anchor="end")]
    p += [wire([qdt["P"], (790, 55)]), iface(790, 55, "I₅", dy=-12)]
    p += [wire([qdm["P"], (860, 220)]), iface(860, 220, "I₄", dx=10, dy=5, anchor="start")]
    p += [wire([qdp["P"], (750, 425)]), iface(750, 425, "I₃", dy=24)]
    # internal wires
    p += [wire([qbt[2], qdt[1]]),   # b_top.2 ↔ δ_top.1
          wire([qbt[3], qdm[1]]),   # b_top.3 ↔ δ_mid.1
          wire([qbt["P"], qdm[2]]), # b_top.P ↔ δ_mid.2
          wire([qbb[2], qdt[2]]),   # b_bot.2 ↔ δ_top.2 (long diagonal)
          wire([qbb[3], qdp[1]]),   # b_bot.3 ↔ δ′.1
          wire([qbb["P"], qdp[2]])] # b_bot.P ↔ δ′.2
    p.append(imply())
    return wrap("Rule 11", "\n".join(p))

def dots(x, y, s="·····"):
    """Ellipsis (generalization) marker."""
    return f'<text x="{_f(x)}" y="{_f(y)}" text-anchor="middle" {FF} font-size="22" fill="#000">{_e(s)}</text>'

def label(x, y, s, size=FS, anchor="start"):
    """Plain text label."""
    return f'<text x="{_f(x)}" y="{_f(y)}" text-anchor="{anchor}" {FF} font-size="{size}" fill="#000">{_e(s)}</text>'

def arrow(x1, y1, x2, y2):
    """Line from (x1,y1) to (x2,y2) with a filled arrowhead at (x2,y2)."""
    ang = math.atan2(y2 - y1, x2 - x1)
    ah = 9
    a1 = ang + math.radians(155)
    a2 = ang - math.radians(155)
    p1 = (x2 + ah * math.cos(a1), y2 + ah * math.sin(a1))
    p2 = (x2 + ah * math.cos(a2), y2 + ah * math.sin(a2))
    return (f'<line x1="{_f(x1)}" y1="{_f(y1)}" x2="{_f(x2)}" y2="{_f(y2)}" '
            f'stroke="#000" stroke-width="1.5"/>'
            f'<polygon points="{_f(x2)},{_f(y2)} {_f(p1[0])},{_f(p1[1])} '
            f'{_f(p2[0])},{_f(p2[1])}" fill="#000"/>')

def dot(x, y):
    """Interface dot only (no label)."""
    return f'<circle cx="{_f(x)}" cy="{_f(y)}" r="{DR}" fill="#000"/>'

def ilabel(x, y, sub, dx=0, dy=0, anchor="middle"):
    """Label 'I' with a subscript string (e.g. '1', 'k', 'k+1', 'm')."""
    return (f'<text x="{_f(x+dx)}" y="{_f(y+dy)}" text-anchor="{anchor}" {FF} fill="#000">'
            f'<tspan font-size="{FS}">I</tspan>'
            f'<tspan font-size="{FS-6}" dy="4">{_e(sub)}</tspan></text>')

def reduction_rule_spec():
    """General reduction rule:  (x₁ ⋈ x₂) subnet  ⇒  subnet  (same interface)."""
    p = []
    # ── LHS ──
    p.append(subnet(*LHS))
    a1, q1 = agent(215, 150, "x₁", {}, 90)
    a2, q2 = agent(215, 320, "x₂", {}, 270)
    p += [a1, a2, wire([q1["P"], q2["P"]])]
    p += [wire([_p(215, 150, AR, 225), (120, 55)]),  dot(120, 55),  ilabel(120, 55, "k", dy=-12)]
    p += [wire([_p(215, 150, AR, 315), (310, 55)]),  dot(310, 55),  ilabel(310, 55, "1", dy=-12)]
    p += [wire([_p(215, 320, AR, 135), (120, 425)]), dot(120, 425), ilabel(120, 425, "k+1", dy=24)]
    p += [wire([_p(215, 320, AR, 45),  (310, 425)]), dot(310, 425), ilabel(310, 425, "m", dy=24)]
    p.append(dots(215, 82, "···"))
    p.append(dots(215, 404, "···"))
    # ── RHS ──
    p.append(subnet(*RHS))
    p.append(f'<rect x="635" y="185" width="150" height="100" rx="24" ry="24" '
             f'fill="#fff" stroke="#000" stroke-width="{SW}"/>')
    p.append(label(710, 242, "subnet", anchor="middle"))
    p += [wire([(635, 185), (600, 55)]),  dot(600, 55),  ilabel(600, 55, "k", dy=-12)]
    p += [wire([(785, 185), (820, 55)]),  dot(820, 55),  ilabel(820, 55, "1", dy=-12)]
    p += [wire([(635, 285), (600, 425)]), dot(600, 425), ilabel(600, 425, "k+1", dy=24)]
    p += [wire([(785, 285), (820, 425)]), dot(820, 425), ilabel(820, 425, "m", dy=24)]
    p.append(dots(710, 82, "···"))
    p.append(dots(710, 404, "···"))
    p.append(imply())
    p.append(label(W // 2, 488, "Reduction rule", size=24, anchor="middle"))
    return wrap(None, "\n".join(p), h=500)

def legend():
    """Notation legend: agent (primary + aux ports) and subnet (interface)."""
    p = []
    # ── Agent ──
    a, q = agent(160, 120, "a", {1: 180, 2: 90, 3: 0}, 270)
    p.append(a)
    p += [wire([q[1], (95, 120)]),
          wire([q[2], (160, 195)]),
          wire([q[3], (225, 120)])]
    p.append(label(258, 80, "primary port"))
    p.append(arrow(253, 84, q["P"][0] + 8, q["P"][1] + 2))
    p.append(label(258, 168, "secondary (aux) port with member"))
    p.append(arrow(253, 162, q[3][0] + 5, q[3][1] - 5))
    # ── Subnet ──
    p.append(subnet(60, 240, 240, 180, r=30))
    p.append(f'<rect x="130" y="310" width="100" height="60" rx="18" ry="18" '
             f'fill="#fff" stroke="#000" stroke-width="{SW}"/>')
    p.append(label(180, 348, "subnet", anchor="middle"))
    p += [wire([(180, 310), (180, 240)]), iface(180, 240, "I₁", dy=-12)]
    p += [wire([(130, 370), (90, 420)]),  iface(90, 420, "I₂", dy=24)]
    p += [wire([(230, 370), (270, 420)]), iface(270, 420, "I₃", dy=24)]
    p += [wire([(230, 340), (300, 340)]), iface(300, 340, "I₄", dx=10, dy=5, anchor="start")]
    p.append(label(360, 300, "Interface with numbered ports"))
    p.append(arrow(355, 305, 302, 288))
    return wrap("Legend", "\n".join(p))

def rule_12():
    """Rule 12:  δ ⋈ d  ⇒  (λ,δ) + ··· + (λ,δ)   (generalized over k)"""
    p = []
    # ── LHS ──
    p.append(subnet(*LHS))
    a1, q1 = agent(215, 140, "δ", {2: 225, 1: 315}, 90)
    a2, _  = agent(215, 300, "d", {}, 270)
    p += [a1, a2, wire([q1["P"], (215, 256)])]
    p += [wire([q1[2], (120, 55)]),  iface(120, 55, "I₂", dy=-12)]
    p += [wire([q1[1], (310, 55)]),  iface(310, 55, "I₁", dy=-12)]
    p += [wire([_p(215, 300, AR, 135), (120, 425)]), iface(120, 425, "I₃", dy=24)]
    p += [wire([_p(215, 300, AR, 45),  (310, 425)]), iface(310, 425, "Iₖ", dy=24)]
    p.append(dots(215, 388))
    # ── RHS ──
    p.append(subnet(*RHS))
    ll, _ = agent(620, 130, "λ", {}, 270)
    lr, _ = agent(780, 130, "λ", {}, 270)
    dl, ql = agent(620, 300, "δ", {2: 225, 1: 315}, 90)
    dr, qr = agent(780, 300, "δ", {2: 225, 1: 315}, 90)
    p += [ll, lr, dl, dr]
    # P-ports → interface
    p += [wire([(620, 86), (620, 55)]),  iface(620, 55, "I₂", dy=-12)]
    p += [wire([(780, 86), (780, 55)]),  iface(780, 55, "I₁", dy=-12)]
    p += [wire([ql["P"], (620, 425)]),   iface(620, 425, "I₃", dy=24)]
    p += [wire([qr["P"], (780, 425)]),   iface(780, 425, "Iₖ", dy=24)]
    # λ bottom wires (two per λ: one vertical, one crossing diagonal)
    p += [wire([_p(620, 130, AR, 90),  ql[2]]),   # λ_L ↓ → δ_L.2
          wire([_p(620, 130, AR, 45),  qr[2]]),   # λ_L ↘ → δ_R.2
          wire([_p(780, 130, AR, 90),  qr[1]]),   # λ_R ↓ → δ_R.1
          wire([_p(780, 130, AR, 135), ql[1]])]   # λ_R ↙ → δ_L.1
    p.append(dots(700, 305, "···"))
    p.append(imply())
    return wrap("Rule 12", "\n".join(p))

def rule_13():
    """Rule 13:  b ⋈ b′  ⇒  b′ + b′ + b"""
    p = []
    # ── LHS ──
    p.append(subnet(*LHS))
    a1, q1 = agent(215, 140, "b", {2: 270, 1: 180, 3: 0}, 90)
    a2, q2 = agent(215, 320, "b′", {3: 180, 1: 0, 2: 90}, 270)
    p += [a1, a2, wire([q1["P"], q2["P"]])]
    p += [wire([q1[2], (215, 55)]),  iface(215, 55, "I₂", dy=-12)]
    p += [wire([q1[1], (40, 140)]),   iface(40, 140, "I₃", dx=-10, dy=5, anchor="end")]
    p += [wire([q1[3], (390, 140)]),  iface(390, 140, "I₁", dx=10, dy=5, anchor="start")]
    p += [wire([q2[3], (40, 320)]),   iface(40, 320, "I₄", dx=-10, dy=5, anchor="end")]
    p += [wire([q2[1], (390, 320)]),  iface(390, 320, "I₆", dx=10, dy=5, anchor="start")]
    p += [wire([q2[2], (215, 425)]),  iface(215, 425, "I₅", dy=24)]
    # ── RHS ──
    p.append(subnet(*RHS))
    stl, qtl = agent(650, 140, "b′", {1: 270, 2: 0, 3: 90}, 180)
    sbl, qbl = agent(650, 300, "b′", {1: 270, 2: 0, 3: 90}, 180)
    sb, qb = agent(790, 220, "b", {2: 270, 3: 0, 1: 180}, 90)
    p += [stl, sbl, sb]
    # interface wires
    p += [wire([qtl[1], (650, 55)]),   iface(650, 55, "I₆", dy=-12)]
    p += [wire([qtl["P"], (510, 140)]), iface(510, 140, "I₂", dx=-10, dy=5, anchor="end")]
    p += [wire([qbl["P"], (510, 300)]), iface(510, 300, "I₃", dx=-10, dy=5, anchor="end")]
    p += [wire([qbl[3], (650, 425)]),   iface(650, 425, "I₄", dy=24)]
    p += [wire([qb[3], (860, 220)]),    iface(860, 220, "I₁", dx=10, dy=5, anchor="start")]
    p += [wire([qb["P"], (790, 425)]),  iface(790, 425, "I₅", dy=24)]
    # internal wires
    p += [wire([qtl[2], qb[2]]),   # b′_TL.2 ↔ b.2
          wire([qtl[3], qbl[1]]),  # b′_TL.3 ↔ b′_BL.1
          wire([qbl[2], qb[1]])]   # b′_BL.2 ↔ b.1
    p.append(imply())
    return wrap("Rule 13", "\n".join(p))

def rule_14():
    """Rule 14:  c ⋈ b  ⇒  δ + b′ + c + b′ + c"""
    p = []
    # ── LHS ──
    p.append(subnet(*LHS))
    a1, q1 = agent(150, 240, "c", {1: 225, 2: 135}, 0)
    a2, q2 = agent(280, 240, "b", {1: 270, 2: 0, 3: 90}, 180)
    p += [a1, a2, wire([q1["P"], q2["P"]])]
    p += [wire([q1[1], (40, 150)]),  iface(40, 150, "I₁", dx=-10, dy=5, anchor="end")]
    p += [wire([q1[2], (40, 330)]),  iface(40, 330, "I₂", dx=-10, dy=5, anchor="end")]
    p += [wire([q2[1], (280, 55)]),  iface(280, 55, "I₅", dy=-12)]
    p += [wire([q2[2], (390, 240)]), iface(390, 240, "I₄", dx=10, dy=5, anchor="start")]
    p += [wire([q2[3], (280, 425)]), iface(280, 425, "I₃", dy=24)]
    # ── RHS ──
    p.append(subnet(*RHS))
    sd, qd = agent(700, 100, "δ", {1: 225, 2: 90}, 270)
    sbu, qbu = agent(590, 170, "b′", {1: 270, 2: 0, 3: 90}, 180)
    scr, qcr = agent(810, 250, "c", {1: 225, 2: 135}, 0)
    sbl, qbl = agent(590, 320, "b′", {1: 270, 2: 315, 3: 45}, 180)
    scb, qcb = agent(730, 380, "c", {2: 225, 1: 315}, 90)
    p += [sd, sbu, scr, sbl, scb]
    # interface wires
    p += [wire([qd["P"], (700, 55)]),   iface(700, 55, "I₅", dy=-12)]
    p += [wire([qbu["P"], (510, 170)]), iface(510, 170, "I₁", dx=-10, dy=5, anchor="end")]
    p += [wire([qcr["P"], (860, 250)]), iface(860, 250, "I₄", dx=10, dy=5, anchor="start")]
    p += [wire([qbl["P"], (510, 320)]), iface(510, 320, "I₂", dx=-10, dy=5, anchor="end")]
    p += [wire([qcb["P"], (730, 425)]), iface(730, 425, "I₃", dy=24)]
    # internal wires
    p += [wire([qd[1], qbu[1]]),    # δ.1 ↔ b′_UL.1
          wire([qd[2], qbl[1]]),    # δ.2 ↔ b′_LL.1
          wire([qbu[2], qcr[1]]),   # b′_UL.2 ↔ c_R.1
          wire([qbu[3], qcb[2]]),   # b′_UL.3 ↔ c_B.2
          wire([qbl[2], qcr[2]]),   # b′_LL.2 ↔ c_R.2
          wire([qbl[3], qcb[1]])]   # b′_LL.3 ↔ c_B.1
    p.append(imply())
    return wrap("Rule 14", "\n".join(p))

def rule_15():
    """Rule 15:  d ⋈ b′  ⇒  wire(I₁—I₂) + d
    NOTE: LHS bottom interface is drawn as I₇ (RHS is I₂) — kept as drawn."""
    p = []
    # ── LHS ──
    p.append(subnet(*LHS))
    a1, q1 = agent(150, 240, "d", {}, 0)
    a2, q2 = agent(280, 240, "b′", {1: 270, 2: 0, 3: 90}, 180)
    p += [a1, a2, wire([q1["P"], q2["P"]])]
    p += [wire([q2[1], (280, 55)]),  iface(280, 55, "I₁", dy=-12)]
    p += [wire([q2[2], (390, 240)]), iface(390, 240, "I₃", dx=10, dy=5, anchor="start")]
    p += [wire([q2[3], (280, 425)]), iface(280, 425, "I₇", dy=24)]
    # ── RHS ──
    p.append(subnet(*RHS))
    a3, q3 = agent(780, 240, "d", {}, 0)
    p.append(a3)
    p.append(wire([(685, 55), (685, 425)]))          # vertical I₁—I₂
    p += [iface(685, 55, "I₁", dy=-12), iface(685, 425, "I₂", dy=24)]
    p += [wire([q3["P"], (860, 240)]), iface(860, 240, "I₃", dx=10, dy=5, anchor="start")]
    p.append(imply())
    return wrap("Rule 15", "\n".join(p))

def rule_16():
    """Rule 16:  ε ⋈ λ  ⇒  ε + ··· + ε   (generalized over k)"""
    p = []
    # ── LHS ──
    p.append(subnet(*LHS))
    a1, q1 = agent(215, 140, "ε", {}, 90)
    a2, _  = agent(215, 300, "λ", {}, 270)
    p += [a1, a2, wire([q1["P"], (215, 256)])]
    p += [wire([_p(215, 300, AR, 135), (120, 425)]), iface(120, 425, "I₁", dy=24)]
    p += [wire([_p(215, 300, AR, 45),  (310, 425)]), iface(310, 425, "Iₖ", dy=24)]
    p.append(dots(215, 388))
    # ── RHS ──
    p.append(subnet(*RHS))
    a3, q3 = agent(620, 240, "ε", {}, 90)
    a4, q4 = agent(780, 240, "ε", {}, 90)
    p += [a3, a4]
    p += [wire([q3["P"], (620, 425)]), iface(620, 425, "I₁", dy=24)]
    p += [wire([q4["P"], (780, 425)]), iface(780, 425, "Iₖ", dy=24)]
    p.append(dots(700, 245, "···"))
    p.append(imply())
    return wrap("Rule 16", "\n".join(p))

RULES = {
    "rule_01": rule_01,
    "rule_02": rule_02,
    "rule_03": rule_03,
    "rule_04": rule_04,
    "rule_05": rule_05,
    "rule_06": rule_06,
    "rule_07": rule_07,
    "rule_08": rule_08,
    "rule_09": rule_09,
    "rule_10": rule_10,
    "rule_11": rule_11,
    "rule_12": rule_12,
    "rule_13": rule_13,
    "rule_14": rule_14,
    "rule_15": rule_15,
    "rule_16": rule_16,
    "legend": legend,
    "reduction_rule_spec": reduction_rule_spec,
}

# ── Driver ────────────────────────────────────────────────────────────

def render(name, fn, no_png=False):
    os.makedirs(OUT, exist_ok=True)
    svg = fn()
    m = re.search(r'width="(\d+)" height="(\d+)"', svg)
    sw, sh = (int(m.group(1)), int(m.group(2))) if m else (W, H)
    sp = os.path.join(OUT, name + ".svg")
    pp = os.path.join(OUT, name + ".png")
    with open(sp, "w", encoding="utf-8") as f:
        f.write(svg)
    if not no_png:
        subprocess.run(
            ["inkscape", sp, "--export-type=png",
             f"--export-filename={pp}", "-w", str(sw * 2), "-h", str(sh * 2)],
            check=True, capture_output=True)
    print(f"✓ {name}")

def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    no_png = "--no-png" in sys.argv
    sel = args or list(RULES)
    for n in sel:
        if n in RULES:
            render(n, RULES[n], no_png)
        else:
            print(f"? unknown rule: {n}")

if __name__ == "__main__":
    main()
