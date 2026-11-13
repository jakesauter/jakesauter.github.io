/* Throttle Hills: physics + levels. No DOM here, so the same code runs in the browser and in the Node test bot.
   World units are meters, y points up. The bike is four particles (rear wheel, front wheel, seat, head) held
   together by distance constraints (position-based dynamics); each wheel also has a spin rate for traction. */
(function (root) {
  'use strict';
  const G = 15, WR = 0.38, KI = 0.5, MU = 1.4;
  const OMAX = 46, ENGINE = 320, LEAN = 11, SUB = 8;
  const REST = [[0, 0], [1.45, 0], [0.65, 0.62], [0.5, 1.45]];
  const MASS = [1, 1, 3, 1];
  const RAD = [WR, WR, 0.32, 0.27];
  const LINKS = [[0, 1, 1], [0, 2, 0.45], [1, 2, 0.45], [2, 3, 1], [0, 3, 0.7], [1, 3, 0.7]];

  /* ---------- level building ---------- */
  function buildLevel(def) {
    const lines = []; let line = [[-30, 0], [0, 0]]; let x = 0, y = 0;
    const cps = [], deco = [];
    const add = (nx, ny) => { line.push([nx, ny]); x = nx; y = ny; };
    const curve = (L, f, step = 0.5) => { const n = Math.max(2, Math.ceil(L / step)), x0 = x, y0 = y;
      for (let i = 1; i <= n; i++) { const u = i / n; add(x0 + L * u, y0 + f(u)); } };
    const ops = {
      flat: L => add(x + L, y),
      slope: (L, h) => curve(L, u => h * (u * u * (3 - 2 * u))),
      hill: (L, h) => curve(L, u => h * (1 - Math.cos(2 * Math.PI * u)) / 2),
      bumps: (n, L, h) => { for (let i = 0; i < n; i++) ops.hill(L, h); },
      ramp: (L, h) => curve(L, u => h * u * u, 0.3),
      kicker: (L, h) => { curve(L, u => h * u * u, 0.3); add(x + 0.05, y - h); },
      table: (L, h, top) => { curve(L, u => h * u * u, 0.3); add(x + top, y); curve(L * 1.3, u => -h * (u * u * (3 - 2 * u))); },
      gap: (L, dy = 0) => { lines.push(line); line = [[x + L, y + dy - 6], [x + L, y + dy]]; x += L; y += dy; },
      steps: (n, L, h) => { for (let i = 0; i < n; i++) { add(x + L, y); add(x + 0.25, y + h); } },
      drop: h => add(x + 0.05, y - h),
      cp: () => cps.push(x),
      tree: () => deco.push(['tree', x]), rock: () => deco.push(['rock', x]),
    };
    for (const s of def.segs) ops[s[0]](...s.slice(1));
    const finish = x + 4; ops.flat(30); lines.push(line);
    // segments bucketed by x for fast collision
    const segs = [], buckets = new Map(), BW = 2;
    for (const l of lines) for (let i = 0; i < l.length - 1; i++) {
      const s = [l[i][0], l[i][1], l[i + 1][0], l[i + 1][1]]; segs.push(s);
      const a = Math.floor((Math.min(s[0], s[2]) - 1) / BW), b = Math.floor((Math.max(s[0], s[2]) + 1) / BW);
      for (let k = a; k <= b; k++) { if (!buckets.has(k)) buckets.set(k, []); buckets.get(k).push(s); }
    }
    let minY = Infinity; for (const s of segs) minY = Math.min(minY, s[1], s[3]);
    return { ...def, lines, segs, buckets, BW, cps, deco, finish, end: x, minY };
  }

  function groundY(lv, gx) { // highest surface at x, or null over a gap
    let best = null;
    for (const s of lv.buckets.get(Math.floor(gx / lv.BW)) || []) {
      const [x1, y1, x2, y2] = s; if (x2 - x1 < 0.01) continue;
      if (gx >= x1 && gx <= x2) { const yy = y1 + (y2 - y1) * (gx - x1) / (x2 - x1); if (best === null || yy > best) best = yy; }
    }
    return best;
  }

  function closest(lv, px, py, r) { // deepest contact for a circle
    let hit = null;
    const a = Math.floor((px - r) / lv.BW), b = Math.floor((px + r) / lv.BW), seen = new Set();
    for (let k = a; k <= b; k++) for (const s of lv.buckets.get(k) || []) {
      if (seen.has(s)) continue; seen.add(s);
      const [x1, y1, x2, y2] = s, dx = x2 - x1, dy = y2 - y1, L2 = dx * dx + dy * dy || 1e-9;
      let t = ((px - x1) * dx + (py - y1) * dy) / L2; t = t < 0 ? 0 : t > 1 ? 1 : t;
      const cx = x1 + dx * t, cy = y1 + dy * t, ex = px - cx, ey = py - cy, d = Math.hypot(ex, ey);
      if (d < r && (!hit || d < hit.d)) {
        let nx, ny; if (d > 1e-6) { nx = ex / d; ny = ey / d; } else { const l = Math.sqrt(L2); nx = -dy / l; ny = dx / l; }
        hit = { d, nx, ny };
      }
    }
    return hit;
  }

  /* ---------- the bike ---------- */
  function spawn(lv, sx) {
    const gy = groundY(lv, sx) ?? 0, gy2 = groundY(lv, sx + 1.45) ?? gy;
    const ang = Math.atan2(gy2 - gy, 1.45), c = Math.cos(ang), s = Math.sin(ang);
    const p = REST.map(([rx, ry]) => { const X = sx + rx * c - ry * s, Y = gy + WR + 0.02 + rx * s + ry * c; return { x: X, y: Y, vx: 0, vy: 0, px: X, py: Y }; });
    return { p, w: [0, 0], wa: [0, 0], ground: [0, 0], air: 0, spin: 0, lastAng: ang, crashed: false, finished: false,
      t: 0, flips: 0, bonus: 0, events: [], started: false };
  }

  const comOf = b => { let x = 0, y = 0, m = 0; b.p.forEach((q, i) => { x += q.x * MASS[i]; y += q.y * MASS[i]; m += MASS[i]; }); return [x / m, y / m]; };
  const angleOf = b => Math.atan2(b.p[1].y - b.p[0].y, b.p[1].x - b.p[0].x);

  function step(lv, b, inp, dt) { // one frame; inp = {gas, brake, lean (+1 back / -1 forward)}
    if (b.finished) return;
    if (!b.crashed && (inp.gas || inp.brake || inp.lean)) b.started = true;
    const h = dt / SUB, live = !b.crashed;
    const gas = live && inp.gas, brake = live && inp.brake, lean = live ? (inp.lean || 0) : 0;
    for (let sI = 0; sI < SUB; sI++) {
      // engine and brakes act on wheel spin
      if (gas) b.w[0] += ENGINE * h * Math.max(0, 1 - b.w[0] / OMAX);
      if (brake) {
        for (let i = 0; i < 2; i++) b.w[i] *= Math.max(0, 1 - 14 * h);
        if (Math.abs(b.w[0]) < 2 && !gas) b.w[0] = Math.max(b.w[0] - 30 * h, -OMAX * 0.22);
      }
      if (!gas && !brake) for (let i = 0; i < 2; i++) b.w[i] *= 1 - 0.15 * h;
      // lean (and the engine lifting the front) as a torque about the center of mass
      const tq = lean * LEAN + (gas && b.ground[0] > 0 ? 1.7 : 0);
      const [cx, cy] = comOf(b);
      for (let i = 0; i < 4; i++) {
        const q = b.p[i];
        if (tq) { q.vx += -(q.y - cy) * tq * h; q.vy += (q.x - cx) * tq * h; }
        q.vy -= G * h; q.vx *= 1 - 0.03 * h; q.vy *= 1 - 0.03 * h;
        q.px = q.x; q.py = q.y; q.x += q.vx * h; q.y += q.vy * h;
      }
      for (let it = 0; it < 3; it++) for (const [i, j, k] of LINKS) {
        const A = b.p[i], B = b.p[j], rest = Math.hypot(REST[i][0] - REST[j][0], REST[i][1] - REST[j][1]);
        const dx = B.x - A.x, dy = B.y - A.y, d = Math.hypot(dx, dy) || 1e-9, diff = (d - rest) / d * k;
        const wa = 1 / MASS[i], wb = 1 / MASS[j], ws = wa + wb;
        A.x += dx * diff * wa / ws; A.y += dy * diff * wa / ws; B.x -= dx * diff * wb / ws; B.y -= dy * diff * wb / ws;
      }
      for (let i = 0; i < 4; i++) {
        const q = b.p[i], c = closest(lv, q.x, q.y, RAD[i]);
        if (i < 2) b.ground[i] = Math.max(0, b.ground[i] - h);
        if (!c) continue;
        const pen = RAD[i] - c.d; q.x += c.nx * pen; q.y += c.ny * pen;
        if (i >= 2) { if (!b.crashed && !b.finished) { b.crashed = true; b.events.push(['crash']); } continue; }
        b.ground[i] = 0.12;
        const tx = c.ny, ty = -c.nx; // tangent (forward along the ground)
        const vt = ((q.x - q.px) * tx + (q.y - q.py) * ty) / h;
        const slip = vt - b.w[i] * WR;
        let P = slip / (1 + 1 / KI); const lim = MU * pen / h; P = Math.max(-lim, Math.min(lim, P));
        q.x -= tx * P * h; q.y -= ty * P * h; b.w[i] += P / (KI * WR);
      }
      for (let i = 0; i < 4; i++) { const q = b.p[i]; q.vx = (q.x - q.px) / h; q.vy = (q.y - q.py) / h; }
      for (let i = 0; i < 2; i++) b.wa[i] -= b.w[i] * h;
    }
    if (b.started && !b.crashed) b.t += dt;
    // flips: count rotation while both wheels are off the ground
    const a = angleOf(b); let da = a - b.lastAng; if (da > Math.PI) da -= 2 * Math.PI; if (da < -Math.PI) da += 2 * Math.PI; b.lastAng = a;
    const airborne = b.ground[0] <= 0 && b.ground[1] <= 0;
    if (airborne) { b.air += dt; b.spin += da; }
    else {
      if (b.air > 0.25 && !b.crashed) {
        const n = Math.floor((Math.abs(b.spin) + 0.9) / (2 * Math.PI));
        if (n > 0) { b.flips += n; b.bonus += n; b.events.push([b.spin > 0 ? 'backflip' : 'frontflip', n]); }
        if (b.air > 1.1) b.events.push(['air', b.air]);
      }
      b.air = 0; b.spin = 0;
    }
    const [cx] = comOf(b);
    if (!b.crashed && b.p.every(q => q.y < lv.minY - 6)) { b.crashed = true; b.events.push(['fell']); }
    if (!b.crashed && cx > lv.finish) { b.finished = true; b.events.push(['finish']); }
  }

  const score = (lv, b) => Math.max(0, b.t - b.bonus);
  const stars = (lv, t) => t <= lv.par ? 3 : t <= lv.par * 1.3 ? 2 : 1;

  /* ---------- levels ---------- */
  const T = (name, theme, par, segs) => ({ name, theme, par, segs });
  const LEVELS = [
    T('First Ride', 'dunes', 11, [['flat', 12], ['hill', 18, 1.5], ['flat', 6], ['hill', 22, 2.5], ['cp'], ['bumps', 4, 3.5, 0.35], ['flat', 8], ['slope', 16, 3], ['flat', 6], ['slope', 16, -3], ['flat', 10]]),
    T('Sand Waves', 'dunes', 19, [['flat', 10], ['bumps', 3, 12, 2], ['flat', 6], ['kicker', 6, 1], ['flat', 12], ['cp'], ['hill', 20, 3.5], ['bumps', 6, 4, 0.5], ['slope', 14, 2.5], ['table', 6, 1.2, 4], ['flat', 10], ['hill', 16, -2], ['flat', 8]]),
    T('Jump School', 'dunes', 14.5, [['flat', 14], ['kicker', 7, 1.4], ['flat', 16], ['kicker', 7, 1.8], ['flat', 18], ['cp'], ['table', 7, 1.6, 6], ['flat', 10], ['ramp', 10, 2], ['gap', 4, -2], ['flat', 14], ['kicker', 8, 2], ['flat', 20]]),
    T('Camel Back', 'dunes', 22.5, [['flat', 10], ['hill', 14, 3], ['hill', 14, 3.5], ['hill', 16, 4], ['cp'], ['bumps', 8, 3, 0.6], ['flat', 6], ['slope', 18, 5], ['flat', 4], ['drop', 2.5], ['flat', 10], ['slope', 18, -2.5], ['cp'], ['hill', 12, 2.5], ['hill', 12, 2.5], ['flat', 10]]),
    T('Pine Trail', 'forest', 26, [['flat', 10], ['steps', 4, 4, 0.4], ['flat', 6], ['hill', 18, 3], ['cp'], ['kicker', 6, 1.5], ['flat', 14], ['steps', 4, 4, -0.5], ['flat', 8], ['bumps', 5, 3, 0.5], ['cp'], ['ramp', 14, 3.5], ['gap', 5, -1], ['flat', 12], ['hill', 20, 3], ['flat', 10]]),
    T('Root Steps', 'forest', 34, [['flat', 10], ['steps', 6, 3.2, 0.5], ['flat', 8], ['steps', 4, 4, -0.8], ['cp'], ['hill', 12, 2.5], ['steps', 5, 3, 0.5], ['flat', 12], ['ramp', 7, 1.4], ['gap', 5, -3], ['flat', 12], ['cp'], ['bumps', 6, 3, 0.7], ['kicker', 7, 2], ['flat', 16], ['steps', 3, 3.5, -0.6], ['flat', 10]]),
    T('Creek Gaps', 'forest', 17, [['flat', 12], ['ramp', 8, 1.2], ['gap', 4, -1.2], ['flat', 12], ['ramp', 8, 1.5], ['gap', 5, -1.5], ['flat', 10], ['cp'], ['hill', 16, 3], ['ramp', 10, 2], ['gap', 6, -2], ['flat', 12], ['cp'], ['bumps', 4, 4, 0.6], ['ramp', 10, 2.5], ['gap', 7, -2.5], ['flat', 16]]),
    T('Logging Road', 'forest', 31, [['flat', 10], ['hill', 26, 6], ['bumps', 5, 3, 0.6], ['cp'], ['slope', 20, 6], ['table', 6, 1.5, 8], ['slope', 18, -6], ['flat', 8], ['cp'], ['kicker', 8, 2.2], ['flat', 18], ['steps', 5, 3, 0.5], ['flat', 12], ['ramp', 7, 1.4], ['gap', 6, -3], ['flat', 10], ['cp'], ['hill', 18, -3], ['hill', 18, 4], ['flat', 10]]),
    T('Red Mesa', 'canyon', 21.5, [['flat', 12], ['slope', 16, 5], ['flat', 8], ['drop', 3], ['flat', 10], ['drop', 2], ['flat', 12], ['cp'], ['kicker', 8, 2.4], ['flat', 10], ['ramp', 6, 1], ['gap', 6, -2], ['flat', 14], ['cp'], ['hill', 20, 5], ['bumps', 6, 3, 0.7], ['ramp', 14, 3], ['gap', 7, -3], ['flat', 16]]),
    T('Arches', 'canyon', 35, [['flat', 12], ['hill', 18, -4], ['hill', 18, 5], ['cp'], ['table', 8, 2, 6], ['flat', 6], ['table', 8, 2.5, 8], ['flat', 8], ['cp'], ['steps', 5, 3, 0.5], ['flat', 12], ['ramp', 7, 1.4], ['gap', 7, -3.5], ['flat', 12], ['cp'], ['hill', 14, 3], ['hill', 14, -3], ['kicker', 9, 2.6], ['flat', 22]]),
    T('Night Rally', 'night', 24, [['flat', 12], ['bumps', 8, 3, 0.6], ['kicker', 8, 2.2], ['flat', 18], ['cp'], ['ramp', 14, 4], ['gap', 6, -2], ['flat', 10], ['steps', 5, 3, -0.7], ['cp'], ['hill', 22, 6], ['flat', 6], ['ramp', 10, 2.5], ['gap', 8, -3], ['flat', 14], ['cp'], ['table', 9, 2.4, 10], ['flat', 12]]),
    T('The Gauntlet', 'night', 66, [['flat', 12], ['steps', 5, 3, 0.5], ['flat', 12], ['ramp', 7, 1.4], ['gap', 6, -3], ['flat', 10], ['cp'], ['bumps', 8, 2.8, 0.75], ['kicker', 9, 2.6], ['flat', 20], ['cp'], ['hill', 18, -4], ['hill', 18, 6], ['ramp', 12, 3], ['gap', 8, -4], ['flat', 12], ['cp'], ['table', 9, 2.6, 8], ['flat', 6], ['steps', 4, 3.2, 0.5], ['flat', 12], ['ramp', 7, 1.4], ['gap', 7, -2.8], ['flat', 10], ['cp'], ['hill', 16, 3.5], ['bumps', 5, 3, 0.8], ['kicker', 9, 2.8], ['flat', 22]]),
  ];

  const api = { G, WR, REST, LEVELS, buildLevel, groundY, spawn, step, score, stars, comOf, angleOf, SUB };
  if (typeof module !== 'undefined') module.exports = api; else root.TH = api;
})(this);
