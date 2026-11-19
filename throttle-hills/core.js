/* Throttle Hills: physics + levels. No DOM here, so the same code runs in the browser and in the Node test bot.
   World units are meters, y points up.
   The bike is a rigid frame (bike + rider) and two wheels. Each wheel slides on a sprung, damped suspension track
   fixed to the frame (rear: up/down; front: along the raked fork) with travel limits. Wheels grip the ground
   through their spin (traction-limited); the engine spins the rear wheel and twists the frame the other way
   (wheelies); leaning twists the frame (flips). Only the rider's head or torso touching the ground is a crash. */
(function (root) {
  'use strict';
  const G = 8.5, WR = 0.38, SUB = 10;  // gravity measured from Bike Race footage: ≈6.5–8.5 m/s² at this bike size
  // frame, in "bike-local" coordinates: origin = rear axle at normal ride height, x toward the front axle, y up
  const COM = [0.62, 0.36];                    // center of mass of bike + rider
  const MC = 6, IC = 1.1, MW = 0.8, IW = 0.6 * 0.8 * WR * WR;
  const MU = 1.25;                             // tire grip
  const ENGINE = 21, OMAX = 58, REACT = 0.35;  // wheel torque (N·m), top wheel speed (rad/s ≈ 22 m/s), frame reaction
  const LEAN = 40, SPINMAX = 8.5, AIRDAMP = 2.2, GROUND_LEAN = 0.85, AIR_LEAN = 1.8;  // lean torque, max lean-driven spin (rad/s), spin settling in air
  const SUSP = [ // E = wheel position at full extension, u = compression direction, k/c = spring/damper, pre = preload
    { E: [0, -0.10], u: [0, 1], k: 850, c: 65, travel: 0.27 },
    { E: [1.45 + 0.10 * 0.30, -0.10 * 0.954], u: [-0.30, 0.954], k: 850, c: 65, travel: 0.27 },
  ];
  { // preload so each wheel sits 0.10 m into its travel at rest
    const rear = MC * G * (1.45 - COM[0]) / 1.45, front = MC * G * COM[0] / 1.45;
    SUSP[0].pre = rear / SUSP[0].k - 0.10; SUSP[1].pre = front / SUSP[1].k - 0.10;
  }
  const RIDER = [[0.62, 1.05, 0.24], [0.5, 1.45, 0.26]];      // torso, head: touching ground = crash
  const SKID = [[0.62, 0.18, 0.2], [1.02, 0.42, 0.16], [0.1, 0.42, 0.14]]; // frame/engine: may scrape
  const REST = [[0, 0], [1.45, 0], [0.65, 0.62], [0.5, 1.45]]; // for the renderer: rear axle, front axle, seat, head
  const MASS = [1, 1, 3, 1];

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
  const rot = (a, x, y) => { const c = Math.cos(a), s = Math.sin(a); return [x * c - y * s, x * s + y * c]; };
  // world position of a bike-local point, and its offset from the center of mass
  function framePt(b, lx, ly) { const [rx, ry] = rot(b.a, lx - COM[0], ly - COM[1]); return [b.cx + rx, b.cy + ry, rx, ry]; }

  function spawn(lv, sx) {
    const gy = groundY(lv, sx) ?? 0, gy2 = groundY(lv, sx + 1.45) ?? gy;
    const a = Math.atan2(gy2 - gy, 1.45);
    const [ox, oy] = [sx, gy + WR + 0.03];
    const [rx, ry] = rot(a, COM[0], COM[1]);
    const b = { cx: ox + rx, cy: oy + ry, a, vx: 0, vy: 0, om: 0,
      wh: [], q: [0.10, 0.10], w: [0, 0], wa: [0, 0], ground: [0, 0], air: 0, spin: 0, lastAng: a,
      crashed: false, finished: false, t: 0, flips: 0, bonus: 0, events: [], started: false, p: [] };
    for (let i = 0; i < 2; i++) {
      const S = SUSP[i], lx = S.E[0] + S.u[0] * 0.10, ly = S.E[1] + S.u[1] * 0.10, [x, y] = framePt(b, lx, ly);
      b.wh.push({ x, y, vx: 0, vy: 0 });
    }
    syncPts(b);
    return b;
  }

  // the four points the renderer, camera and bot read (rear wheel, front wheel, seat, head)
  function syncPts(b) {
    const pt = (x, y, vx, vy) => ({ x, y, vx, vy });
    b.p[0] = pt(b.wh[0].x, b.wh[0].y, b.wh[0].vx, b.wh[0].vy);
    b.p[1] = pt(b.wh[1].x, b.wh[1].y, b.wh[1].vx, b.wh[1].vy);
    for (const [k, [lx, ly]] of [[2, REST[2]], [3, REST[3]]]) {
      const [x, y, rx, ry] = framePt(b, lx, ly);
      b.p[k] = pt(x, y, b.vx - b.om * ry, b.vy + b.om * rx);
    }
  }

  const comOf = b => [b.cx, b.cy];
  const angleOf = b => b.a;
  // origin of the bike-local frame (for drawing): rear axle at ride height
  const frameOrigin = b => { const [x, y] = framePt(b, 0, 0); return [x, y]; };

  // apply an impulse (jx, jy) to the frame at offset (rx, ry) from the center of mass
  function hitFrame(b, rx, ry, jx, jy) { b.vx += jx / MC; b.vy += jy / MC; b.om += (rx * jy - ry * jx) / IC; }

  function step(lv, b, inp, dt) { // one frame; inp = {gas, brake, lean (+1 back / -1 forward)}
    if (b.finished) return;
    if (!b.crashed && (inp.gas || inp.brake || inp.lean)) b.started = true;
    const h = dt / SUB, live = !b.crashed;
    const gas = live && inp.gas, brake = live && inp.brake, lean = live ? (inp.lean || 0) : 0;
    for (let sI = 0; sI < SUB; sI++) {
      const airborne = b.ground[0] <= 0 && b.ground[1] <= 0;
      // ---- forces → velocities
      b.vy -= G * h;
      for (const W of b.wh) W.vy -= G * h;
      // engine: torque on the rear wheel, the opposite twist on the frame
      if (gas) { const T = ENGINE * Math.max(0, 1 - b.w[0] / OMAX); b.w[0] += T / IW * h; b.om += REACT * T / IC * h; }
      if (brake) {
        for (let i = 0; i < 2; i++) b.w[i] *= Math.max(0, 1 - 20 * h);
        if (Math.abs(b.w[0]) < 2 && !gas && b.ground[0] > 0) b.w[0] = Math.max(b.w[0] - 40 * h, -OMAX * 0.2);
      }
      if (!gas && !brake) for (let i = 0; i < 2; i++) b.w[i] *= 1 - 0.2 * h;
      // lean: twist the frame, up to a spin-rate cap; in the air, spin settles when you let go
      if (lean) b.om += lean * LEAN * (airborne ? AIR_LEAN : GROUND_LEAN) / IC * h * Math.max(0, 1 - lean * b.om / SPINMAX);
      else if (airborne) b.om *= 1 - AIRDAMP * h;
      b.vx *= 1 - 0.02 * h; b.vy *= 1 - 0.02 * h;
      // suspension springs + dampers
      const sus = [];
      for (let i = 0; i < 2; i++) {
        const S = SUSP[i], W = b.wh[i];
        const [ex, ey, rx, ry] = framePt(b, S.E[0], S.E[1]);
        const [ux, uy] = rot(b.a, S.u[0], S.u[1]), nx = -uy, ny = ux;
        const vax = b.vx - b.om * ry, vay = b.vy + b.om * rx;     // velocity of the track's base point
        const dx = W.x - ex, dy = W.y - ey, q = dx * ux + dy * uy;
        const qd = (W.vx - vax) * ux + (W.vy - vay) * uy;
        const stop = q > 0.17 ? 25000 * (q - 0.17) * (q - 0.17) : 0; // progressive bump stop near the end of travel
        const F = Math.max(0, S.k * (q + S.pre) + stop + S.c * qd);      // pushes wheel out (−u), frame in (+u)
        W.vx -= ux * F / MW * h; W.vy -= uy * F / MW * h;
        hitFrame(b, rx, ry, ux * F * h, uy * F * h);
        sus.push({ ex, ey, rx, ry, ux, uy, nx, ny });
        b.q[i] = q;
      }
      // ---- contacts (found once per substep)
      const wc = [0, 1].map(i => { const W = b.wh[i], c = closest(lv, W.x, W.y, WR); return c ? { ...c, pen: WR - c.d, ln: 0 } : null; });
      const bc = [];
      for (const [k, list] of [[1, RIDER], [0, SKID]]) for (const [lx, ly, r] of list) {
        const [x, y, rx, ry] = framePt(b, lx, ly), c = closest(lv, x, y, r);
        if (c) { bc.push({ ...c, pen: r - c.d, rx, ry, ln: 0, rider: k }); if (k && !b.crashed && !b.finished) { b.crashed = true; b.events.push(['crash']); } }
      }
      for (let i = 0; i < 2; i++) b.ground[i] = wc[i] ? 0.12 : Math.max(0, b.ground[i] - h);
      // ---- velocity constraints
      for (let it = 0; it < 6; it++) {
        // wheel stays on its suspension track (no sideways play)
        for (let i = 0; i < 2; i++) {
          const W = b.wh[i], s = sus[i];
          const vax = b.vx - b.om * s.ry, vay = b.vy + b.om * s.rx;
          const e = (W.x - s.ex) * s.nx + (W.y - s.ey) * s.ny;
          const ed = (W.vx - vax) * s.nx + (W.vy - vay) * s.ny;
          const cr = s.rx * s.ny - s.ry * s.nx, K = 1 / MW + 1 / MC + cr * cr / IC;
          const lam = -(ed + 0.25 / h * e) / K;
          W.vx += s.nx * lam / MW; W.vy += s.ny * lam / MW; hitFrame(b, s.rx, s.ry, -s.nx * lam, -s.ny * lam);
          // travel limits: topped out (q < 0) or bottomed out (q > travel)
          const q = (W.x - s.ex) * s.ux + (W.y - s.ey) * s.uy, qd = (W.vx - vax) * s.ux + (W.vy - vay) * s.uy;
          const T = SUSP[i].travel; let tgt = null;
          if (q < 0 && qd < 0) tgt = -0.2 / h * q; else if (q > T && qd > 0) tgt = -0.2 / h * (q - T);
          if (tgt !== null) {
            const cu = s.rx * s.uy - s.ry * s.ux, Ku = 1 / MW + 1 / MC + cu * cu / IC, l2 = (tgt - qd) / Ku;
            W.vx += s.ux * l2 / MW; W.vy += s.uy * l2 / MW; hitFrame(b, s.rx, s.ry, -s.ux * l2, -s.uy * l2);
          }
        }
        // tires on the ground: no sinking, grip through wheel spin
        for (let i = 0; i < 2; i++) {
          const c = wc[i]; if (!c) continue; const W = b.wh[i];
          const vn = W.vx * c.nx + W.vy * c.ny, bias = Math.max(0, c.pen - 0.004) * 0.3 / h;
          const old = c.ln; c.ln = Math.max(0, old + (bias - vn) * MW); const dl = c.ln - old;
          W.vx += c.nx * dl / MW; W.vy += c.ny * dl / MW;
          const tx = c.ny, ty = -c.nx, slip = W.vx * tx + W.vy * ty - b.w[i] * WR;
          let P = -slip / (1 / MW + WR * WR / IW); const lim = MU * c.ln; P = Math.max(-lim, Math.min(lim, P));
          W.vx += tx * P / MW; W.vy += ty * P / MW; b.w[i] -= P * WR / IW;
        }
        // frame and rider against the ground
        for (const c of bc) {
          const vx = b.vx - b.om * c.ry, vy = b.vy + b.om * c.rx, vn = vx * c.nx + vy * c.ny;
          const cr = c.rx * c.ny - c.ry * c.nx, K = 1 / MC + cr * cr / IC;
          const bias = Math.max(0, c.pen - 0.004) * 0.3 / h;
          const old = c.ln; c.ln = Math.max(0, old + (bias - vn) / K); const dl = c.ln - old;
          hitFrame(b, c.rx, c.ry, c.nx * dl, c.ny * dl);
          const tx = c.ny, ty = -c.nx, vt = (b.vx - b.om * c.ry) * tx + (b.vy + b.om * c.rx) * ty;
          const ct = c.rx * ty - c.ry * tx, Kt = 1 / MC + ct * ct / IC;
          const P = Math.max(-0.6 * c.ln, Math.min(0.6 * c.ln, -vt / Kt)) / 6;
          hitFrame(b, c.rx, c.ry, tx * P, ty * P);
        }
      }
      // ---- integrate positions
      b.cx += b.vx * h; b.cy += b.vy * h; b.a += b.om * h;
      for (let i = 0; i < 2; i++) { const W = b.wh[i]; W.x += W.vx * h; W.y += W.vy * h; b.wa[i] -= b.w[i] * h; }
    }
    syncPts(b);
    if (b.started && !b.crashed) b.t += dt;
    // flips: count frame rotation while both wheels are off the ground
    const a = b.a; let da = a - b.lastAng; b.lastAng = a;
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
    if (!b.crashed && b.p.every(q => q.y < lv.minY - 6)) { b.crashed = true; b.events.push(['fell']); }
    if (!b.crashed && b.cx > lv.finish) { b.finished = true; b.events.push(['finish']); }
  }

  const score = (lv, b) => Math.max(0, b.t - b.bonus);
  const stars = (lv, t) => t <= lv.par ? 3 : t <= lv.par * 1.3 ? 2 : 1;

  /* ---------- levels ---------- */
  const T = (name, theme, par, segs) => ({ name, theme, par, segs });
  const LEVELS = [
    T('First Ride', 'dunes', 13, [['flat', 12], ['hill', 18, 1.5], ['flat', 6], ['hill', 22, 2.5], ['cp'], ['bumps', 4, 3.5, 0.35], ['flat', 8], ['slope', 16, 3], ['flat', 6], ['slope', 16, -3], ['flat', 10]]),
    T('Sand Waves', 'dunes', 22, [['flat', 10], ['bumps', 3, 12, 2], ['flat', 6], ['kicker', 6, 1], ['flat', 12], ['cp'], ['hill', 20, 3.5], ['bumps', 6, 4, 0.5], ['slope', 14, 2.5], ['table', 6, 1.2, 4], ['flat', 10], ['hill', 16, -2], ['flat', 8]]),
    T('Jump School', 'dunes', 16.5, [['flat', 14], ['kicker', 7, 1.4], ['flat', 16], ['kicker', 7, 1.8], ['flat', 18], ['cp'], ['table', 7, 1.6, 6], ['flat', 10], ['ramp', 10, 2], ['gap', 4, -2], ['flat', 14], ['kicker', 8, 2], ['flat', 20]]),
    T('Camel Back', 'dunes', 23, [['flat', 10], ['hill', 14, 3], ['hill', 14, 3.5], ['hill', 16, 4], ['cp'], ['bumps', 8, 3, 0.6], ['flat', 6], ['slope', 18, 5], ['flat', 4], ['drop', 2.5], ['flat', 10], ['slope', 18, -2.5], ['cp'], ['hill', 12, 2.5], ['hill', 12, 2.5], ['flat', 10]]),
    T('Pine Trail', 'forest', 23, [['flat', 10], ['steps', 4, 4, 0.4], ['flat', 6], ['hill', 18, 3], ['cp'], ['kicker', 6, 1.5], ['flat', 14], ['steps', 4, 4, -0.5], ['flat', 8], ['bumps', 5, 3, 0.5], ['cp'], ['ramp', 14, 3.5], ['gap', 5, -1], ['flat', 12], ['hill', 20, 3], ['flat', 10]]),
    T('Root Steps', 'forest', 36, [['flat', 10], ['steps', 6, 3.2, 0.5], ['flat', 8], ['steps', 4, 4, -0.8], ['cp'], ['hill', 12, 2.5], ['steps', 5, 3, 0.5], ['flat', 12], ['ramp', 7, 1.4], ['gap', 5, -3], ['flat', 12], ['cp'], ['bumps', 6, 3, 0.7], ['kicker', 7, 2], ['flat', 16], ['steps', 3, 3.5, -0.6], ['flat', 10]]),
    T('Creek Gaps', 'forest', 18.5, [['flat', 12], ['ramp', 8, 1.2], ['gap', 4, -1.2], ['flat', 12], ['ramp', 8, 1.5], ['gap', 5, -1.5], ['flat', 10], ['cp'], ['hill', 16, 3], ['ramp', 10, 2], ['gap', 6, -2], ['flat', 12], ['cp'], ['bumps', 4, 4, 0.6], ['ramp', 10, 2.5], ['gap', 7, -2.5], ['flat', 16]]),
    T('Logging Road', 'forest', 31.5, [['flat', 10], ['hill', 26, 6], ['bumps', 5, 3, 0.6], ['cp'], ['slope', 20, 6], ['table', 6, 1.5, 8], ['slope', 18, -6], ['flat', 8], ['cp'], ['kicker', 8, 2.2], ['flat', 18], ['steps', 5, 3, 0.5], ['flat', 12], ['ramp', 7, 1.4], ['gap', 6, -3], ['flat', 10], ['cp'], ['hill', 18, -3], ['hill', 18, 4], ['flat', 10]]),
    T('Red Mesa', 'canyon', 21.5, [['flat', 12], ['slope', 16, 5], ['flat', 8], ['drop', 3], ['flat', 10], ['drop', 2], ['flat', 12], ['cp'], ['kicker', 8, 2.4], ['flat', 10], ['ramp', 6, 1], ['gap', 6, -2], ['flat', 14], ['cp'], ['hill', 20, 5], ['bumps', 6, 3, 0.7], ['ramp', 14, 3], ['gap', 7, -3], ['flat', 16]]),
    T('Arches', 'canyon', 35, [['flat', 12], ['hill', 18, -4], ['hill', 18, 5], ['cp'], ['table', 8, 2, 6], ['flat', 6], ['table', 8, 2.5, 8], ['flat', 8], ['cp'], ['steps', 5, 3, 0.5], ['flat', 12], ['ramp', 7, 1.4], ['gap', 7, -3.5], ['flat', 12], ['cp'], ['hill', 14, 3], ['hill', 14, -3], ['kicker', 9, 2.6], ['flat', 22]]),
    T('Night Rally', 'night', 27, [['flat', 12], ['bumps', 8, 3, 0.6], ['kicker', 8, 2.2], ['flat', 18], ['cp'], ['ramp', 14, 4], ['gap', 6, -2], ['flat', 10], ['steps', 5, 3, -0.7], ['cp'], ['hill', 22, 6], ['flat', 6], ['ramp', 10, 2.5], ['gap', 8, -3], ['flat', 14], ['cp'], ['table', 9, 2.4, 10], ['flat', 12]]),
    T('The Gauntlet', 'night', 58, [['flat', 12], ['steps', 5, 3, 0.5], ['flat', 12], ['ramp', 7, 1.4], ['gap', 6, -3], ['flat', 10], ['cp'], ['bumps', 8, 2.8, 0.75], ['kicker', 9, 2.6], ['flat', 20], ['cp'], ['hill', 18, -4], ['hill', 18, 6], ['ramp', 12, 3], ['gap', 8, -4], ['flat', 12], ['cp'], ['table', 9, 2.6, 8], ['flat', 6], ['steps', 4, 3.2, 0.5], ['flat', 12], ['ramp', 7, 1.4], ['gap', 7, -2.8], ['flat', 10], ['cp'], ['hill', 16, 3.5], ['bumps', 5, 3, 0.8], ['kicker', 9, 2.8], ['flat', 22]]),
  ];

  const api = { G, WR, REST, LEVELS, buildLevel, groundY, spawn, step, score, stars, comOf, angleOf, frameOrigin, SUB, SUSP };
  if (typeof module !== 'undefined') module.exports = api; else root.TH = api;
})(this);
