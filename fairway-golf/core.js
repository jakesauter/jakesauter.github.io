/* Fairway: golf physics + course. No DOM, so the same code runs in the browser and in the Node test bot.
   Units are meters and seconds; y is up. Each hole has its own coordinates: the tee is at (0, 0) and the hole
   runs roughly toward +z. Distances shown to players are in yards (and feet on the green). */
(function (root) {
  'use strict';
  const G = 9.81, YD = 1.0936, FT = 3.2808;
  const CUP_R = 0.08, CUP_CATCH_SPEED = 1.9;           // a ball faster than this over the cup lips out
  // ball aerodynamics (per unit mass): drag 0.5·ρ·Cd·A/m, lift 0.5·ρ·Cl·A/m
  const KD = 0.5 * 1.2 * 0.24 * 0.00143 / 0.0459;

  /* clubs: full-swing ball speed (m/s), launch angle (deg), lift coefficient (backspin), name */
  const CLUBS = [
    { id: 'driver', name: 'Driver', v: 71, launch: 11, cl: 0.165, bite: 0 },
    { id: 'wood', name: 'Wood', v: 64, launch: 13, cl: 0.17, bite: 0.1 },
    { id: 'liron', name: 'Long Iron', v: 55, launch: 16, cl: 0.18, bite: 0.3 },
    { id: 'siron', name: 'Short Iron', v: 46, launch: 22, cl: 0.2, bite: 0.5 },
    { id: 'wedge', name: 'Wedge', v: 37, launch: 30, cl: 0.22, bite: 0.7 },
    { id: 'sand', name: 'Sand Wedge', v: 30, launch: 38, cl: 0.22, bite: 0.8 },
    { id: 'putter', name: 'Putter', v: 10.5, launch: 0, cl: 0, bite: 0 },
  ];
  /* surfaces: bounce (restitution), bounce friction, rolling resistance (fraction of g), lie (ball speed kept) */
  const SURF = {
    tee:    { e: 0.32, mu: 0.3, roll: 0.24, lie: 1.0, name: 'Tee' },
    fairway:{ e: 0.32, mu: 0.3, roll: 0.24, lie: 1.0, name: 'Fairway' },
    rough:  { e: 0.18, mu: 0.5, roll: 0.55, lie: 0.85, name: 'Rough' },
    deep:   { e: 0.12, mu: 0.65, roll: 0.9, lie: 0.7, name: 'Deep rough' },
    sand:   { e: 0.04, mu: 0.9, roll: 0.9, lie: 0.6, name: 'Bunker' },
    green:  { e: 0.28, mu: 0.3, roll: 0.075, lie: 1.0, name: 'Green' },
    water:  { e: 0, mu: 1, roll: 1, lie: 1, name: 'Water' },
  };

  /* ---------- tiny seeded random ---------- */
  function rng(seed) { let s = seed >>> 0 || 1; return () => ((s = Math.imul(s ^ (s >>> 15), 2246822519) ^ Math.imul(s ^ (s >>> 13), 3266489917), (s ^= s >>> 16) >>> 0) / 4294967296); }

  /* ---------- the course ---------- */
  // path: centerline points [x, z] from tee to green; fw: fairway half-width; green: radius + slope (rise per m, x/z)
  const HOLES = [
    { name: 'Pine Gate', par: 4, path: [[0, 0], [0, 120], [-12, 230], [-20, 330]], fw: 18, green: { r: 15, sx: 0.012, sz: -0.02 },
      bunkers: [[14, 215, 9, 6], [-30, 318, 6, 9], [-6, 345, 8, 4]], water: [], hills: 2.5, seed: 11 },
    { name: 'Little Lake', par: 3, path: [[0, 0], [5, 75], [8, 150]], fw: 14, green: { r: 13, sx: -0.015, sz: 0.01 },
      bunkers: [[-6, 162, 7, 4], [22, 146, 5, 7]], water: [[3, 95, 22, 26]], hills: 1.2, seed: 22 },
    { name: 'Long Meadow', par: 5, path: [[0, 0], [0, 150], [30, 280], [40, 380], [30, 465]], fw: 20, green: { r: 16, sx: 0.02, sz: 0.008 },
      bunkers: [[-15, 160, 8, 10], [55, 300, 8, 8], [14, 450, 7, 5], [46, 470, 5, 8]], water: [[-5, 330, 14, 20]], hills: 3.5, seed: 33 },
    { name: 'Ridge Line', par: 4, path: [[0, 0], [10, 140], [-20, 260], [-35, 370]], fw: 17, green: { r: 14, sx: -0.022, sz: -0.01 },
      bunkers: [[30, 230, 8, 8], [-50, 250, 7, 10], [-22, 385, 9, 5]], water: [], hills: 4, seed: 44 },
    { name: 'Short Hop', par: 4, path: [[0, 0], [0, 110], [15, 210], [25, 300]], fw: 19, green: { r: 13, sx: 0.0, sz: -0.025 },
      bunkers: [[-14, 190, 7, 9], [40, 290, 6, 8], [10, 312, 8, 4]], water: [[45, 140, 16, 30]], hills: 2, seed: 55 },
    { name: 'Island Pin', par: 3, path: [[0, 0], [0, 85], [-4, 165]], fw: 12, green: { r: 14, sx: 0.015, sz: 0.015 },
      bunkers: [[12, 175, 5, 6]], water: [[-4, 140, 34, 18]], hills: 1, seed: 66 },
    { name: 'Dogleg Right', par: 4, path: [[0, 0], [0, 200], [60, 290], [95, 340]], fw: 18, green: { r: 14, sx: -0.01, sz: 0.02 },
      bunkers: [[22, 215, 9, 7], [-16, 205, 7, 9], [110, 330, 6, 8]], water: [], hills: 3, seed: 77 },
    { name: 'The Long Haul', par: 5, path: [[0, 0], [-10, 160], [-40, 300], [-30, 420], [-10, 490]], fw: 19, green: { r: 16, sx: 0.018, sz: -0.012 },
      bunkers: [[12, 170, 8, 8], [-62, 300, 8, 12], [-12, 405, 10, 6], [4, 500, 6, 6]], water: [[-15, 360, 20, 14]], hills: 4.5, seed: 88 },
    { name: 'Home Hole', par: 4, path: [[0, 0], [0, 160], [-25, 300], [-30, 380]], fw: 18, green: { r: 16, sx: -0.012, sz: 0.022 },
      bunkers: [[20, 270, 8, 10], [-52, 300, 7, 7], [-46, 392, 7, 6], [-12, 396, 6, 5]], water: [[-60, 200, 18, 40]], hills: 3, seed: 99 },
  ];

  function buildHole(def, idx) {
    const R = rng(def.seed);
    const path = def.path, end = path[path.length - 1], prev = path[path.length - 2];
    // pin: a few meters from the green's center, toward a random side
    const pa = R() * Math.PI * 2, pd = def.green.r * (0.15 + R() * 0.45);
    const pin = [end[0] + Math.cos(pa) * pd, end[1] + Math.sin(pa) * pd];
    // hills: a few soft waves, flattened on the tee and shaped on the green
    const waves = []; for (let i = 0; i < 5; i++) waves.push([0.004 + R() * 0.014, 0.004 + R() * 0.014, R() * 7, R() * 7, def.hills * (0.4 + R() * 0.6) / 2.5]);
    const trees = [];
    let len = 0; for (let i = 1; i < path.length; i++) len += Math.hypot(path[i][0] - path[i - 1][0], path[i][1] - path[i - 1][1]);
    for (let s = 20; s < len + 30; s += 7 + R() * 9) for (const side of [-1, 1]) {
      if (R() < 0.25) continue;
      const [px, pz, tx, tz] = alongPath(path, Math.min(s, len));
      const off = def.fw + 16 + R() * 28;
      trees.push({ x: px - tz * off * side, z: pz + tx * off * side, h: 9 + R() * 8, r: 2.4 + R() * 1.6 });
    }
    // wind: steady for the hole
    const windDir = R() * Math.PI * 2, windMph = Math.round(R() * 8);
    const hole = { ...def, idx, pin, waves, trees, len,
      wind: { x: Math.cos(windDir) * windMph * 0.447, z: Math.sin(windDir) * windMph * 0.447, mph: windMph, dir: windDir },
      tee: [0, 0], green: { ...def.green, x: end[0], z: end[1] } };
    hole.waterY = Math.min(...[0, ...def.water.map(w => baseY(hole, w[0], w[1]) - 0.8)]);
    return hole;
  }

  function alongPath(path, s) { // point and unit tangent at distance s along the path
    for (let i = 1; i < path.length; i++) {
      const dx = path[i][0] - path[i - 1][0], dz = path[i][1] - path[i - 1][1], L = Math.hypot(dx, dz);
      if (s <= L || i === path.length - 1) { const u = Math.min(1, s / L); return [path[i - 1][0] + dx * u, path[i - 1][1] + dz * u, dx / L, dz / L]; }
      s -= L;
    }
  }
  function distToPath(path, x, z) {
    let best = Infinity;
    for (let i = 1; i < path.length; i++) {
      const ax = path[i - 1][0], az = path[i - 1][1], dx = path[i][0] - ax, dz = path[i][1] - az, L2 = dx * dx + dz * dz;
      let t = ((x - ax) * dx + (z - az) * dz) / L2; t = Math.max(0, Math.min(1, t));
      best = Math.min(best, Math.hypot(x - ax - dx * t, z - az - dz * t));
    }
    return best;
  }
  const inEllipse = (e, x, z, grow = 0) => { const a = (x - e[0]) / (e[2] + grow), b = (z - e[1]) / (e[3] + grow); return a * a + b * b; };

  function baseY(h, x, z) { let y = 0; for (const [kx, kz, px, pz, a] of h.waves) y += a * Math.sin(kx * x + px) * Math.cos(kz * z + pz); return y; }

  function heightAt(h, x, z) {
    let y = baseY(h, x, z);
    // tee box: flat
    const dt = Math.hypot(x, z); if (dt < 9) { const t = Math.max(0, (dt - 5) / 4), y0 = baseY(h, 0, 0); y = y0 + (y - y0) * t * t; }
    // green: a tilted plane, blended into the surroundings
    const g = h.green, dg = Math.hypot(x - g.x, z - g.z), gy0 = baseY(h, g.x, g.z);
    if (dg < g.r + 8) { const plane = gy0 + g.sx * (x - g.x) + g.sz * (z - g.z), t = Math.max(0, Math.min(1, (dg - g.r) / 8)); y = plane + (y - plane) * t * t * (3 - 2 * t); }
    // bunkers: shallow bowls
    for (const b of h.bunkers) { const q = inEllipse(b, x, z); if (q < 1) y -= 0.5 * (1 - q); }
    // water: sunk below the surface
    for (const w of h.water) { const q = inEllipse(w, x, z, 4); if (q < 1) y = Math.min(y, h.waterY + 0.3 + q * 2); }
    return y;
  }
  function normalAt(h, x, z) {
    const e = 0.25, dx = heightAt(h, x + e, z) - heightAt(h, x - e, z), dz = heightAt(h, x, z + e) - heightAt(h, x, z - e);
    const nx = -dx / (2 * e), nz = -dz / (2 * e), L = Math.hypot(nx, 1, nz); return [nx / L, 1 / L, nz / L];
  }
  function surfaceAt(h, x, z) {
    for (const w of h.water) if (inEllipse(w, x, z) < 1) return 'water';
    for (const b of h.bunkers) if (inEllipse(b, x, z) < 1) return 'sand';
    if (Math.hypot(x - h.green.x, z - h.green.z) < h.green.r) return 'green';
    if (Math.hypot(x, z) < 6) return 'tee';
    const d = distToPath(h.path, x, z);
    if (d < h.fw) return 'fairway';
    if (d < h.fw + 20 || Math.hypot(x - h.green.x, z - h.green.z) < h.green.r + 10) return 'rough';
    return 'deep';
  }

  /* ---------- shots ---------- */
  // aim: heading in radians (0 = +z, positive = toward +x); power 0..1.1; side −1..1 (− = hook left, + = slice right)
  function launch(h, ball, club, aim, power, side) {
    const C = CLUBS.find(c => c.id === club), surf = surfaceAt(h, ball.x, ball.z);
    let lie = SURF[surf].lie; if (surf === 'sand' && club === 'sand') lie = 0.92; if (club === 'putter') lie = 1;
    const p = Math.max(0, Math.min(1.1, power)), over = Math.max(0, p - 1);
    const s = Math.max(-1, Math.min(1, side)) + over * 4 * (side >= 0 ? 1 : -1); // overswing makes a miss worse
    const v = C.v * p * lie, dir = aim + s * 0.035, la = C.launch * Math.PI / 180;
    const fx = Math.sin(dir), fz = Math.cos(dir);
    return { x: ball.x, y: heightAt(h, ball.x, ball.z) + 0.02, z: ball.z,
      vx: fx * v * Math.cos(la), vy: v * Math.sin(la), vz: fz * v * Math.cos(la),
      cl: C.cl, bite: C.bite, tilt: s * 0.28, putt: club === 'putter', club, t: 0, air: club !== 'putter', bounces: 0,
      carry: null, done: false, holed: false, inWater: false, start: [ball.x, ball.z], path: [] };
  }

  // advance a ball in flight/rolling by dt; returns the ball
  function stepBall(h, b, dt) {
    if (b.done) return b;
    const N = Math.max(1, Math.ceil(dt / 0.004)), sdt = dt / N;
    for (let k = 0; k < N && !b.done; k++) {
      b.t += sdt;
      const gy = heightAt(h, b.x, b.z);
      if (b.air) {
        const rx = b.vx - h.wind.x, ry = b.vy, rz = b.vz - h.wind.z, sp = Math.hypot(rx, ry, rz) || 1e-9;
        // lift: perpendicular to motion, mostly up (backspin), tilted sideways by sidespin
        const hx = rx / sp, hy = ry / sp, hz = rz / sp;
        const sideX = hz, sideZ = -hx, sl = Math.hypot(sideX, sideZ) || 1;  // horizontal right-hand vector
        let ux = -hy * hx, uy = 1 - hy * hy, uz = -hy * hz; const ul = Math.hypot(ux, uy, uz) || 1; ux /= ul; uy /= ul; uz /= ul;
        const ct = Math.cos(b.tilt), st = Math.sin(b.tilt);
        const lx = ux * ct + (sideX / sl) * st, ly = uy * ct, lz = uz * ct + (sideZ / sl) * st;
        const lift = 0.5 * 1.2 * b.cl * 0.00143 / 0.0459 * sp * sp;
        b.vx += (-KD * sp * rx + lift * lx) * sdt;
        b.vy += (-KD * sp * ry + lift * ly - G) * sdt;
        b.vz += (-KD * sp * rz + lift * lz) * sdt;
        b.cl *= 1 - 0.04 * sdt;                                       // spin slowly decays
        b.x += b.vx * sdt; b.y += b.vy * sdt; b.z += b.vz * sdt;
        hitTrees(h, b);
        if (b.y <= heightAt(h, b.x, b.z)) touchDown(h, b);
      } else {
        // rolling: gravity along the slope, rolling resistance against motion
        const [nx, ny, nz] = normalAt(h, b.x, b.z), S = SURF[surfaceAt(h, b.x, b.z)];
        if (S === SURF.water) { b.inWater = true; b.done = true; break; }
        let ax = G * nx * ny, az = G * nz * ny;                          // downhill pull (horizontal part)
        const sp = Math.hypot(b.vx, b.vz);
        if (sp > 1e-4) { ax -= S.roll * G * b.vx / sp; az -= S.roll * G * b.vz / sp; }
        b.vx += ax * sdt; b.vz += az * sdt;
        const sp2 = Math.hypot(b.vx, b.vz);
        if (sp > 1e-4 && b.vx * b.vx + b.vz * b.vz > 0 && (b.vx * (b.vx - ax * sdt) + b.vz * (b.vz - az * sdt)) < 0 && Math.hypot(G * nx * ny, G * nz * ny) < S.roll * G * 1.2) { b.vx = 0; b.vz = 0; }
        b.x += b.vx * sdt; b.z += b.vz * sdt; b.y = heightAt(h, b.x, b.z);
        // the cup
        const dc = Math.hypot(b.x - h.pin[0], b.z - h.pin[1]);
        if (dc < CUP_R && sp2 < CUP_CATCH_SPEED) { b.holed = true; b.done = true; b.x = h.pin[0]; b.z = h.pin[1]; break; }
        if (sp2 < 0.03 && Math.hypot(G * nx * ny, G * nz * ny) < S.roll * G * 1.2) { b.vx = b.vz = 0; b.done = true; }
        if (b.t > 40) b.done = true;
      }
      if (b.path.length === 0 || b.t - b.path[b.path.length - 1][3] > 0.05) b.path.push([b.x, b.y, b.z, b.t]);
    }
    return b;
  }
  function touchDown(h, b) {
    const surf = surfaceAt(h, b.x, b.z), S = SURF[surf];
    if (b.carry === null) b.carry = { x: b.x, z: b.z, t: b.t, surf };
    if (surf === 'water') { b.inWater = true; b.done = true; return; }
    const [nx, ny, nz] = normalAt(h, b.x, b.z);
    b.y = heightAt(h, b.x, b.z);
    const vn = b.vx * nx + b.vy * ny + b.vz * nz;
    let tx = b.vx - vn * nx, ty = b.vy - vn * ny, tz = b.vz - vn * nz;
    let keep = 1 - S.mu * (surf === 'green' ? 1 + b.cl * 2 : 1);     // backspin bites more on the green
    if (b.bounces === 0) keep *= 1 - (b.bite || 0);                   // lofted clubs check up where they land
    tx *= keep; ty *= keep; tz *= keep;
    const bounceV = -vn * S.e;
    b.bounces++;
    if (bounceV < 1.2 || b.bounces > 8) { b.air = false; b.vx = tx; b.vz = tz; b.vy = 0; b.cl = 0; }
    else { b.vx = tx + nx * bounceV; b.vy = ty + ny * bounceV; b.vz = tz + nz * bounceV; b.y += 0.01; b.cl *= 0.3; b.tilt = 0; }
  }
  function hitTrees(h, b) {
    for (const t of h.trees) {
      if (Math.abs(b.x - t.x) > t.r + 1 || Math.abs(b.z - t.z) > t.r + 1) continue;
      const gy = heightAt(h, t.x, t.z), d = Math.hypot(b.x - t.x, b.z - t.z), cy = gy + t.h * 0.55;
      if (b.y > gy && b.y < gy + t.h && d < t.r * (1 - (b.y - cy) / (t.h * 0.6)) * 0.9) {
        // knocked down by branches: lose most speed, drop
        b.vx *= 0.15; b.vz *= 0.15; b.vy = Math.min(b.vy, 0) * 0.3; b.cl = 0; b.tilt = 0;
        b.x += (b.x - t.x) / (d || 1) * 0.2; b.z += (b.z - t.z) / (d || 1) * 0.2;
        if (!b.hitTree) b.hitTree = true;
      }
    }
  }
  // run a whole shot (for the bot and the landing preview)
  function simulate(h, ball, club, aim, power, side) { const b = launch(h, ball, club, aim, power, side); let n = 0; while (!b.done && n++ < 20000) stepBall(h, b, 1 / 60); return b; }

  const dist = (a, b) => Math.hypot(a[0] - b[0], a[1] - b[1]);
  const scoreName = (strokes, par) => strokes === 1 ? 'Hole in One' : ({ '-3': 'Albatross', '-2': 'Eagle', '-1': 'Birdie', '0': 'Par', '1': 'Bogey', '2': 'Double Bogey', '3': 'Triple Bogey' })[strokes - par] || `+${strokes - par}`;

  const api = { G, YD, FT, CLUBS, SURF, HOLES, buildHole, heightAt, normalAt, surfaceAt, launch, stepBall, simulate, dist, scoreName, CUP_R, alongPath, distToPath };
  if (typeof module !== 'undefined') module.exports = api; else root.FG = api;
})(this);
