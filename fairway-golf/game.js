/* Fairway Golf: 3D view, controls, HUD, sound and round flow. Physics and the course live in core.js (window.FG). */
import * as THREE from './vendor/three.module.min.js';
const FG = window.FG, $ = (id) => document.getElementById(id);
const isTouch = matchMedia('(pointer: coarse)').matches;

/* ---------------- renderer + scene ---------------- */
const canvas = $('gl');
const renderer = new THREE.WebGLRenderer({ canvas, antialias: true });
renderer.setPixelRatio(Math.min(2, devicePixelRatio || 1));
const scene = new THREE.Scene();
scene.background = new THREE.Color('#9fd6ff');
scene.fog = new THREE.Fog('#bfe3ff', 220, 700);
const camera = new THREE.PerspectiveCamera(55, 1, 0.1, 2000);
scene.add(new THREE.HemisphereLight('#dff3ff', '#3e7a33', 1.35));
const sun = new THREE.DirectionalLight('#fff4dc', 1.6); sun.position.set(-120, 200, -60); scene.add(sun);
// sky dome with a soft gradient
{
  const g = new THREE.SphereGeometry(1500, 24, 12), c = [], top = new THREE.Color('#5fb1ff'), low = new THREE.Color('#d9f0ff');
  const p = g.attributes.position; for (let i = 0; i < p.count; i++) { const t = Math.max(0, p.getY(i) / 1500); const col = low.clone().lerp(top, Math.pow(t, 0.6)); c.push(col.r, col.g, col.b); }
  g.setAttribute('color', new THREE.Float32BufferAttribute(c, 3));
  scene.add(new THREE.Mesh(g, new THREE.MeshBasicMaterial({ vertexColors: true, side: THREE.BackSide, fog: false })));
}
function resize() {
  const w = innerWidth, h = innerHeight; renderer.setSize(w, h, false); camera.aspect = w / h; camera.updateProjectionMatrix();
  $('rotate').hidden = !(isTouch && h > w * 1.1);
}
addEventListener('resize', resize); resize();

/* ---------------- sound (synthesized) ---------------- */
const audio = {
  ctx: null, muted: (() => { try { return localStorage.getItem('fg_muted') === '1'; } catch { return false; } })(),
  unlock() { if (this.muted) return; if (!this.ctx) { try { this.ctx = new (window.AudioContext || window.webkitAudioContext)(); } catch { return; } } if (this.ctx.state === 'suspended') this.ctx.resume(); },
  noise(dur, freq, q, gain, type = 'bandpass') {
    if (!this.ctx || this.muted) return; const C = this.ctx, n = C.sampleRate * dur, buf = C.createBuffer(1, n, C.sampleRate), d = buf.getChannelData(0);
    for (let i = 0; i < n; i++) d[i] = (Math.random() * 2 - 1) * (1 - i / n);
    const s = C.createBufferSource(), f = C.createBiquadFilter(), g = C.createGain(); s.buffer = buf; f.type = type; f.frequency.value = freq; f.Q.value = q; g.gain.value = gain;
    s.connect(f).connect(g).connect(C.destination); s.start();
  },
  tone(freq, dur, gain = 0.15, type = 'sine') {
    if (!this.ctx || this.muted) return; const C = this.ctx, o = C.createOscillator(), g = C.createGain(); o.type = type; o.frequency.value = freq;
    g.gain.setValueAtTime(gain, C.currentTime); g.gain.exponentialRampToValueAtTime(0.001, C.currentTime + dur); o.connect(g).connect(C.destination); o.start(); o.stop(C.currentTime + dur);
  },
  hit(p) { this.noise(0.08, 2500, 1.5, 0.6 * Math.min(1, p + 0.3)); this.tone(1400, 0.06, 0.2, 'triangle'); },
  whoosh() { this.noise(0.25, 900, 0.8, 0.25); },
  cup() { this.tone(1600, 0.08, 0.2, 'triangle'); setTimeout(() => this.tone(1100, 0.1, 0.18, 'triangle'), 90); setTimeout(() => this.tone(800, 0.15, 0.15, 'triangle'), 200); },
  splash() { this.noise(0.6, 600, 0.5, 0.5, 'lowpass'); },
  cheer() { this.noise(1.6, 1800, 0.4, 0.22); },
  setMuted(m) { this.muted = m; try { localStorage.setItem('fg_muted', m ? '1' : '0'); } catch {} if (!m) this.unlock(); },
};

/* ---------------- course meshes ---------------- */
const COL = {
  tee: '#6bcd5c', fairwayA: '#5cbf4a', fairwayB: '#51b043', greenA: '#74d75c', greenB: '#69cd52', rough: '#3f9a35', deep: '#2f7d2b', sand: '#ead9a0', water: '#2c6f8c',
};
const cache = {}; const col = (k) => cache[k] || (cache[k] = new THREE.Color(COL[k]));
let course = null;
function buildCourse(h) {
  if (course) { scene.remove(course); course.traverse((o) => { if (o.geometry) o.geometry.dispose(); }); }
  course = new THREE.Group(); scene.add(course);
  // bounds around the hole
  let x0 = Infinity, x1 = -Infinity, z0 = Infinity, z1 = -Infinity;
  for (const [x, z] of h.path) { x0 = Math.min(x0, x); x1 = Math.max(x1, x); z0 = Math.min(z0, z); z1 = Math.max(z1, z); }
  const M = h.fw + 70; x0 -= M; x1 += M; z0 -= 50; z1 += 60;
  const step = 2, nx = Math.ceil((x1 - x0) / step), nz = Math.ceil((z1 - z0) / step);
  const pos = [], cols = [], idx = [];
  for (let j = 0; j <= nz; j++) for (let i = 0; i <= nx; i++) {
    const x = x0 + i * step, z = z0 + j * step, y = FG.heightAt(h, x, z), s = FG.surfaceAt(h, x, z);
    pos.push(x, y, z);
    let c;
    if (s === 'fairway') c = col(Math.floor(z / 10) % 2 ? 'fairwayA' : 'fairwayB');
    else if (s === 'green') c = col((Math.floor(x / 2.5) + Math.floor(z / 2.5)) % 2 ? 'greenA' : 'greenB');
    else c = col(s === 'water' ? 'water' : s);
    const shade = 0.94 + 0.06 * Math.sin(x * 0.11) * Math.cos(z * 0.07);
    cols.push(c.r * shade, c.g * shade, c.b * shade);
  }
  for (let j = 0; j < nz; j++) for (let i = 0; i < nx; i++) { const a = j * (nx + 1) + i, b = a + 1, c = a + nx + 1, d = c + 1; idx.push(a, c, b, b, c, d); }
  const g = new THREE.BufferGeometry(); g.setAttribute('position', new THREE.Float32BufferAttribute(pos, 3)); g.setAttribute('color', new THREE.Float32BufferAttribute(cols, 3));
  g.setIndex(idx); g.computeVertexNormals();
  course.add(new THREE.Mesh(g, new THREE.MeshLambertMaterial({ vertexColors: true })));
  // water
  for (const w of h.water) {
    const m = new THREE.Mesh(new THREE.CircleGeometry(1, 48), new THREE.MeshPhongMaterial({ color: '#3f8fbf', transparent: true, opacity: 0.85, shininess: 90 }));
    m.rotation.x = -Math.PI / 2; m.scale.set(w[2] + 2, w[3] + 2, 1); m.position.set(w[0], h.waterY + 0.35, w[1]); course.add(m);
  }
  // trees (instanced)
  const trunkG = new THREE.CylinderGeometry(0.25, 0.35, 1, 6), crownG = new THREE.ConeGeometry(1, 1, 8);
  const trunks = new THREE.InstancedMesh(trunkG, new THREE.MeshLambertMaterial({ color: '#6b4a2b' }), h.trees.length);
  const crowns = new THREE.InstancedMesh(crownG, new THREE.MeshLambertMaterial({ color: '#2f7a3a' }), h.trees.length);
  const m4 = new THREE.Matrix4(), q = new THREE.Quaternion(), sc = new THREE.Vector3(), p3 = new THREE.Vector3();
  h.trees.forEach((t, i) => {
    const gy = FG.heightAt(h, t.x, t.z);
    m4.compose(p3.set(t.x, gy + t.h * 0.15, t.z), q, sc.set(1, t.h * 0.3, 1)); trunks.setMatrixAt(i, m4);
    m4.compose(p3.set(t.x, gy + t.h * 0.62, t.z), q, sc.set(t.r, t.h * 0.85, t.r)); crowns.setMatrixAt(i, m4);
    crowns.setColorAt(i, new THREE.Color().setHSL(0.33 + (i % 5) * 0.012, 0.45, 0.28 + (i % 3) * 0.03));
  });
  course.add(trunks, crowns);
  // the cup and flag
  const py = FG.heightAt(h, h.pin[0], h.pin[1]);
  const cup = new THREE.Mesh(new THREE.CircleGeometry(0.16, 24), new THREE.MeshBasicMaterial({ color: '#1a1a1a', polygonOffset: true, polygonOffsetFactor: -2 }));
  cup.rotation.x = -Math.PI / 2; cup.position.set(h.pin[0], py + 0.02, h.pin[1]); course.add(cup);
  const pole = new THREE.Mesh(new THREE.CylinderGeometry(0.035, 0.035, 2.6, 6), new THREE.MeshLambertMaterial({ color: '#f4f4f4' }));
  pole.position.set(h.pin[0], py + 1.3, h.pin[1]); course.add(pole);
  flag = new THREE.Mesh(new THREE.PlaneGeometry(0.9, 0.55, 6, 1), new THREE.MeshLambertMaterial({ color: '#e8402f', side: THREE.DoubleSide }));
  flag.position.set(h.pin[0] + 0.45, py + 2.3, h.pin[1]); course.add(flag);
  // slope grid dots on the green (shown while putting)
  const pts = [];
  for (let x = -h.green.r; x <= h.green.r; x += 1.3) for (let z = -h.green.r; z <= h.green.r; z += 1.3) if (Math.hypot(x, z) < h.green.r - 0.5) pts.push([h.green.x + x, h.green.z + z, Math.random()]);
  slopeDots = { base: pts, mesh: new THREE.Points(new THREE.BufferGeometry().setAttribute('position', new THREE.Float32BufferAttribute(new Float32Array(pts.length * 3), 3)),
    new THREE.PointsMaterial({ color: '#ffffff', size: 0.11, map: dotTex, alphaTest: 0.3, transparent: true, opacity: 0.85, depthWrite: false })) };
  slopeDots.mesh.visible = false; course.add(slopeDots.mesh);
}
let flag = null, slopeDots = null;
const dotTex = (() => { const c = document.createElement('canvas'); c.width = c.height = 32; const x = c.getContext('2d'); const g = x.createRadialGradient(16, 16, 2, 16, 16, 15);
  g.addColorStop(0, 'rgba(255,255,255,1)'); g.addColorStop(1, 'rgba(255,255,255,0)'); x.fillStyle = g; x.beginPath(); x.arc(16, 16, 15, 0, 7); x.fill(); return new THREE.CanvasTexture(c); })();

/* ball, trail, aim line, golfer */
const ballMesh = new THREE.Mesh(new THREE.SphereGeometry(0.11, 16, 12), new THREE.MeshLambertMaterial({ color: '#ffffff', emissive: '#555555' }));
scene.add(ballMesh);
const shadow = new THREE.Mesh(new THREE.CircleGeometry(0.14, 16), new THREE.MeshBasicMaterial({ color: '#000', transparent: true, opacity: 0.35, depthWrite: false }));
shadow.rotation.x = -Math.PI / 2; scene.add(shadow);
const trail = new THREE.Line(new THREE.BufferGeometry(), new THREE.LineBasicMaterial({ color: '#ffffff', transparent: true, opacity: 0.7 })); scene.add(trail);
const aimDots = new THREE.Points(new THREE.BufferGeometry(), new THREE.PointsMaterial({ color: '#4fc3ff', size: 0.35 })); scene.add(aimDots);
const landMark = new THREE.Mesh(new THREE.OctahedronGeometry(0.9), new THREE.MeshBasicMaterial({ color: '#ffd23a' })); scene.add(landMark);

function makeGolfer() {
  const g = new THREE.Group(), mat = (c) => new THREE.MeshLambertMaterial({ color: c });
  const cap = (r, l, c) => new THREE.Mesh(new THREE.CapsuleGeometry(r, l, 4, 8), mat(c));
  const legL = cap(0.09, 0.7, '#2e3a59'), legR = cap(0.09, 0.7, '#2e3a59'); legL.position.set(-0.13, 0.45, 0); legR.position.set(0.13, 0.45, 0);
  const body = cap(0.2, 0.45, '#e85d3a'); body.position.set(0, 1.15, -0.05); body.rotation.x = 0.35;
  const head = new THREE.Mesh(new THREE.SphereGeometry(0.17, 16, 12), mat('#f1c6a0')); head.position.set(0, 1.62, 0.12);
  const hat = new THREE.Mesh(new THREE.SphereGeometry(0.18, 16, 8, 0, Math.PI * 2, 0, Math.PI / 2), mat('#1f6fd1')); hat.position.set(0, 1.66, 0.12);
  const brim = new THREE.Mesh(new THREE.CylinderGeometry(0.12, 0.12, 0.02, 12), mat('#1f6fd1')); brim.position.set(0, 1.67, 0.27);
  g.add(legL, legR, body, head, hat, brim);
  // arms + club swing around the shoulders, in the plane facing the ball
  const pivot = new THREE.Group(); pivot.position.set(0, 1.38, 0.12);
  const arms = cap(0.06, 0.55, '#e85d3a'); arms.position.set(0, -0.35, 0.12); arms.rotation.x = 0.35;
  const shaft = new THREE.Mesh(new THREE.CylinderGeometry(0.012, 0.012, 1.05, 6), mat('#cfd3d8')); shaft.position.set(0, -1.0, 0.33); shaft.rotation.x = 0.35;
  const headC = new THREE.Mesh(new THREE.BoxGeometry(0.1, 0.06, 0.08), mat('#4a4f57')); headC.position.set(0, -1.48, 0.5);
  pivot.add(arms, shaft, headC); g.add(pivot);
  g.userData.pivot = pivot;
  return g;
}
const golfer = makeGolfer(); scene.add(golfer);

/* ---------------- round state ---------------- */
const SAVE = 'fairway_best';
let state = 'title', hi = 0, hole = null, ball = { x: 0, z: 0 }, strokes = 0, scores = [], clubI = 0, aim = 0, shot = null, plan = null, prevBall = null;
let swingAnim = 0, swingT = 0, introT = 0, resultT = 0, motionOn = false;
const camPos = new THREE.Vector3(), camLook = new THREE.Vector3();
const fullDist = {}; // full-swing total distance per club on flat ground, for auto club choice and the landing marker
{
  const t = FG.buildHole({ name: 't', par: 4, path: [[0, 0], [0, 600]], fw: 60, green: { r: 15, sx: 0, sz: 0 }, bunkers: [], water: [], hills: 0, seed: 1 }, 0);
  t.wind = { x: 0, z: 0, mph: 0 }; t.trees = [];
  for (const c of FG.CLUBS) { const b = FG.simulate(t, { x: 0, z: 0 }, c.id, 0, 1, 0); fullDist[c.id] = { total: b.z, carry: b.carry ? b.carry.z : b.z }; }
}
const club = () => FG.CLUBS[clubI];
const onGreen = () => FG.surfaceAt(hole, ball.x, ball.z) === 'green';
const toPin = () => FG.dist([ball.x, ball.z], hole.pin);

function startRound() { scores = []; hi = 0; startHole(); }
function startHole() {
  hole = FG.buildHole(FG.HOLES[hi], hi); buildCourse(hole);
  ball = { x: 0, z: 0 }; strokes = 0; shot = null; trail.visible = false;
  aim = Math.atan2(hole.path[1][0], hole.path[1][1]);
  autoClub(); state = 'intro'; introT = 0;
  callout(`Hole ${hi + 1}`, `Par ${hole.par} · ${Math.round(hole.len * FG.YD)} yds · ${hole.name}`, 3.2);
  showHud(false);
}
function autoClub() {
  if (onGreen() || (toPin() < 18 && FG.surfaceAt(hole, ball.x, ball.z) !== 'sand')) { clubI = FG.CLUBS.findIndex((c) => c.id === 'putter'); return; }
  if (FG.surfaceAt(hole, ball.x, ball.z) === 'sand' && toPin() < 90) { clubI = FG.CLUBS.findIndex((c) => c.id === 'sand'); return; }
  const d = toPin(), order = FG.CLUBS.filter((c) => c.id !== 'putter').slice().reverse();      // shortest first
  const pick = order.find((c) => fullDist[c.id].total >= d * 0.95) || FG.CLUBS[0];
  clubI = FG.CLUBS.indexOf(ball.x === 0 && ball.z === 0 && hole.par > 3 ? FG.CLUBS[0] : pick);
}
function readyShot() {
  state = 'aim'; showHud(true); autoClub();
  // aim at the pin, or along the fairway when the pin is out of reach
  const reach = fullDist[club().id].total;
  if (toPin() <= reach * 1.05 || onGreen()) aim = Math.atan2(hole.pin[0] - ball.x, hole.pin[1] - ball.z);
  else { const s = Math.min(hole.len, alongDist() + reach * 0.95), [px, pz] = FG.alongPath(hole.path, s); aim = Math.atan2(px - ball.x, pz - ball.z); }
  updateHud(); placeGolfer(); hint();
}
function alongDist() { // how far along the hole's centerline the ball is
  let best = 0, bd = Infinity, acc = 0; const P = hole.path;
  for (let i = 1; i < P.length; i++) {
    const ax = P[i - 1][0], az = P[i - 1][1], dx = P[i][0] - ax, dz = P[i][1] - az, L = Math.hypot(dx, dz);
    const t = Math.max(0, Math.min(1, ((ball.x - ax) * dx + (ball.z - az) * dz) / (L * L))), d = Math.hypot(ball.x - ax - dx * t, ball.z - az - dz * t);
    if (d < bd) { bd = d; best = acc + t * L; } acc += L;
  }
  return best;
}

/* ---------------- swing ---------------- */
function takeShot(power, side) {
  if (state !== 'aim') return;
  audio.unlock(); audio.whoosh();
  prevBall = { ...ball }; strokes++;
  const c = club().id;
  shot = FG.launch(hole, ball, c, aim, power, side);
  plan = FG.simulate(hole, ball, c, aim, power, side);   // same physics, run ahead: tells the camera where it lands
  state = 'swing'; swingT = 0; swingAnim = Math.min(1.1, power);
  setTimeout(() => audio.hit(power), 140);
  trail.visible = c !== 'putter'; trailPts = [];
  showHud(false); slopeDots.mesh.visible = false;
}
let trailPts = [];
function finishShot() {
  const b = shot;
  if (b.inWater) { audio.splash(); strokes++; ball = { ...prevBall }; callout('Water', '+1 stroke · play again from where you were', 2); state = 'result'; resultT = 0; return; }
  ball = { x: b.x, z: b.z };
  if (b.holed) {
    audio.cup(); const name = FG.scoreName(strokes, hole.par); if (strokes <= hole.par - 1) audio.cheer();
    scores[hi] = strokes; callout(name + '!', `${strokes} stroke${strokes > 1 ? 's' : ''} · par ${hole.par}`, 2.6); state = 'holed'; resultT = 0; return;
  }
  const s = FG.surfaceAt(hole, ball.x, ball.z), d = toPin();
  const where = FG.SURF[s].name, left = s === 'green' ? `${(d * FG.FT).toFixed(1)} ft to go` : `${Math.round(d * FG.YD)} yds to go`;
  callout(where, (b.hitTree ? 'Off the trees · ' : '') + left, 1.8, true);
  if (strokes >= hole.par + 5) { scores[hi] = strokes + 1; callout('Picked up', `Counts as ${strokes + 1}`, 2); state = 'holed'; resultT = 0; return; }
  state = 'result'; resultT = 0;
}

/* pointer: drag sideways to aim; drag down to pull back, flick up to swing */
let g = null;
const meterFill = $('meterFill'), meterMark = $('meterMark');
const H = () => innerHeight * 0.34;
canvas.addEventListener('pointerdown', (e) => {
  audio.unlock();
  if (state !== 'aim') return;
  canvas.setPointerCapture(e.pointerId);
  g = { id: e.pointerId, x0: e.clientX, y0: e.clientY, mode: 'pending', maxDy: 0, xAtMax: e.clientX, lastY: e.clientY, upStart: null, peakRate: 0, alphaAtPeak: 0 };
});
canvas.addEventListener('pointermove', (e) => {
  if (!g || e.pointerId !== g.id || state !== 'aim') return;
  const dx = e.clientX - g.x0, dy = e.clientY - g.y0;
  if (g.mode === 'pending') { if (Math.abs(dx) > 14 && Math.abs(dx) > Math.abs(dy)) g.mode = 'aim'; else if (dy > 10) g.mode = 'back'; }
  if (g.mode === 'aim') { aim += (e.movementX || 0) * (onGreen() ? 0.0012 : 0.003); updateHud(); placeGolfer(); return; }
  if (g.mode === 'back' && !motionOn) {
    if (dy >= g.maxDy) { g.maxDy = dy; g.xAtMax = e.clientX; g.upStart = null; }
    const p = Math.min(1.1, g.maxDy / H()); setMeter(p); swingAnim = -p;
    // flick up: the swing fires once the pointer comes back up past 40% of the backswing, quickly
    if (dy < g.maxDy - 8 && !g.upStart) g.upStart = performance.now();
    if (g.upStart && dy < g.maxDy * 0.4) {
      const quick = performance.now() - g.upStart < 450;
      const side = Math.max(-1, Math.min(1, (e.clientX - g.xAtMax) / Math.max(40, g.maxDy * 0.9)));
      const p2 = g.maxDy / H();
      g = null; setMeter(0);
      if (quick && p2 > 0.04) takeShot(Math.min(1.1, p2), side);
      else { swingAnim = 0; hintTemp('Flick up faster to swing'); }
    }
  }
});
canvas.addEventListener('pointerup', (e) => {
  if (!g || e.pointerId !== g.id) return;
  if (motionOn && g.mode !== 'aim' && g.peakRate > 250) takeShot(Math.min(1.1, g.peakRate / 1000), Math.max(-1, Math.min(1, g.alphaAtPeak / (g.peakRate || 1) * 3)));
  else if (g.mode === 'back') { setMeter(0); swingAnim = 0; hintTemp('Flick up to swing'); }
  g = null;
});
/* motion swing: hold the screen and swing the phone like a club */
let lastMotion = 0;
addEventListener('devicemotion', (e) => {
  if (!motionOn || !g || state !== 'aim' || !e.rotationRate) return;
  const r = e.rotationRate, rate = Math.hypot(r.alpha || 0, r.beta || 0, r.gamma || 0);
  if (rate > g.peakRate) { g.peakRate = rate; g.alphaAtPeak = r.alpha || 0; }
  setMeter(Math.min(1.1, g.peakRate / 1000));
  if (g.peakRate > 250 && rate < g.peakRate * 0.45 && performance.now() - lastMotion > 600) {   // swing finished
    lastMotion = performance.now(); const p = Math.min(1.1, g.peakRate / 1000), side = Math.max(-1, Math.min(1, g.alphaAtPeak / g.peakRate * 3));
    g = null; setMeter(0); takeShot(p, side);
  }
});
async function setMotion(on) {
  if (on && typeof DeviceMotionEvent !== 'undefined' && DeviceMotionEvent.requestPermission) {
    try { if (await DeviceMotionEvent.requestPermission() !== 'granted') on = false; } catch { on = false; }
  }
  motionOn = on; for (const id of ['motionBtn', 'mMotion']) $(id).textContent = 'Swing with phone: ' + (on ? 'on' : 'off'); hint();
}
if (isTouch && typeof DeviceMotionEvent !== 'undefined') { $('motionBtn').hidden = false; $('mMotion').hidden = false; }
/* keyboard: hold Space to pull back, release to swing; arrows aim; Q/E change club */
let keyBack = null;
addEventListener('keydown', (e) => {
  audio.unlock();
  if (e.code === 'Space') { e.preventDefault(); if (state === 'aim' && keyBack === null) keyBack = performance.now(); }
  if (state !== 'aim') return;
  if (e.code === 'ArrowLeft') { aim -= onGreen() ? 0.01 : 0.025; updateHud(); placeGolfer(); }
  if (e.code === 'ArrowRight') { aim += onGreen() ? 0.01 : 0.025; updateHud(); placeGolfer(); }
  if (e.code === 'KeyQ') changeClub(-1); if (e.code === 'KeyE') changeClub(1);
});
addEventListener('keyup', (e) => {
  if (e.code !== 'Space' || keyBack === null) return;
  const p = Math.min(1.1, (performance.now() - keyBack) / 1200); keyBack = null; setMeter(0);
  if (p > 0.03) takeShot(p, (Math.random() - 0.5) * (p > 1 ? 1.2 : 0.15));
});
function changeClub(d) { const n = FG.CLUBS.length; clubI = (clubI + d + n) % n; if (club().id === 'putter' && !onGreen() && toPin() > 40) clubI = (clubI + d + n) % n; updateHud(); }
$('clubPrev').onclick = () => changeClub(-1); $('clubNext').onclick = () => changeClub(1); $('clubName').onclick = () => changeClub(1);
let aimHold = null;
for (const [id, d] of [['aimL', -1], ['aimR', 1]]) {
  const b = $(id);
  b.onpointerdown = () => { if (state !== 'aim') return; const f = () => { aim += d * (onGreen() ? 0.006 : 0.015); updateHud(); placeGolfer(); aimHold = setTimeout(f, 60); }; f(); };
  b.onpointerup = b.onpointerleave = () => clearTimeout(aimHold);
}
let peek = false;
$('viewBtn').onclick = () => { if (state === 'aim') peek = !peek; };

/* ---------------- HUD ---------------- */
function setMeter(p) { meterFill.style.height = `${Math.min(1.1, p) / 1.1 * 100}%`; meterFill.classList.toggle('over', p > 1); }
meterMark.style.bottom = `${1 / 1.1 * 100}%`;
function showHud(on) { for (const id of ['meter', 'minimap', 'bottomLeft', 'bottomRight', 'hint']) $(id).hidden = !on; for (const id of ['topLeft', 'topRight']) $(id).hidden = state === 'title'; }
function hint() { $('hint').textContent = motionOn ? 'Hold the screen and swing your phone like a club' : isTouch ? 'Drag down to pull back · flick up to swing' : 'Drag down, flick up — or hold Space'; }
let hintTimer = null;
function hintTemp(t) { $('hint').textContent = t; clearTimeout(hintTimer); hintTimer = setTimeout(hint, 1600); }
function updateHud() {
  if (!hole) return;
  $('holeTxt').textContent = `Hole ${hi + 1}`; $('subTxt').textContent = `Par ${hole.par} · Stroke ${strokes + 1}`;
  const d = toPin(); $('left').textContent = onGreen() ? `${(d * FG.FT).toFixed(1)} ft left` : `${Math.round(d * FG.YD)} yds left`;
  $('windTxt').textContent = `${hole.wind.mph} mph`;
  $('windArrow').style.transform = `rotate(${(Math.atan2(hole.wind.x, hole.wind.z) - aim) * 180 / Math.PI}deg)`;
  $('windArrow').style.opacity = hole.wind.mph ? 1 : 0.3;
  $('clubName').textContent = club().name;
  drawMinimap(); updateAimLine();
}
function landingPreview() { // where a full, straight swing with this club would land, with the wind
  if (club().id === 'putter') { const d = Math.min(toPin() * 1.15, 25); return [ball.x + Math.sin(aim) * d, ball.z + Math.cos(aim) * d]; }
  const b = FG.simulate(hole, ball, club().id, aim, 1, 0); return b.carry ? [b.carry.x, b.carry.z] : [b.x, b.z];
}
function updateAimLine() {
  if (state !== 'aim') { aimDots.visible = landMark.visible = false; return; }
  const [lx, lz] = landingPreview(), pts = [], n = club().id === 'putter' ? 14 : 22;
  for (let i = 2; i <= n; i++) { const u = i / n, x = ball.x + (lx - ball.x) * u, z = ball.z + (lz - ball.z) * u; pts.push(x, FG.heightAt(hole, x, z) + 0.25, z); }
  aimDots.geometry.setAttribute('position', new THREE.Float32BufferAttribute(pts, 3)); aimDots.geometry.computeBoundingSphere(); aimDots.visible = true;
  landMark.visible = club().id !== 'putter'; landMark.position.set(lx, FG.heightAt(hole, lx, lz) + 1.6, lz);
  aimDots.material.size = club().id === 'putter' ? 0.1 : 0.4;
}
const mm = $('minimap'), mx = mm.getContext('2d');
function drawMinimap() {
  const W = mm.width, Hh = mm.height; mx.clearRect(0, 0, W, Hh);
  const putting = onGreen();
  const g0 = hole.green, rot = putting ? -aim : -Math.atan2(g0.x, g0.z);   // tee→green (or the putt line) points up
  let pts = hole.path.flatMap((p) => [p]).concat([[hole.pin[0], hole.pin[1]], [ball.x, ball.z]]);
  if (putting) pts = [[g0.x - g0.r, g0.z - g0.r], [g0.x + g0.r, g0.z + g0.r], [ball.x, ball.z]];
  const R = (x, z) => [x * Math.cos(rot) - z * Math.sin(rot), x * Math.sin(rot) + z * Math.cos(rot)];
  const rp = pts.map((p) => R(p[0], p[1])); const xs = rp.map((p) => p[0]), zs = rp.map((p) => p[1]);
  const pad = putting ? 4 : hole.fw + 14, minX = Math.min(...xs) - pad, maxX = Math.max(...xs) + pad, minZ = Math.min(...zs) - pad, maxZ = Math.max(...zs) + pad;
  const s = Math.min(W / (maxX - minX), Hh / (maxZ - minZ));
  const cx = (minX + maxX) / 2, cz = (minZ + maxZ) / 2;
  const P = (x, z) => { const [a, b] = R(x, z); return [W / 2 + (a - cx) * s, Hh / 2 - (b - cz) * s]; };
  mx.fillStyle = '#2f7d2b'; mx.fillRect(0, 0, W, Hh);
  // fairway
  mx.strokeStyle = '#5cbf4a'; mx.lineWidth = hole.fw * 2 * s; mx.lineCap = 'round'; mx.lineJoin = 'round'; mx.beginPath();
  hole.path.forEach((p, i) => { const q = P(p[0], p[1]); i ? mx.lineTo(...q) : mx.moveTo(...q); }); mx.stroke();
  const ell = (e, c) => { const [x, y] = P(e[0], e[1]); mx.fillStyle = c; mx.beginPath(); mx.ellipse(x, y, e[2] * s, e[3] * s, -rot, 0, 7); mx.fill(); };
  for (const w of hole.water) ell(w, '#3f8fbf');
  const [gx, gy] = P(g0.x, g0.z); mx.fillStyle = '#7fe066'; mx.beginPath(); mx.arc(gx, gy, g0.r * s, 0, 7); mx.fill();
  for (const b of hole.bunkers) ell(b, '#ead9a0');
  mx.fillStyle = '#1f5a28'; for (const t of hole.trees) { const [x, y] = P(t.x, t.z); mx.beginPath(); mx.arc(x, y, Math.max(1.5, t.r * s), 0, 7); mx.fill(); }
  if (putting) { // slope arrows on the green
    mx.strokeStyle = 'rgba(255,255,255,.75)'; mx.lineWidth = 1.5;
    for (let x = -g0.r; x <= g0.r; x += g0.r / 3) for (let z = -g0.r; z <= g0.r; z += g0.r / 3) {
      if (Math.hypot(x, z) > g0.r * 0.9) continue; const n = FG.normalAt(hole, g0.x + x, g0.z + z), k = 160;
      const [a, b] = P(g0.x + x, g0.z + z), [c, d] = P(g0.x + x + n[0] * k * 0.1, g0.z + z + n[2] * k * 0.1); mx.beginPath(); mx.moveTo(a, b); mx.lineTo(c, d); mx.stroke();
    }
  }
  // pin, aim line, landing diamond, ball
  const [px, py] = P(hole.pin[0], hole.pin[1]); mx.fillStyle = '#e8402f'; mx.fillRect(px - 1, py - 14, 2, 14); mx.beginPath(); mx.moveTo(px, py - 14); mx.lineTo(px + 9, py - 10); mx.lineTo(px, py - 7); mx.fill();
  if (state === 'aim') {
    const [lx, lz] = landingPreview(), [bx, by] = P(ball.x, ball.z), [ex, ey] = P(lx, lz);
    mx.strokeStyle = '#4fc3ff'; mx.lineWidth = 3; mx.setLineDash([6, 6]); mx.beginPath(); mx.moveTo(bx, by); mx.lineTo(ex, ey); mx.stroke(); mx.setLineDash([]);
    if (club().id !== 'putter') { mx.fillStyle = '#ffd23a'; mx.beginPath(); mx.moveTo(ex, ey - 7); mx.lineTo(ex + 6, ey); mx.lineTo(ex, ey + 7); mx.lineTo(ex - 6, ey); mx.fill(); }
  }
  const [bx, by] = P(ball.x, ball.z); mx.fillStyle = '#fff'; mx.strokeStyle = '#000'; mx.lineWidth = 1.5; mx.beginPath(); mx.arc(bx, by, 4.5, 0, 7); mx.fill(); mx.stroke();
}

/* ---------------- camera, golfer, animation ---------------- */
function placeGolfer() {
  const fx = Math.sin(aim), fz = Math.cos(aim), rx = Math.cos(aim), rz = -Math.sin(aim);
  const gx = ball.x + rx * 0.85, gz = ball.z + rz * 0.85;   // right-handed: target on the golfer's left
  golfer.position.set(gx, FG.heightAt(hole, gx, gz), gz);
  golfer.rotation.y = Math.atan2(-rx, -rz);  // face the ball
  golfer.visible = true;
  const by = FG.heightAt(hole, ball.x, ball.z);
  ballMesh.position.set(ball.x, by + 0.11, ball.z); shadow.position.set(ball.x, by + 0.02, ball.z);
}
function callout(big, small, secs, quiet) {
  const el = $('callout'); el.innerHTML = ''; el.append(big); if (small) { const s = document.createElement('small'); s.textContent = small; el.append(s); }
  el.style.fontSize = quiet ? 'clamp(26px, 4.5vw, 40px)' : ''; el.hidden = false; el.style.opacity = 1;
  clearTimeout(callout.t); callout.t = setTimeout(() => { el.style.opacity = 0; setTimeout(() => (el.hidden = true), 300); }, secs * 1000);
}
const v3 = (x, y, z) => new THREE.Vector3(x, y, z);
function desiredCamera(dt) {
  const fx = Math.sin(aim), fz = Math.cos(aim);
  if (state === 'intro') {
    // fly from above the green back to the tee
    const t = Math.min(1, introT / 3), e = t * t * (3 - 2 * t), gG = hole.green, gy = FG.heightAt(hole, gG.x, gG.z);
    const from = v3(gG.x - fx * 20, gy + 45, gG.z - fz * 60), to = v3(ball.x - fx * 7, FG.heightAt(hole, 0, 0) + 2.6, ball.z - fz * 7);
    return [from.lerp(to, e), v3(gG.x, gy, gG.z).lerp(v3(ball.x + fx * 40, 0, ball.z + fz * 40), e)];
  }
  const by = FG.heightAt(hole, ball.x, ball.z);
  if (state === 'aim' || state === 'result') {
    if (peek && state === 'aim') { const [lx, lz] = landingPreview(); const ly = FG.heightAt(hole, lx, lz); return [v3(lx - fx * 30, ly + 22, lz - fz * 30), v3(lx, ly, lz)]; }
    const putt = club().id === 'putter';
    const back = putt ? 3.6 : 6.5, up = putt ? 1.4 : 2.5;
    const rx = Math.cos(aim), rz = -Math.sin(aim), side = putt ? 1.0 : 1.8;
    return [v3(ball.x - fx * back - rx * side, by + up, ball.z - fz * back - rz * side), v3(ball.x + fx * (putt ? 6 : 30), by + (putt ? 0 : 2), ball.z + fz * (putt ? 6 : 30))];
  }
  if (state === 'swing' || state === 'flight' || state === 'holed') {
    const b = shot; if (!b) return [camPos.clone(), camLook.clone()];
    if (b.putt) return [v3(b.start[0] - fx * 3.6, FG.heightAt(hole, b.start[0], b.start[1]) + 1.6, b.start[1] - fz * 3.6), v3(b.x, b.y, b.z)];
    const land = plan.carry || { x: plan.x, z: plan.z }, ly = FG.heightAt(hole, land.x, land.z);
    const dl = Math.hypot(b.x - land.x, b.z - land.z), descending = b.air && b.vy < 0;
    if ((descending && dl < 70) || !b.air) {
      // cut to a spot past the landing, looking back at the ball coming in
      const hx = land.x - b.start[0], hz = land.z - b.start[1], hl = Math.hypot(hx, hz) || 1;
      return [v3(land.x + hx / hl * 18 + hz / hl * 8, ly + 5, land.z + hz / hl * 18 - hx / hl * 8), v3(b.x, b.y, b.z)];
    }
    const vl = Math.hypot(b.vx, b.vz) || 1;
    return [v3(b.x - b.vx / vl * 8, b.y + 2.5, b.z - b.vz / vl * 8), v3(b.x + b.vx / vl * 6, b.y, b.z + b.vz / vl * 6)];
  }
  return [camPos.clone(), camLook.clone()];
}
let last = performance.now(), dotT = 0;
function frame(now) {
  const dt = Math.min(0.05, (now - last) / 1000); last = now;
  if (hole) {
    if (state === 'intro') { introT += dt; if (introT > 3.3) readyShot(); }
    if (state === 'swing') {
      swingT += dt;
      if (swingT > 0.14) { state = 'flight'; }
    }
    if (state === 'flight' || state === 'holed') {
      if (shot && !shot.done) {
        FG.stepBall(hole, shot, dt);
        ballMesh.position.set(shot.x, shot.y + 0.11, shot.z);
        const gy = FG.heightAt(hole, shot.x, shot.z); shadow.position.set(shot.x, gy + 0.02, shot.z);
        if (trail.visible && shot.air) { trailPts.push(shot.x, shot.y, shot.z); if (trailPts.length > 900) trailPts.splice(0, 3); trail.geometry.setAttribute('position', new THREE.Float32BufferAttribute(trailPts, 3)); }
        if (shot.done) finishShot();
      }
    }
    if (state === 'result') { resultT += dt; if (resultT > 1.9) { prevBall = null; readyShot(); } }
    if (state === 'holed') { resultT += dt; if (resultT > 2.8) { state = 'card'; showCard(); } }
    // swing animation: back with the meter, then through
    const pv = golfer.userData.pivot;
    if (state === 'swing' || state === 'flight') pv.rotation.z += ((Math.min(1.1, swingAnim) * 2.4) - pv.rotation.z) * Math.min(1, dt * 22);
    else pv.rotation.z += (-(Math.max(0, -swingAnim)) * 2.4 - pv.rotation.z) * Math.min(1, dt * 18);
    if (keyBack !== null && state === 'aim') { const p = Math.min(1.1, (now - keyBack) / 1200); setMeter(p); swingAnim = -p; }
    if (flag) flag.rotation.y = Math.sin(now / 400) * 0.25 + Math.atan2(-hole.wind.z, hole.wind.x) * 0.2;
    // slope dots drift downhill while putting
    if (slopeDots) {
      const show = (state === 'aim' && onGreen()); slopeDots.mesh.visible = show;
      if (show) {
        dotT += dt; const arr = slopeDots.mesh.geometry.attributes.position.array;
        slopeDots.base.forEach(([x, z, ph], i) => { const n = FG.normalAt(hole, x, z), k = ((dotT * 0.6 + ph) % 1) * 1.1; const px = x + n[0] * 30 * k, pz = z + n[2] * 30 * k; arr[i * 3] = px; arr[i * 3 + 1] = FG.heightAt(hole, px, pz) + 0.04; arr[i * 3 + 2] = pz; });
        slopeDots.mesh.geometry.attributes.position.needsUpdate = true;
      }
    }
    const [p, l] = desiredCamera(dt), k = state === 'intro' ? 1 : 1 - Math.exp(-dt * (state === 'flight' ? 9 : 5));
    camPos.lerp(p, k); camLook.lerp(l, k); camera.position.copy(camPos); camera.lookAt(camLook);
    // keep the ball readable at a distance (drawn a little larger the farther it is)
    ballMesh.scale.setScalar(Math.max(1, camera.position.distanceTo(ballMesh.position) / 9));
  } else { // title: slow orbit over hole 1
    const t = now / 9000; camera.position.set(Math.sin(t) * 140, 60, 180 + Math.cos(t) * 140); camera.lookAt(0, 0, 180);
  }
  renderer.render(scene, camera);
  requestAnimationFrame(frame);
}

/* ---------------- screens ---------------- */
function showCard(final) {
  final = final || (state === 'card' && hi === FG.HOLES.length - 1 && scores[hi] !== undefined);
  const T = $('sc'), pars = FG.HOLES.map((h) => h.par);
  const tot = scores.reduce((a, b) => a + (b || 0), 0), parPlayed = scores.reduce((a, b, i) => a + (b ? pars[i] : 0), 0), diff = tot - parPlayed;
  T.innerHTML = '<tr><th>Hole</th>' + pars.map((_, i) => `<th>${i + 1}</th>`).join('') + '<th>Tot</th></tr>' +
    '<tr><th>Par</th>' + pars.map((p) => `<td>${p}</td>`).join('') + `<td>${pars.reduce((a, b) => a + b)}</td></tr>` +
    '<tr><th>You</th>' + pars.map((p, i) => scores[i] ? `<td class="${scores[i] < p ? 'd' : scores[i] > p ? 'u' : ''}">${scores[i]}</td>` : '<td></td>').join('') + `<td>${tot || ''}</td></tr>`;
  $('cardTitle').textContent = final ? `Round over: ${tot} (${diff === 0 ? 'even' : diff > 0 ? '+' + diff : diff})` : `Scorecard · ${diff === 0 ? 'Even' : diff > 0 ? '+' + diff : diff}`;
  $('cardNext').textContent = final ? 'Play again' : state === 'card' ? 'Next hole' : 'Back';
  if (final) { try { const best = +localStorage.getItem(SAVE) || 0; if (!best || tot < best) localStorage.setItem(SAVE, tot); } catch {} }
  $('cardScreen').hidden = false; showHud(false);
}
$('cardNext').onclick = () => {
  $('cardScreen').hidden = true;
  if (state === 'card') { if (hi < FG.HOLES.length - 1) { hi++; startHole(); } else startRound(); }
  else if (state === 'aim') showHud(true);
};
$('cardMenu').onclick = () => { $('cardScreen').hidden = true; toTitle(); };
$('playBtn').onclick = () => { audio.unlock(); $('title').hidden = true; startRound(); };
$('motionBtn').onclick = () => setMotion(!motionOn); $('mMotion').onclick = () => setMotion(!motionOn);
$('menuBtn').onclick = () => { $('menu').hidden = false; };
$('mResume').onclick = () => { $('menu').hidden = true; };
$('mCard').onclick = () => { $('menu').hidden = true; showCard(); };
$('mSound').onclick = () => { audio.setMuted(!audio.muted); $('mSound').textContent = 'Sound: ' + (audio.muted ? 'off' : 'on'); };
$('mSound').textContent = 'Sound: ' + (audio.muted ? 'off' : 'on');
$('mQuit').onclick = () => { $('menu').hidden = true; toTitle(); };
function toTitle() { state = 'title'; hole = null; showHud(false); $('topLeft').hidden = $('topRight').hidden = true; $('title').hidden = false; const h1 = FG.buildHole(FG.HOLES[0], 0); buildCourse(h1); golfer.visible = false; aimDots.visible = landMark.visible = false; ballMesh.position.set(0, -50, 0); shadow.position.set(0, -50, 0); }
try { const best = +localStorage.getItem(SAVE); if (best) $('best').textContent = `Best round: ${best}`; } catch {}
toTitle(); hint();
requestAnimationFrame(frame);
window.__fg = { get cam() { return [camera.position.x, camera.position.y, camera.position.z]; }, get state() { return state; }, get shot() { return shot; }, get ball() { return ball; }, get strokes() { return strokes; }, takeShot, get hole() { return hole; }, place: (x, z) => { ball = { x, z }; readyShot(); }, setAim: (a) => { aim = a; updateHud(); } };
