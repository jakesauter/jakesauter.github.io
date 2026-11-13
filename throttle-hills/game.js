/* Throttle Hills: drawing, controls, menus, sound. Physics and levels are in core.js (window.TH). */
(function () {
  'use strict';
  const TH = window.TH, cv = document.getElementById('game'), cx = cv.getContext('2d');
  const $ = s => document.querySelector(s);
  const THEMES = {
    dunes:  { sky: ['#ffb46b', '#ffe7c2'], sun: '#fff3c4', far: '#eab27a', near: '#d9925a', ground: '#c47a3e', edge: '#8a4c22', stripe: '#b56d35', prop: 'cactus' },
    forest: { sky: ['#7cc6f2', '#e3f5ff'], sun: '#fffbe0', far: '#8fc48a', near: '#5f9e5a', ground: '#6e4a2c', edge: '#3c9a3a', stripe: '#5f3f25', prop: 'pine' },
    canyon: { sky: ['#f28a5b', '#ffd2a1'], sun: '#ffe9c2', far: '#d4704c', near: '#b4533a', ground: '#9a3e27', edge: '#62241a', stripe: '#87351f', prop: 'rock' },
    night:  { sky: ['#0a1330', '#2a3b70'], sun: '#e9efff', far: '#1d2a55', near: '#162043', ground: '#252a40', edge: '#56d6ff', stripe: '#1e2236', prop: 'lamp', stars: true },
  };
  const SAVE_KEY = 'throttlehills_v1';
  let save = { unlocked: 1, stars: {}, best: {} };
  try { Object.assign(save, JSON.parse(localStorage.getItem(SAVE_KEY) || '{}')); } catch (e) {}
  const persist = () => { try { localStorage.setItem(SAVE_KEY, JSON.stringify(save)); } catch (e) {} };

  let W = 0, H = 0, S = 40, dpr = 1;
  function resize() {
    dpr = Math.min(2, window.devicePixelRatio || 1); W = innerWidth; H = innerHeight;
    cv.width = W * dpr; cv.height = H * dpr; cv.style.width = W + 'px'; cv.style.height = H + 'px';
    S = Math.max(26, Math.min(64, Math.min(H / 9.5, W / 14)));
    $('#rotate').hidden = !(matchMedia('(pointer:coarse)').matches && H > W * 1.15) || rotateDismissed;
  }
  let rotateDismissed = false;
  addEventListener('resize', resize);

  /* ---------- input ---------- */
  const keys = {}, touch = { gas: 0, brake: 0, back: 0, fwd: 0 };
  addEventListener('keydown', e => {
    keys[e.code] = true;
    if (['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight', 'Space'].includes(e.code)) e.preventDefault();
    if (e.code === 'KeyR' && state === 'play') restart();
    if ((e.code === 'Escape' || e.code === 'KeyP') && (state === 'play' || state === 'paused')) togglePause();
    if (e.code === 'Enter' && state === 'done') next();
    audio.unlock();
  });
  addEventListener('keyup', e => { keys[e.code] = false; });
  const pointers = new Map();
  function pad(e) { const el = document.elementFromPoint(e.clientX, e.clientY); return el && el.closest('[data-pad]') ? el.closest('[data-pad]').dataset.pad : null; }
  function syncTouch() { for (const k in touch) touch[k] = 0; for (const p of pointers.values()) if (p) touch[p] = 1;
    document.querySelectorAll('[data-pad]').forEach(b => b.classList.toggle('on', !!touch[b.dataset.pad])); }
  const ctl = $('#controls');
  ctl.addEventListener('pointerdown', e => { e.preventDefault(); audio.unlock(); pointers.set(e.pointerId, pad(e)); syncTouch(); });
  ctl.addEventListener('pointermove', e => { if (pointers.has(e.pointerId)) { pointers.set(e.pointerId, pad(e)); syncTouch(); } });
  ['pointerup', 'pointercancel', 'pointerleave'].forEach(t => ctl.addEventListener(t, e => { pointers.delete(e.pointerId); syncTouch(); }));
  ctl.addEventListener('contextmenu', e => e.preventDefault());
  function input() {
    const gas = keys.ArrowUp || keys.KeyW || touch.gas, brake = keys.ArrowDown || keys.KeyS || touch.brake;
    const back = keys.ArrowLeft || keys.KeyA || touch.back, fwd = keys.ArrowRight || keys.KeyD || touch.fwd;
    return { gas: !!gas, brake: !!brake, lean: (back ? 1 : 0) - (fwd ? 1 : 0) };
  }

  /* ---------- sound (synthesized, no files) ---------- */
  const audio = {
    ctx: null, muted: (() => { try { return localStorage.getItem('th_muted') === '1'; } catch (e) { return false; } })(),
    unlock() {
      if (this.ctx || this.muted) { if (this.ctx && this.ctx.state === 'suspended') this.ctx.resume(); return; }
      try {
        const C = new (window.AudioContext || window.webkitAudioContext)(); this.ctx = C;
        this.master = C.createGain(); this.master.gain.value = 0.5; this.master.connect(C.destination);
        this.osc = C.createOscillator(); this.osc.type = 'sawtooth'; this.osc2 = C.createOscillator(); this.osc2.type = 'square';
        this.lp = C.createBiquadFilter(); this.lp.type = 'lowpass'; this.lp.frequency.value = 700;
        this.eg = C.createGain(); this.eg.gain.value = 0;
        this.osc.connect(this.lp); this.osc2.connect(this.lp); this.lp.connect(this.eg); this.eg.connect(this.master);
        this.osc.start(); this.osc2.start();
      } catch (e) { this.ctx = null; }
    },
    engine(rpm, on) {
      if (!this.ctx) return; const t = this.ctx.currentTime, f = 38 + rpm * 2.6;
      this.osc.frequency.setTargetAtTime(f, t, 0.05); this.osc2.frequency.setTargetAtTime(f * 0.5, t, 0.05);
      this.eg.gain.setTargetAtTime(this.muted ? 0 : on ? 0.11 : 0.045, t, 0.08);
    },
    blip(freq, dur, type = 'square', vol = 0.18, slide = 1) {
      if (!this.ctx || this.muted) return; const C = this.ctx, o = C.createOscillator(), g = C.createGain(), t = C.currentTime;
      o.type = type; o.frequency.setValueAtTime(freq, t); o.frequency.exponentialRampToValueAtTime(freq * slide, t + dur);
      g.gain.setValueAtTime(vol, t); g.gain.exponentialRampToValueAtTime(0.001, t + dur); o.connect(g); g.connect(this.master); o.start(t); o.stop(t + dur);
    },
    crash() { if (!this.ctx || this.muted) return; const C = this.ctx, n = C.sampleRate * 0.35, buf = C.createBuffer(1, n, C.sampleRate), d = buf.getChannelData(0);
      for (let i = 0; i < n; i++) d[i] = (Math.random() * 2 - 1) * Math.pow(1 - i / n, 2);
      const s = C.createBufferSource(), g = C.createGain(); s.buffer = buf; g.gain.value = 0.5; s.connect(g); g.connect(this.master); s.start(); },
    setMuted(m) { this.muted = m; try { localStorage.setItem('th_muted', m ? '1' : '0'); } catch (e) {} if (m && this.ctx) this.eg.gain.value = 0; if (!m) this.unlock(); },
  };

  /* ---------- game state ---------- */
  let state = 'title', li = 0, lv = null, bike = null, cp = 2, cpIdx = -1, cam = { x: 0, y: 0 }, shake = 0, crashAt = 0, deaths = 0;
  let parts = [], toasts = [];

  function startLevel(i) {
    li = i; lv = TH.buildLevel(TH.LEVELS[i]); cp = 2; cpIdx = -1; deaths = 0; bike = TH.spawn(lv, cp); parts = []; toasts = [];
    const [x, y] = TH.comOf(bike); cam = { x, y };
    $('#lvname').textContent = `${i + 1}. ${TH.LEVELS[i].name}`;
    show('play');
  }
  function restart() { startLevel(li); }
  function respawn() { const t = bike.t, f = bike.flips, bo = bike.bonus; bike = TH.spawn(lv, cp); bike.t = t; bike.flips = f; bike.bonus = bo; bike.started = true; state = 'play'; }
  function togglePause() { show(state === 'paused' ? 'play' : 'paused'); }
  function next() { const go = () => li + 1 < TH.LEVELS.length ? startLevel(li + 1) : show('levels');
    if (window.THAds && (li + 1) % 3 === 0) window.THAds.between(go); else go(); }

  function show(s) {
    state = s;
    for (const id of ['title', 'levels', 'paused', 'done']) $('#' + id).hidden = s !== id;
    $('#hud').hidden = !(s === 'play' || s === 'paused');
    $('#controls').hidden = s !== 'play';
    $('#mute').hidden = s === 'play' || s === 'done';
    if (s === 'levels') paintLevels();
    if (s !== 'play') audio.engine(0, false);
  }

  function paintLevels() {
    const g = $('#grid'); g.innerHTML = '';
    TH.LEVELS.forEach((L, i) => {
      const b = document.createElement('button'), locked = i + 1 > save.unlocked, st = save.stars[i] || 0;
      b.className = 'lvl ' + L.theme + (locked ? ' locked' : ''); b.disabled = locked;
      b.innerHTML = `<span class="n">${locked ? '🔒' : i + 1}</span><span class="t">${L.name}</span><span class="s">${'★'.repeat(st)}<i>${'★'.repeat(3 - st)}</i></span>` +
        (save.best[i] ? `<span class="b">${save.best[i].toFixed(2)}s</span>` : '');
      b.onclick = () => { audio.unlock(); startLevel(i); };
      g.appendChild(b);
    });
    const total = Object.values(save.stars).reduce((a, b) => a + b, 0);
    $('#total').textContent = `★ ${total} / ${TH.LEVELS.length * 3}`;
  }

  function finish() {
    const t = TH.score(lv, bike), st = TH.stars(lv, t), prevBest = save.best[li];
    const isBest = !prevBest || t < prevBest;
    if (isBest) save.best[li] = t;
    save.stars[li] = Math.max(save.stars[li] || 0, st);
    save.unlocked = Math.max(save.unlocked, Math.min(TH.LEVELS.length, li + 2)); persist();
    $('#d-time').textContent = t.toFixed(2) + 's';
    $('#d-best').textContent = isBest ? (prevBest ? 'New best!' : 'First finish!') : `Best ${prevBest.toFixed(2)}s`;
    $('#d-flips').textContent = bike.flips ? `${bike.flips} flip${bike.flips > 1 ? 's' : ''} (−${bike.bonus}s)` : 'Tip: flips take a second off your time';
    $('#d-par').textContent = st < 3 ? `${(st === 2 ? lv.par : lv.par * 1.3).toFixed(1)}s for ${st + 1} stars` : 'Perfect run';
    $('#d-crashes').textContent = deaths ? `${deaths} crash${deaths > 1 ? 'es' : ''}` : 'No crashes';
    const stars = $('#d-stars'); stars.innerHTML = '';
    for (let k = 0; k < 3; k++) { const s = document.createElement('span'); s.textContent = '★'; if (k < st) { s.className = 'got'; s.style.animationDelay = (0.25 + k * 0.25) + 's'; setTimeout(() => audio.blip(660 + k * 220, 0.18, 'triangle', 0.2), 250 + k * 250); } stars.appendChild(s); }
    $('#d-next').hidden = li + 1 >= TH.LEVELS.length;
    $('#d-title').textContent = li + 1 >= TH.LEVELS.length ? 'You beat every track!' : 'Finished!';
    show('done');
  }

  function toast(text, big) { toasts.push({ text, t: 0, big }); }

  /* ---------- update ---------- */
  function update(dt) {
    if (state !== 'play') return;
    const inp = input();
    TH.step(lv, bike, inp, dt);
    const [bx, by] = TH.comOf(bike);
    for (let k = 0; k < lv.cps.length; k++) if (k > cpIdx && bx > lv.cps[k] + 1) { cpIdx = k; cp = lv.cps[k]; toast('Checkpoint'); audio.blip(880, 0.12, 'triangle', 0.15, 1.5); }
    for (const ev of bike.events.splice(0)) {
      if (ev[0] === 'crash' || ev[0] === 'fell') { deaths++; crashAt = performance.now(); shake = 0.5; audio.crash(); burst(bike.p[3].x, bike.p[3].y, 18, '#fff'); }
      if (ev[0] === 'backflip' || ev[0] === 'frontflip') { toast(`${ev[1] > 1 ? ev[1] + '× ' : ''}${ev[0] === 'backflip' ? 'BACKFLIP' : 'FRONTFLIP'}!  −${ev[1]}s`, true); audio.blip(523, 0.12, 'square', 0.15, 2); }
      if (ev[0] === 'finish') { audio.blip(523, 0.1, 'square', 0.18, 1); setTimeout(() => audio.blip(784, 0.25, 'square', 0.18, 1), 110); setTimeout(finish, 700); }
    }
    if (bike.crashed && performance.now() - crashAt > 900) respawn();
    // dust from the rear tire
    if (inp.gas && bike.ground[0] > 0 && Math.random() < 0.6) { const q = bike.p[0]; parts.push({ x: q.x - 0.2, y: q.y - TH.WR + 0.05, vx: -1 - Math.random() * 2, vy: 0.5 + Math.random() * 1.5, life: 0.6, r: 0.08 + Math.random() * 0.12, c: THEMES[lv.theme].edge }); }
    for (const p of parts) { p.x += p.vx * dt; p.y += p.vy * dt; p.vy -= 3 * dt; p.life -= dt; }
    parts = parts.filter(p => p.life > 0);
    const vx = (bike.p[0].vx + bike.p[1].vx) / 2;
    cam.x += (bx + Math.max(-2, Math.min(4, vx * 0.25)) - cam.x) * Math.min(1, dt * 5);
    cam.y += (by - cam.y) * Math.min(1, dt * 4);
    shake = Math.max(0, shake - dt);
    audio.engine(Math.abs(bike.w[0]), inp.gas && !bike.crashed);
    $('#time').textContent = Math.max(0, bike.t - bike.bonus).toFixed(2);
    for (const t of toasts) t.t += dt; toasts = toasts.filter(t => t.t < 1.4);
  }
  function burst(x, y, n, c) { for (let i = 0; i < n; i++) { const a = Math.random() * 6.28, v = 1 + Math.random() * 4; parts.push({ x, y, vx: Math.cos(a) * v, vy: Math.sin(a) * v, life: 0.7, r: 0.06 + Math.random() * 0.08, c }); } }

  /* ---------- drawing ---------- */
  const X = x => (x - cam.x) * S + W * 0.38 + (shake ? (Math.random() - 0.5) * shake * 16 : 0);
  const Y = y => H * 0.58 - (y - cam.y) * S;
  function hash(n) { const s = Math.sin(n * 127.1) * 43758.5453; return s - Math.floor(s); }

  function drawBackground(th) {
    const g = cx.createLinearGradient(0, 0, 0, H); g.addColorStop(0, th.sky[0]); g.addColorStop(1, th.sky[1]);
    cx.fillStyle = g; cx.fillRect(0, 0, W, H);
    if (th.stars) { cx.fillStyle = '#fff'; for (let i = 0; i < 70; i++) { const sx = ((hash(i) * W * 3 - cam.x * S * 0.02) % W + W) % W; cx.globalAlpha = 0.3 + hash(i + 9) * 0.7; cx.fillRect(sx, hash(i + 3) * H * 0.6, 2, 2); } cx.globalAlpha = 1; }
    cx.fillStyle = th.sun; cx.beginPath(); cx.arc(W * 0.78, H * 0.2, Math.min(W, H) * 0.08, 0, 7); cx.fill();
    for (const [col, par, amp, base] of [[th.far, 0.12, 0.13, 0.55], [th.near, 0.25, 0.1, 0.68]]) {
      cx.fillStyle = col; cx.beginPath(); cx.moveTo(0, H);
      for (let sx = 0; sx <= W + 10; sx += 10) { const wx = (sx + cam.x * S * par) / 90; cx.lineTo(sx, H * base - H * amp * (Math.sin(wx * 0.7) * 0.6 + Math.sin(wx * 1.9 + 1) * 0.3 + Math.sin(wx * 0.23) * 0.5) - (cam.y * S * par * 0.3)); }
      cx.lineTo(W, H); cx.fill();
    }
  }

  function drawProps(th) {
    const x0 = Math.floor((cam.x - W / S) / 7), x1 = Math.ceil((cam.x + W / S) / 7);
    for (let k = x0; k <= x1; k++) {
      if (hash(k + li * 31) < 0.45) continue;
      const wx = k * 7 + hash(k) * 4, gy = TH.groundY(lv, wx); if (gy === null) continue;
      const sx = X(wx), sy = Y(gy), sc = S * (0.8 + hash(k + 5) * 0.5);
      cx.save(); cx.translate(sx, sy); cx.globalAlpha = 0.9;
      if (th.prop === 'cactus') { cx.fillStyle = '#5e8a3a'; rr(-0.12 * sc, -1.6 * sc, 0.24 * sc, 1.6 * sc, 0.12 * sc); rr(-0.5 * sc, -1.1 * sc, 0.18 * sc, 0.6 * sc, 0.09 * sc); rr(-0.5 * sc, -0.6 * sc, 0.4 * sc, 0.16 * sc, 0.08 * sc); rr(0.3 * sc, -1.3 * sc, 0.18 * sc, 0.5 * sc, 0.09 * sc); rr(0.1 * sc, -0.9 * sc, 0.38 * sc, 0.16 * sc, 0.08 * sc); }
      if (th.prop === 'pine') { cx.fillStyle = '#5a3a22'; cx.fillRect(-0.08 * sc, -0.5 * sc, 0.16 * sc, 0.5 * sc); cx.fillStyle = '#2f7a3b'; for (let t = 0; t < 3; t++) { cx.beginPath(); cx.moveTo(-0.7 * sc + t * 0.15 * sc, -0.4 * sc - t * 0.6 * sc); cx.lineTo(0, -1.4 * sc - t * 0.6 * sc); cx.lineTo(0.7 * sc - t * 0.15 * sc, -0.4 * sc - t * 0.6 * sc); cx.fill(); } }
      if (th.prop === 'rock') { cx.fillStyle = '#7a3020'; cx.beginPath(); cx.moveTo(-0.7 * sc, 0); cx.lineTo(-0.5 * sc, -0.7 * sc); cx.lineTo(0.1 * sc, -0.9 * sc); cx.lineTo(0.6 * sc, -0.4 * sc); cx.lineTo(0.7 * sc, 0); cx.fill(); }
      if (th.prop === 'lamp') { cx.fillStyle = '#3a4466'; cx.fillRect(-0.04 * sc, -2.2 * sc, 0.08 * sc, 2.2 * sc); cx.fillStyle = '#ffe9a0'; cx.shadowColor = '#ffd36b'; cx.shadowBlur = 18; cx.beginPath(); cx.arc(0, -2.25 * sc, 0.13 * sc, 0, 7); cx.fill(); cx.shadowBlur = 0; }
      cx.restore();
    }
  }
  function rr(x, y, w, h, r) { cx.beginPath(); cx.roundRect ? cx.roundRect(x, y, w, h, r) : cx.rect(x, y, w, h); cx.fill(); }

  function drawTerrain(th) {
    const left = cam.x - W / S, right = cam.x + W / S, bottom = Y(lv.minY - 30);
    for (const l of lv.lines) {
      if (l[l.length - 1][0] < left || l[0][0] > right) continue;
      cx.beginPath(); cx.moveTo(X(l[0][0]), Math.max(bottom, H + 10));
      for (const [x, y] of l) cx.lineTo(X(x), Y(y));
      cx.lineTo(X(l[l.length - 1][0]), Math.max(bottom, H + 10)); cx.closePath();
      cx.fillStyle = th.ground; cx.fill();
      cx.save(); cx.clip(); cx.strokeStyle = th.stripe; cx.lineWidth = S * 0.5; cx.globalAlpha = 0.5;
      for (let k = Math.floor(left / 3); k < right / 3; k++) { cx.beginPath(); cx.moveTo(X(k * 3), 0); cx.lineTo(X(k * 3) - H, H); cx.stroke(); }
      cx.restore();
      cx.beginPath(); l.forEach(([x, y], i) => i ? cx.lineTo(X(x), Y(y)) : cx.moveTo(X(x), Y(y)));
      cx.strokeStyle = th.edge; cx.lineWidth = Math.max(4, S * 0.18); cx.lineJoin = 'round'; cx.stroke();
    }
    // checkpoints and finish
    lv.cps.forEach((c, k) => { const gy = TH.groundY(lv, c); if (gy === null) return; flag(X(c), Y(gy), k <= cpIdx ? '#35d07f' : '#ffcf3a'); });
    const fy = TH.groundY(lv, lv.finish) ?? 0, fx = X(lv.finish), top = Y(fy + 3.2);
    cx.fillStyle = '#222'; cx.fillRect(fx - S * 0.08, top, S * 0.16, Y(fy) - top);
    const sq = S * 0.3; for (let r = 0; r < 3; r++) for (let c = 0; c < 6; c++) { cx.fillStyle = (r + c) % 2 ? '#111' : '#fff'; cx.fillRect(fx + c * sq, top + r * sq, sq, sq); }
  }
  function flag(x, y, col) { cx.fillStyle = '#333'; cx.fillRect(x - S * 0.04, y - S * 2, S * 0.08, S * 2); cx.fillStyle = col; cx.beginPath(); cx.moveTo(x, y - S * 2); cx.lineTo(x + S * 0.7, y - S * 1.75); cx.lineTo(x, y - S * 1.5); cx.fill(); }

  function drawBike() {
    const p = bike.p, ang = TH.angleOf(bike), c = Math.cos(ang), s = Math.sin(ang);
    const L = (lx, ly) => [X(p[0].x + lx * c - ly * s), Y(p[0].y + lx * s + ly * c)]; // bike-local → screen
    const line = (pts, w, col) => { cx.beginPath(); pts.forEach((q, i) => i ? cx.lineTo(...q) : cx.moveTo(...q)); cx.strokeStyle = col; cx.lineWidth = w * S; cx.lineCap = 'round'; cx.lineJoin = 'round'; cx.stroke(); };
    const poly = (pts, col) => { cx.beginPath(); pts.forEach((q, i) => i ? cx.lineTo(...L(...q)) : cx.moveTo(...L(...q))); cx.closePath(); cx.fillStyle = col; cx.fill(); };
    // wheels
    for (let i = 0; i < 2; i++) {
      const wx = X(p[i].x), wy = Y(p[i].y), r = TH.WR * S;
      cx.fillStyle = '#1b1b1b'; cx.beginPath(); cx.arc(wx, wy, r, 0, 7); cx.fill();
      cx.fillStyle = '#3a3a3a'; for (let k = 0; k < 14; k++) { const a = bike.wa[i] + k * 0.449; cx.beginPath(); cx.arc(wx + Math.cos(a) * r * 0.97, wy - Math.sin(a) * r * 0.97, r * 0.09, 0, 7); cx.fill(); }
      cx.strokeStyle = '#c9ccd2'; cx.lineWidth = Math.max(1.5, S * 0.035); cx.beginPath(); cx.arc(wx, wy, r * 0.7, 0, 7); cx.stroke();
      cx.lineWidth = Math.max(1, S * 0.02); for (let k = 0; k < 8; k++) { const a = bike.wa[i] + k * 0.785; cx.beginPath(); cx.moveTo(wx, wy); cx.lineTo(wx + Math.cos(a) * r * 0.7, wy - Math.sin(a) * r * 0.7); cx.stroke(); }
      cx.fillStyle = '#888'; cx.beginPath(); cx.arc(wx, wy, r * 0.14, 0, 7); cx.fill();
    }
    line([L(0, 0), L(0.62, 0.36)], 0.09, '#555b66'); // swingarm
    line([L(1.45, 0), L(1.18, 0.86)], 0.08, '#c9ccd2'); // fork
    poly([[0.48, 0.24], [0.92, 0.22], [1.02, 0.5], [0.6, 0.52]], '#3a3f4a'); // engine
    poly([[0.05, 0.5], [0.42, 0.66], [0.9, 0.7], [1.1, 0.78], [1.12, 0.6], [0.8, 0.52], [0.35, 0.5]], '#ff6a1a'); // body
    poly([[0.62, 0.66], [1.06, 0.8], [1.08, 0.66], [0.78, 0.56]], '#ff8b3d'); // tank highlight
    poly([[0.12, 0.62], [0.68, 0.74], [0.7, 0.68], [0.15, 0.56]], '#1e1e1e'); // seat
    poly([[1.28, 0.46], [1.62, 0.42], [1.66, 0.36], [1.3, 0.38]], '#ff6a1a'); // front fender
    poly([[1.08, 0.72], [1.26, 0.98], [1.32, 0.8], [1.2, 0.62]], '#ffffff'); // number plate
    line([L(1.12, 0.98), L(1.02, 1.04)], 0.05, '#222'); // bars
    // rider
    const head = [X(p[3].x), Y(p[3].y)], hip = L(0.5, 0.82);
    const shoulder = [hip[0] + (head[0] - hip[0]) * 0.72, hip[1] + (head[1] - hip[1]) * 0.72];
    line([hip, L(0.92, 0.58), L(0.7, 0.3)], 0.13, '#1f3a8a'); // leg
    line([L(0.62, 0.28), L(0.8, 0.3)], 0.09, '#111'); // boot
    line([hip, shoulder], 0.2, '#2b56d9'); // torso
    line([shoulder, L(0.92, 0.92), L(1.08, 1.0)], 0.1, '#2b56d9'); // arm
    cx.fillStyle = '#f5f5f5'; cx.beginPath(); cx.arc(head[0], head[1], 0.24 * S, 0, 7); cx.fill();
    cx.fillStyle = '#ff6a1a'; cx.beginPath(); cx.arc(head[0], head[1], 0.24 * S, Math.PI * 0.9 - ang, Math.PI * 1.6 - ang); cx.fill();
    const vis = [head[0] + Math.cos(-ang + 0.1) * 0.13 * S, head[1] + Math.sin(-ang + 0.1) * 0.13 * S];
    cx.fillStyle = '#1b2333'; cx.beginPath(); cx.arc(vis[0], vis[1], 0.1 * S, 0, 7); cx.fill();
  }

  function draw() {
    cx.setTransform(dpr, 0, 0, dpr, 0, 0);
    if (!lv) { drawTitleBg(); return; }
    const th = THEMES[lv.theme];
    drawBackground(th); drawProps(th); drawTerrain(th);
    for (const p of parts) { cx.globalAlpha = Math.max(0, p.life / 0.7); cx.fillStyle = p.c; cx.beginPath(); cx.arc(X(p.x), Y(p.y), p.r * S, 0, 7); cx.fill(); }
    cx.globalAlpha = 1; drawBike();
    for (const t of toasts) {
      const a = t.t < 0.2 ? t.t / 0.2 : t.t > 1.1 ? (1.4 - t.t) / 0.3 : 1;
      cx.globalAlpha = a; cx.font = `900 ${t.big ? 34 : 24}px system-ui, sans-serif`; cx.textAlign = 'center';
      cx.lineWidth = 6; cx.strokeStyle = 'rgba(0,0,0,.55)'; cx.strokeText(t.text, W / 2, H * 0.28 - t.t * 30);
      cx.fillStyle = t.big ? '#ffd23a' : '#fff'; cx.fillText(t.text, W / 2, H * 0.28 - t.t * 30);
    }
    cx.globalAlpha = 1;
    if (bike.crashed) { cx.fillStyle = 'rgba(160,20,20,.18)'; cx.fillRect(0, 0, W, H); }
  }
  let titleT = 0;
  function drawTitleBg() {
    titleT += 1 / 60; const th = THEMES.dunes; cam.x = titleT * 6; cam.y = 0;
    drawBackground(th);
  }

  /* ---------- loop ---------- */
  let last = performance.now(), acc = 0;
  function frame(now) {
    acc += Math.min(0.1, (now - last) / 1000); last = now;
    while (acc >= 1 / 60) { update(1 / 60); acc -= 1 / 60; }
    draw(); requestAnimationFrame(frame);
  }

  /* ---------- wire up menus ---------- */
  $('#play').onclick = () => { audio.unlock(); show('levels'); };
  $('#lv-back').onclick = () => show('title');
  $('#pause').onclick = togglePause;
  $('#restart').onclick = restart;
  $('#p-resume').onclick = togglePause; $('#p-sound').onclick = () => { audio.setMuted(!audio.muted); paintMute(); }; $('#p-restart').onclick = restart; $('#p-levels').onclick = () => { lv = null; show('levels'); };
  $('#d-next').onclick = next; $('#d-retry').onclick = restart; $('#d-levels').onclick = () => { lv = null; show('levels'); };
  $('#rotate-ok').onclick = () => { rotateDismissed = true; resize(); };
  const mute = $('#mute'); const paintMute = () => { mute.textContent = audio.muted ? '🔇' : '🔊'; $('#p-sound').textContent = audio.muted ? 'Sound: off' : 'Sound: on'; };
  mute.onclick = () => { audio.setMuted(!audio.muted); paintMute(); }; paintMute();
  document.addEventListener('visibilitychange', () => { if (document.hidden && state === 'play') togglePause(); });
  resize(); show('title'); requestAnimationFrame(frame);
  window.__th = { startLevel, get bike() { return bike; }, get state() { return state; }, finishNow: () => { bike.finished = true; finish(); } };
})();
