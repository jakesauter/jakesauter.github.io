"use strict";
// The page can live on jakesauter.github.io (talks to the desktop through Tailscale Funnel) or be served by the
// desktop itself. Nothing secret is in this file; the sign-in token lives only in this tab (sessionStorage).
const BACKEND = location.hostname.endsWith("github.io")
  ? "https://coderx-legion-t730-28ico.tailf42501.ts.net:10000" : "";
const $ = (id) => document.getElementById(id);
const store = {
  get(k) { try { return sessionStorage.getItem(k); } catch { return null; } },
  set(k, v) { try { v == null ? sessionStorage.removeItem(k) : sessionStorage.setItem(k, v); } catch {} },
};
let token = store.get("rt_token");
let cur = null;           // {name, aid, pos, abort}
let term, fit;
let lastActivity = Date.now();

async function api(path, body) {
  const opt = { headers: {}, cache: "no-store" };
  if (token) opt.headers.Authorization = "Bearer " + token;
  if (body !== undefined) { opt.method = "POST"; opt.headers["Content-Type"] = "application/json"; opt.body = JSON.stringify(body); }
  const r = await fetch(BACKEND + path, opt);
  let j = {};
  try { j = await r.json(); } catch {}
  if (r.status === 401 && path !== "/api/login" && path !== "/api/setup") { signedOut(); throw new Error("login"); }
  if (!r.ok) throw new Error(j.error || "Error " + r.status);
  lastActivity = Date.now();
  return j;
}

function show(id) { for (const s of ["login", "setup", "app"]) $(s).hidden = s !== id; }

function signedOut() {
  token = null; store.set("rt_token", null);
  if (cur && cur.abort) cur.abort.abort();
  cur = null;
  show("login");
  $("pw").focus();
}

// ---------------------------------------------------------------- sign in
$("loginForm").addEventListener("submit", async (e) => {
  e.preventDefault();
  const btn = e.target.querySelector("button"); btn.disabled = true; $("loginErr").textContent = "";
  try {
    const j = await api("/api/login", { password: $("pw").value, code: $("code").value });
    token = j.token; store.set("rt_token", token);
    $("pw").value = ""; $("code").value = "";
    startApp();
  } catch (err) {
    $("loginErr").textContent = err.message === "Failed to fetch"
      ? "Can't reach your desktop. It may be off, or remote access is turned off." : err.message;
  } finally { btn.disabled = false; }
});

// ---------------------------------------------------------------- first-time setup (#setup=TOKEN)
const setupTok = (location.hash.match(/setup=([\w-]+)/) || [])[1];
let setupSecret = null;
$("setupForm").addEventListener("submit", async (e) => {
  e.preventDefault();
  $("setupErr").textContent = "";
  const btn = e.submitter || e.target.querySelector("button:not([hidden])");
  if (btn) btn.disabled = true;
  try {
    if (!setupSecret) {
      if ($("spw").value !== $("spw2").value) throw new Error("The two passwords don't match.");
      const j = await api("/api/setup", { setup: setupTok, password: $("spw").value });
      setupSecret = j.secret;
      $("qr").innerHTML = j.qr || "";
      $("secretTxt").textContent = j.secret.replace(/(.{4})/g, "$1 ").trim();
      $("setup1").hidden = true; $("setup2").hidden = false; $("scode").focus();
    } else {
      await api("/api/setup", { setup: setupTok, password: $("spw").value, secret: setupSecret, code: $("scode").value });
      history.replaceState(null, "", location.pathname);
      $("spw").value = $("spw2").value = "";
      alert("All set. Sign in with your password and a fresh code.");
      show("login");
    }
  } catch (err) { $("setupErr").textContent = err.message; }
  finally { if (btn) btn.disabled = false; }
});

// ---------------------------------------------------------------- app
function makeTerm() {
  if (term) return;
  term = new Terminal({
    cursorBlink: true, fontSize: window.innerWidth < 760 ? 12 : 14, scrollback: 5000, allowProposedApi: false,
    fontFamily: 'ui-monospace, "SF Mono", Menlo, Consolas, "DejaVu Sans Mono", monospace',
    theme: { background: "#1f1e1d", foreground: "#ece9e1", cursor: "#d97757", selectionBackground: "#5a4a40" },
  });
  fit = new FitAddon.FitAddon();
  term.loadAddon(fit);
  term.open($("term"));
  term.onData((d) => send(d));
  new ResizeObserver(() => resize()).observe($("termWrap"));
}

let resizeTimer;
function resize() {
  if (!fit) return;
  try { fit.fit(); } catch { return; }
  clearTimeout(resizeTimer);
  resizeTimer = setTimeout(() => { if (cur) api("/api/resize", { aid: cur.aid, cols: term.cols, rows: term.rows }).catch(() => {}); }, 150);
}

// Keystrokes are batched for ~15 ms so typing doesn't send one request per key.
let outQ = "", outTimer = null, sending = Promise.resolve();
function send(data) {
  if (!cur) return;
  outQ += data;
  if (!outTimer) outTimer = setTimeout(flush, 15);
}
function flush() {
  outTimer = null;
  const data = outQ; outQ = "";
  if (!data || !cur) return;
  const aid = cur.aid;
  sending = sending.then(() => api("/api/in", { aid, data }).catch((e) => { if (e.message !== "login") term.write("\r\n\x1b[31m[" + e.message + "]\x1b[0m\r\n"); }));
}

async function refresh() {
  const j = await api("/api/sessions");
  const sel = $("folder"), keep = sel.value;
  sel.innerHTML = "";
  for (const f of j.folders) { const o = document.createElement("option"); o.value = o.textContent = f; sel.appendChild(o); }
  sel.value = keep || "~/Documents/Projects";
  if (!sel.value) sel.selectedIndex = 0;
  const ul = $("sessions"); ul.innerHTML = "";
  for (const s of j.sessions) {
    const li = document.createElement("li"), b = document.createElement("button");
    const isClaude = s.command === "claude" || s.name.startsWith("claude");
    b.innerHTML = `<span class="n"></span><span class="d"></span>`;
    b.querySelector(".n").textContent = (isClaude ? "✳ " : "") + s.name;
    b.querySelector(".d").textContent = `${s.command || "?"} · ${s.path}${s.attached ? " · open elsewhere" : ""}`;
    if (cur && cur.name === s.name) b.classList.add("on");
    b.onclick = () => { attach(s.name, s); closeSide(); };
    li.appendChild(b); ul.appendChild(li);
  }
  if (!j.sessions.length) ul.innerHTML = '<li class="muted" style="padding:8px 10px;font-size:13px">No sessions yet.</li>';
  return j.sessions;
}

async function attach(name, info) {
  detach();
  makeTerm();
  $("empty").hidden = true;
  term.reset();
  resize();
  const j = await api("/api/attach", { target: name, cols: term.cols, rows: term.rows });
  cur = { name, aid: j.aid, pos: 0, abort: null };
  $("curName").textContent = name;
  $("curInfo").textContent = info ? info.path : "";
  document.querySelectorAll("#sessions button").forEach((b) => b.classList.toggle("on", b.querySelector(".n").textContent.endsWith(name)));
  pump(cur);
  term.focus();
}

function detach() {
  if (!cur) return;
  const old = cur; cur = null;
  if (old.abort) old.abort.abort();
  api("/api/detach", { aid: old.aid }).catch(() => {});
}

// Output arrives as server-sent events over fetch (so the token can go in a header). Reconnects resume from `pos`.
async function pump(c) {
  let fails = 0;
  while (cur === c) {
    c.abort = new AbortController();
    try {
      const r = await fetch(`${BACKEND}/api/out?aid=${encodeURIComponent(c.aid)}&from=${c.pos}`,
        { headers: { Authorization: "Bearer " + token }, signal: c.abort.signal, cache: "no-store" });
      if (r.status === 401) return signedOut();
      if (r.status === 410) { term.write("\r\n\x1b[2m[view closed]\x1b[0m\r\n"); $("conn").className = "dot"; return; }
      if (!r.ok) throw new Error(r.status);
      $("conn").className = "dot ok"; fails = 0;
      const rd = r.body.getReader(), dec = new TextDecoder();
      let buf = "";
      for (;;) {
        const { value, done } = await rd.read();
        if (done) break;
        buf += dec.decode(value, { stream: true });
        let i;
        while ((i = buf.indexOf("\n\n")) >= 0) {
          const ev = buf.slice(0, i); buf = buf.slice(i + 2);
          if (ev.startsWith("event: end")) { term.write("\r\n\x1b[2m[session ended]\x1b[0m\r\n"); cur = null; refresh().catch(() => {}); return; }
          const line = ev.split("\n").find((l) => l.startsWith("data: "));
          if (!line) continue;
          const m = JSON.parse(line.slice(6));
          const bin = Uint8Array.from(atob(m.d), (ch) => ch.charCodeAt(0));
          term.write(bin);
          c.pos = m.p;
        }
      }
    } catch (e) {
      if (cur !== c) return;
      $("conn").className = "dot bad";
      fails++;
      await new Promise((ok) => setTimeout(ok, Math.min(8000, 500 * fails)));
    }
  }
}

const KEYS = { esc: "\x1b", tab: "\t", stab: "\x1b[Z", up: "\x1b[A", down: "\x1b[B", left: "\x1b[D", right: "\x1b[C", cc: "\x03", enter: "\r" };
$("keys").addEventListener("click", (e) => {
  const k = e.target.dataset && e.target.dataset.k;
  if (k) { send(KEYS[k]); if (window.innerWidth >= 760) term && term.focus(); }
});

// The message box types into the terminal and presses Enter. Multi-line text goes in as one paste.
$("composer").addEventListener("submit", (e) => {
  e.preventDefault();
  const t = $("msg").value;
  if (!cur || !t) return;
  send(t.includes("\n") ? "\x1b[200~" + t + "\x1b[201~" : t);
  setTimeout(() => send("\r"), 120);
  $("msg").value = ""; grow();
});
$("msg").addEventListener("keydown", (e) => {
  if (e.key === "Enter" && !e.shiftKey && !e.isComposing) { e.preventDefault(); $("composer").requestSubmit(); }
});
function grow() { const m = $("msg"); m.style.height = "auto"; m.style.height = Math.min(160, m.scrollHeight) + "px"; }
$("msg").addEventListener("input", grow);

async function newSession(kind) {
  try {
    const j = await api("/api/new", { kind, folder: $("folder").value });
    const list = await refresh();
    await attach(j.name, list.find((s) => s.name === j.name));
    closeSide();
  } catch (e) { if (e.message !== "login") alert(e.message); }
}
$("newClaude").onclick = () => newSession("claude");
$("newShell").onclick = () => newSession("shell");
$("logout").onclick = async () => { detach(); try { await api("/api/logout", {}); } catch {} signedOut(); };
$("openSide").onclick = () => $("side").classList.add("open");
function closeSide() { $("side").classList.remove("open"); }
$("closeSide").onclick = closeSide;

// Idle sign-out happens on the server after 30 minutes; show the countdown and keep the list fresh.
setInterval(() => {
  if (!token) return;
  const left = Math.max(0, 30 * 60 * 1000 - (Date.now() - lastActivity));
  $("timer").textContent = left < 5 * 60 * 1000 ? `Signs out in ${Math.ceil(left / 60000)} min` : "";
  if (!document.hidden && !cur) refresh().catch(() => {});
}, 20000);

async function startApp() {
  show("app");
  try {
    const list = await refresh();
    makeTerm(); resize();
    const pick = list.find((s) => s.command === "claude") || list[0];
    if (pick && window.innerWidth >= 760) attach(pick.name, pick);
  } catch (e) { if (e.message !== "login") alert(e.message); }
}

(async function boot() {
  if (setupTok) { show("setup"); $("spw").focus(); return; }
  if (token) {
    try { const s = await api("/api/state"); if (s.loggedIn) return startApp(); } catch {}
  }
  signedOut();
})();
