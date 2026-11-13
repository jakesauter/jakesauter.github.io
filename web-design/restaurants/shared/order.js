/* Direct ordering for one restaurant: menu, cart, pickup or delivery, checkout and order tracking.
   Reads the restaurant from <script type="application/json" id="restaurant">. In this demo an order is saved in
   the browser (localStorage) where the kitchen screen (../shared/kitchen.html?r=<id>) picks it up; a real site
   sends it to a small server that texts the restaurant and takes card payment through Stripe (see TECH.md). */
(function () {
  "use strict";

  const R = JSON.parse(document.getElementById("restaurant").textContent);
  const ORDERS_KEY = "demo-orders-" + R.id;
  const CART_KEY = "demo-cart-" + R.id;
  const $ = (sel, root) => (root || document).querySelector(sel);
  const money = n => "$" + (Math.round(n * 100) / 100).toFixed(2);
  const esc = s => String(s == null ? "" : s).replace(/[&<>"']/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));
  const store = {
    get(key, fallback) { try { const v = localStorage.getItem(key); return v ? JSON.parse(v) : fallback; } catch (e) { return fallback; } },
    set(key, value) { try { localStorage.setItem(key, JSON.stringify(value)); } catch (e) { /* private mode: cart lives in memory */ } },
  };

  let mode = store.get("demo-mode-" + R.id, R.delivery ? "delivery" : "pickup");
  let cart = store.get(CART_KEY, []);
  const ITEMS = new Map();
  R.categories.forEach(c => c.items.forEach(item => ITEMS.set(item.id, item)));

  /* ---------- opening hours ---------- */
  const DAY = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
  const minutes = hhmm => { const [h, m] = hhmm.split(":").map(Number); return h * 60 + m; };
  const clock = mins => { const h = Math.floor(mins / 60) % 24, m = mins % 60; return ((h + 11) % 12 + 1) + ":" + String(m).padStart(2, "0") + (h < 12 ? " AM" : " PM"); };
  function hoursToday(now) { return R.hours.find(h => h.days.includes(DAY[now.getDay()])); }
  function openState(now) {
    const today = hoursToday(now);
    if (!today) return { open: false, text: "Closed today" };
    const t = now.getHours() * 60 + now.getMinutes(), o = minutes(today.open), c = minutes(today.close);
    if (t < o) return { open: false, text: "Opens at " + clock(o), opensAt: o, closesAt: c };
    if (t >= c) return { open: false, text: "Closed for today · opens tomorrow" };
    return { open: true, text: "Open now · closes " + clock(c), closesAt: c };
  }
  function timeSlots(now) {
    // ASAP (when open) plus every 15 minutes until closing, starting after the prep time
    const today = hoursToday(now); if (!today) return [];
    const prep = mode === "delivery" ? R.delivery.leadMinutes : R.pickup.leadMinutes;
    const start = Math.max(minutes(today.open) + prep, now.getHours() * 60 + now.getMinutes() + prep);
    const slots = [];
    for (let t = Math.ceil(start / 15) * 15; t <= minutes(today.close); t += 15) slots.push(t);
    return slots;
  }

  /* ---------- prices ---------- */
  function linePrice(line) {
    const item = ITEMS.get(line.id); let each = item.price;
    (item.options || []).forEach((opt, oi) => (line.choices[oi] || []).forEach(ci => { each += opt.choices[ci].price || 0; }));
    return each * line.qty;
  }
  function totals(tipRate) {
    const subtotal = cart.reduce((s, l) => s + linePrice(l), 0);
    const delivery = mode === "delivery" && cart.length ? R.delivery.fee : 0;
    const tax = subtotal * R.taxRate;
    const tip = subtotal * (tipRate || 0);
    return { subtotal, delivery, tax, tip, total: subtotal + delivery + tax + tip };
  }
  function choiceText(line) {
    const item = ITEMS.get(line.id);
    return (item.options || []).map((opt, oi) => (line.choices[oi] || []).map(ci => opt.choices[ci].name).join(", ")).filter(Boolean).join(" · ");
  }

  /* ---------- page ---------- */
  function renderHeader() {
    const state = openState(new Date());
    $("#open-state").textContent = state.text;
    $("#open-state").classList.toggle("closed", !state.open);
    document.querySelectorAll("[data-mode]").forEach(b => b.setAttribute("aria-pressed", String(b.dataset.mode === mode)));
    $("#eta").textContent = mode === "delivery"
      ? `Delivery by our own drivers · ${R.delivery.eta} · ${money(R.delivery.fee)} fee · ${money(R.delivery.minimum)} minimum`
      : `Pickup · ready in ${R.pickup.eta}`;
  }

  function renderMenu() {
    const nav = $("#cats"), menu = $("#menu");
    nav.innerHTML = R.categories.map((c, i) => `<a href="#cat-${i}">${esc(c.name)}</a>`).join("");
    menu.innerHTML = R.categories.map((c, i) => `
      <section class="cat" id="cat-${i}" aria-labelledby="cat-h-${i}">
        <h2 id="cat-h-${i}">${esc(c.name)}</h2>${c.note ? `<p class="cat-note">${esc(c.note)}</p>` : ""}
        <ul class="items">${c.items.map(item => `
          <li><button type="button" class="item" data-item="${esc(item.id)}">
            <span class="item-main">
              <span class="item-name">${esc(item.name)}${item.native ? ` <span class="native" lang="${esc(R.nativeLang || "")}">${esc(item.native)}</span>` : ""}</span>
              ${item.desc ? `<span class="item-desc">${esc(item.desc)}</span>` : ""}
              <span class="tags">${(item.tags || []).map(t => `<span class="tag tag-${esc(t)}">${esc(TAGS[t] || t)}</span>`).join("")}</span>
            </span>
            <span class="item-price">${item.priceLabel ? esc(item.priceLabel) : money(item.price)}</span>
          </button></li>`).join("")}
        </ul>
      </section>`).join("");
  }
  const TAGS = { popular: "Popular", spicy: "Spicy", gf: "Gluten-free", veg: "Vegetarian", new: "New", house: "House favorite" };

  /* ---------- item sheet ---------- */
  const itemDialog = $("#item-dialog");
  let editing = null;
  function openItem(id) {
    const item = ITEMS.get(id);
    editing = { id, qty: 1, choices: (item.options || []).map(opt => opt.required ? [0] : []), note: "" };
    const body = $("#item-body");
    body.innerHTML = `
      <h2 id="item-title">${esc(item.name)}${item.native ? ` <span class="native">${esc(item.native)}</span>` : ""}</h2>
      ${item.desc ? `<p class="item-desc">${esc(item.desc)}</p>` : ""}
      ${(item.options || []).map((opt, oi) => `
        <fieldset class="opt"><legend>${esc(opt.name)} <small>${opt.required ? "Required" : opt.multi ? "Choose any" : "Optional"}</small></legend>
          ${opt.choices.map((ch, ci) => `
            <label class="choice"><input type="${opt.multi ? "checkbox" : "radio"}" name="opt-${oi}" value="${ci}" ${opt.required && ci === 0 ? "checked" : ""}>
              <span>${esc(ch.name)}</span><span class="choice-price">${ch.price ? "+" + money(ch.price) : ""}</span></label>`).join("")}
        </fieldset>`).join("")}
      <label class="note">Special instructions <textarea id="item-note" rows="2" maxlength="140" placeholder="No onions, extra sauce, cut in squares…"></textarea></label>`;
    body.querySelectorAll("input").forEach(input => input.addEventListener("change", () => {
      const oi = Number(input.name.slice(4));
      editing.choices[oi] = [...body.querySelectorAll(`input[name="opt-${oi}"]:checked`)].map(x => Number(x.value));
      paintItemTotal();
    }));
    $("#qty").textContent = "1";
    paintItemTotal();
    itemDialog.showModal();
  }
  function paintItemTotal() { $("#add-item").textContent = "Add to order · " + money(linePrice(editing)); }
  $("#qty-minus").addEventListener("click", () => { if (editing.qty > 1) { editing.qty--; $("#qty").textContent = editing.qty; paintItemTotal(); } });
  $("#qty-plus").addEventListener("click", () => { if (editing.qty < 20) { editing.qty++; $("#qty").textContent = editing.qty; paintItemTotal(); } });
  $("#add-item").addEventListener("click", () => {
    editing.note = $("#item-note").value.trim();
    cart.push(editing); saveCart(); itemDialog.close();
    toast("Added " + ITEMS.get(editing.id).name);
  });
  document.addEventListener("click", e => {
    const b = e.target.closest("[data-item]"); if (b) openItem(b.dataset.item);
    const close = e.target.closest("[data-close]"); if (close) close.closest("dialog").close();
  });
  document.querySelectorAll("dialog").forEach(d => d.addEventListener("click", e => { if (e.target === d) d.close(); }));

  /* ---------- cart ---------- */
  const cartDialog = $("#cart-dialog");
  let tipRate = R.tips[1];
  function saveCart() { store.set(CART_KEY, cart); paintCartButton(); }
  function paintCartButton() {
    const n = cart.reduce((s, l) => s + l.qty, 0);
    const bar = $("#cart-bar");
    bar.hidden = n === 0;
    $("#cart-count").textContent = n + (n === 1 ? " item" : " items");
    $("#cart-total").textContent = money(totals(0).subtotal);
  }
  function renderCart() {
    const t = totals(tipRate);
    const short = mode === "delivery" ? R.delivery.minimum - t.subtotal : 0;
    $("#cart-body").innerHTML = cart.length ? `
      <ul class="lines">${cart.map((l, i) => `
        <li><div><b>${l.qty}× ${esc(ITEMS.get(l.id).name)}</b>${choiceText(l) ? `<small>${esc(choiceText(l))}</small>` : ""}${l.note ? `<small>“${esc(l.note)}”</small>` : ""}</div>
          <div class="line-right">${money(linePrice(l))}
            <span class="line-qty"><button type="button" data-line="${i}" data-step="-1" aria-label="One fewer">−</button><button type="button" data-line="${i}" data-step="1" aria-label="One more">+</button></span></div></li>`).join("")}
      </ul>
      <fieldset class="tip"><legend>Tip ${mode === "delivery" ? "for the driver" : "for the kitchen"}</legend>
        ${R.tips.map(r => `<label><input type="radio" name="tip" value="${r}" ${r === tipRate ? "checked" : ""}><span>${Math.round(r * 100)}%</span></label>`).join("")}
      </fieldset>
      <dl class="sums">
        <dt>Subtotal</dt><dd>${money(t.subtotal)}</dd>
        ${mode === "delivery" ? `<dt>Delivery</dt><dd>${money(t.delivery)}</dd>` : ""}
        <dt>Meals tax (${(R.taxRate * 100).toFixed(2).replace(/\.?0+$/, "")}%)</dt><dd>${money(t.tax)}</dd>
        <dt>Tip</dt><dd>${money(t.tip)}</dd>
        <dt class="total">Total</dt><dd class="total">${money(t.total)}</dd>
      </dl>
      <p class="no-fee">No service fees and no app markup: menu prices are the same as in the restaurant.</p>
      ${short > 0 ? `<p class="warn">Add ${money(short)} more for delivery, or switch to pickup.</p>` : ""}`
      : `<p class="empty">Your order is empty. Tap anything on the menu to add it.</p>`;
    $("#to-checkout").disabled = !cart.length || short > 0;
    $("#cart-body").querySelectorAll("[data-line]").forEach(b => b.addEventListener("click", () => {
      const i = Number(b.dataset.line); cart[i].qty += Number(b.dataset.step);
      if (cart[i].qty <= 0) cart.splice(i, 1);
      saveCart(); renderCart();
    }));
    $("#cart-body").querySelectorAll('input[name="tip"]').forEach(r => r.addEventListener("change", () => { tipRate = Number(r.value); renderCart(); }));
  }
  $("#cart-open").addEventListener("click", () => { renderCart(); cartDialog.showModal(); });

  /* ---------- checkout ---------- */
  const checkoutDialog = $("#checkout-dialog");
  $("#to-checkout").addEventListener("click", () => { cartDialog.close(); renderCheckout(); checkoutDialog.showModal(); });
  function renderCheckout() {
    const slots = timeSlots(new Date()), state = openState(new Date());
    const saved = store.get("demo-customer", {});
    $("#checkout-body").innerHTML = `
      <p class="mode-line">${mode === "delivery" ? "Delivery" : "Pickup"} · <button type="button" class="link" id="switch-mode">switch to ${mode === "delivery" ? "pickup" : "delivery"}</button></p>
      <label>Name <input name="name" autocomplete="name" required value="${esc(saved.name || "")}"></label>
      <label>Mobile phone <input name="phone" type="tel" autocomplete="tel" required placeholder="(617) 555-0100" value="${esc(saved.phone || "")}"></label>
      ${mode === "delivery" ? `
        <label>Street address <input name="street" autocomplete="street-address" required value="${esc(saved.street || "")}"></label>
        <div class="row2"><label>ZIP <input name="zip" inputmode="numeric" autocomplete="postal-code" required maxlength="5" value="${esc(saved.zip || "")}"></label>
          <label>Apt / floor <input name="apt" value="${esc(saved.apt || "")}"></label></div>
        <label>Note for the driver <input name="dropoff" placeholder="Side door, ring bell…"></label>
        <p class="hint">We deliver to ${R.delivery.zips.join(", ")}.</p>` : ""}
      <label>When <select name="when">
        ${state.open ? `<option value="asap">As soon as possible (${mode === "delivery" ? R.delivery.eta : R.pickup.eta})</option>` : ""}
        ${slots.map(t => `<option value="${t}">Today ${clock(t)}</option>`).join("")}
        ${!state.open && !slots.length ? `<option value="" disabled selected>Closed for today</option>` : ""}
      </select></label>
      <fieldset class="pay"><legend>Payment</legend>
        <label class="choice"><input type="radio" name="pay" value="card" checked><span>Card online <small>(demo: no card is charged)</small></span></label>
        <label class="choice"><input type="radio" name="pay" value="door"><span>Pay ${mode === "delivery" ? "the driver" : "at the counter"} (cash or card)</span></label>
      </fieldset>
      <p class="sum-line">Total ${money(totals(tipRate).total)}</p>
      <p class="error" id="checkout-error" role="alert"></p>`;
    $("#switch-mode").addEventListener("click", () => { setMode(mode === "delivery" ? "pickup" : "delivery"); renderCheckout(); });
  }
  $("#checkout-form").addEventListener("submit", e => {
    e.preventDefault();
    const f = new FormData(e.target), err = $("#checkout-error");
    const phone = String(f.get("phone") || "").replace(/\D/g, "");
    if (phone.length < 10) { err.textContent = "Please enter a 10-digit phone number so the restaurant can reach you."; return; }
    if (mode === "delivery" && !R.delivery.zips.includes(String(f.get("zip") || "").trim())) { err.textContent = "Sorry, that ZIP is outside our delivery area. Pickup is always available."; return; }
    if (mode === "delivery" && totals(0).subtotal < R.delivery.minimum) { err.textContent = "Delivery has a " + money(R.delivery.minimum) + " minimum."; return; }
    if (!f.get("when")) { err.textContent = "We're closed for today. Please come back during opening hours."; return; }
    const customer = { name: String(f.get("name")).trim(), phone: String(f.get("phone")).trim(), street: f.get("street") || "", zip: f.get("zip") || "", apt: f.get("apt") || "" };
    store.set("demo-customer", customer);
    const t = totals(tipRate);
    const order = {
      number: String(100 + store.get(ORDERS_KEY, []).length + Math.floor(Math.random() * 9)).padStart(3, "0"),
      placed: Date.now(), mode, when: f.get("when"), pay: f.get("pay"), dropoff: f.get("dropoff") || "",
      customer, status: "new", totals: t,
      lines: cart.map(l => ({ name: ITEMS.get(l.id).name, native: ITEMS.get(l.id).native || "", qty: l.qty, options: choiceText(l), note: l.note, price: linePrice(l) })),
    };
    const orders = store.get(ORDERS_KEY, []); orders.unshift(order); store.set(ORDERS_KEY, orders.slice(0, 50));
    emailOrder(order);
    cart = []; saveCart(); checkoutDialog.close(); showTracking(order.number);
  });

  /* ---------- the restaurant also gets every order by email (through the private form relay) ---------- */
  // R.orderEmail is the relay address for this restaurant; in the demos every restaurant's orders go to the site owner.
  function emailOrder(order) {
    if (!R.orderEmail) return;
    const when = order.when === "asap" ? "ASAP" : "Scheduled for " + clock(Number(order.when));
    const address = order.mode === "delivery" ? [order.customer.street, order.customer.apt, order.customer.zip].filter(Boolean).join(", ") : "";
    const t = order.totals;
    const body = {
      _subject: `New ${order.mode} order #${order.number} · ${money(t.total)} · ${R.name}`,
      "Order": "#" + order.number + " (" + (order.mode === "delivery" ? "DELIVERY" : "PICKUP") + ", " + when + ")",
      "Customer": order.customer.name, "Phone": order.customer.phone,
      "Deliver to": address, "Driver note": order.dropoff,
      "Items": order.lines.map(l => `${l.qty} x ${l.name}${l.native ? " " + l.native : ""} (${money(l.price)})${l.options ? "\n    " + l.options : ""}${l.note ? "\n    Note: " + l.note : ""}`).join("\n"),
      "Totals": `Subtotal ${money(t.subtotal)}${t.delivery ? " + delivery " + money(t.delivery) : ""} + tax ${money(t.tax)} + tip ${money(t.tip)} = ${money(t.total)}`,
      "Payment": order.pay === "card" ? "Paid online" : "Collect " + money(t.total) + (order.mode === "delivery" ? " at the door" : " at pickup"),
    };
    fetch(R.orderEmail, { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(body), keepalive: true }).catch(() => {});
  }

  /* ---------- tracking (updates live when the kitchen screen changes the status) ---------- */
  const STEPS = { pickup: ["new", "cooking", "ready", "done"], delivery: ["new", "cooking", "out", "done"] };
  const STEP_TEXT = { new: "Sent to the kitchen", cooking: "Being made", ready: "Ready for pickup", out: "Out for delivery", done: "Done. Enjoy!" };
  const trackDialog = $("#track-dialog");
  let tracking = null;
  function showTracking(number) { tracking = number; paintTracking(); trackDialog.showModal(); }
  function paintTracking() {
    if (!tracking) return;
    const order = store.get(ORDERS_KEY, []).find(o => o.number === tracking); if (!order) return;
    const steps = STEPS[order.mode], at = steps.indexOf(order.status);
    $("#track-body").innerHTML = `
      <h2>Order #${esc(order.number)} is in</h2>
      <p>${order.when === "asap" ? (order.mode === "delivery" ? "Arriving in about " + R.delivery.eta : "Ready in about " + R.pickup.eta) : "Scheduled for " + clock(Number(order.when))}.
         We'll text ${esc(order.customer.phone)} if anything changes.</p>
      <ol class="steps">${steps.map((s, i) => `<li class="${i < at ? "past" : i === at ? "now" : ""}">${STEP_TEXT[s]}</li>`).join("")}</ol>
      <p class="hint">Demo: open the <a href="../shared/kitchen.html?r=${esc(R.id)}" target="_blank" rel="noopener">kitchen screen</a> in another tab and move the order along. This page follows it.</p>
      <ul class="lines small">${order.lines.map(l => `<li><span>${l.qty}× ${esc(l.name)}${l.options ? ` <small>${esc(l.options)}</small>` : ""}</span><span>${money(l.price)}</span></li>`).join("")}</ul>
      <p class="sum-line">Total ${money(order.totals.total)} · ${order.pay === "card" ? "paid online (demo)" : "pay " + (order.mode === "delivery" ? "the driver" : "at pickup")}</p>`;
  }
  window.addEventListener("storage", e => { if (e.key === ORDERS_KEY) paintTracking(); });

  /* ---------- pickup / delivery ---------- */
  function setMode(m) { mode = m; store.set("demo-mode-" + R.id, m); renderHeader(); }
  document.querySelectorAll("[data-mode]").forEach(b => b.addEventListener("click", () => setMode(b.dataset.mode)));

  /* ---------- little things ---------- */
  let toastTimer = 0;
  function toast(text) {
    const t = $("#toast"); t.textContent = text; t.hidden = false;
    clearTimeout(toastTimer); toastTimer = setTimeout(() => { t.hidden = true; }, 1800);
  }
  $("#hours-list").innerHTML = R.hours.map(h => `<li><span>${esc(h.label)}</span><span>${clock(minutes(h.open))} – ${clock(minutes(h.close))}</span></li>`).join("");

  renderHeader(); renderMenu(); paintCartButton();
  setInterval(renderHeader, 60000);
})();
