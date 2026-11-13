/* Contact form for the Sauter sites. Markup: <form data-contact-form data-site="analytics"> with fields name, email,
   phone, org (optional), message, and a hidden _honey trap. The form is emailed through the private site-forms relay;
   the visitor's email, if given, becomes the reply-to. Recipient addresses live in the relay, not in this page. */
(function () {
  "use strict";
  var ENDPOINT = "https://site-forms.jake-sauter3.workers.dev/send?site=";
  var EMAIL = /^[^\s@]+@[^\s@]+\.[^\s@]{2,}$/;
  document.querySelectorAll("form[data-contact-form]").forEach(function (form) {
    var F = form.elements, status = form.querySelector("[data-status]"), btn = form.querySelector("button[type=submit]");
    var say = function (text, bad) { status.textContent = text; status.classList.toggle("bad", !!bad); };
    form.addEventListener("submit", function (e) {
      e.preventDefault();
      var email = F.email.value.trim(), phone = F.phone.value.replace(/\D/g, "");
      var reachable = (EMAIL.test(email) || (!email && phone.length >= 10)) && (!email || EMAIL.test(email));
      var problems = [];
      if (!F.name.value.trim()) problems.push(F.name);
      if (!reachable) problems.push(email ? F.email : F.phone);
      if (!F.message.value.trim()) problems.push(F.message);
      [F.name, F.email, F.phone, F.message].forEach(function (el) { el.setAttribute("aria-invalid", problems.indexOf(el) >= 0 ? "true" : "false"); });
      if (problems.length) { say("Please add your name, a short message, and a valid email or phone number so I can get back to you.", true); problems[0].focus(); return; }
      var data = {
        _subject: (form.getAttribute("data-subject") || "New inquiry") + ": " + F.name.value.trim(),
        _replyto: email, _honey: F._honey.value,
        "Name": F.name.value.trim(), "Reply by email": email || "(none given)", "Reply by phone": F.phone.value.trim() || "(none given)",
        "Organization": F.org ? F.org.value.trim() : "", "Message": F.message.value.trim()
      };
      if (F.topic) data["Interested in"] = F.topic.value;
      btn.disabled = true; btn.textContent = "Sending…"; say("");
      fetch(ENDPOINT + encodeURIComponent(form.getAttribute("data-site")), {
        method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(data)
      }).then(function (r) { return r.json().then(function (j) { if (!r.ok || !j.success) throw new Error(j.message); }); })
        .then(function () { form.reset(); say("Thanks! Your message was sent. I'll get back to you within a business day."); },
              function () { say("Sorry, that didn't go through. Please try again in a minute, or message me on LinkedIn.", true); })
        .then(function () { btn.disabled = false; btn.textContent = "Send message"; });
    });
  });
})();
