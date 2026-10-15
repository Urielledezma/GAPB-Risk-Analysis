// Slider over the daily re-runs of the class moving-average procedure.
//
// Stage 03 inlines this file after the data it reads, window.MA_TRACKING:
// one entry per tracked session with the 70/30 cut, the four losses of every
// window and its next-session volatility. Nothing is computed here beyond
// formatting and finding each column's minimum; every number comes from R.
(function () {
  "use strict";

  var root = document.getElementById("ma-explorer");
  var data = window.MA_TRACKING;
  if (!root || !data || !data.sessions || data.sessions.length === 0) {
    return;
  }

  var sessions = data.sessions;
  var losses = [
    { key: "rmse", scale: 1e4, digits: 3 },
    { key: "mae", scale: 1e4, digits: 3 },
    { key: "qlike", scale: 1, digits: 1 },
    { key: "relative", scale: 1, digits: 3 }
  ];

  var slider = root.querySelector(".ma-slider");
  var dateLabel = root.querySelector(".ma-date");
  var summary = root.querySelector(".ma-summary");
  var body = root.querySelector("tbody");

  function fixed(value, digits) {
    return value.toLocaleString("en-US", {
      minimumFractionDigits: digits,
      maximumFractionDigits: digits
    });
  }

  function percent(value, digits) {
    return fixed(100 * value, digits) + "%";
  }

  function cell(text, strong) {
    var td = document.createElement("td");
    td.style.textAlign = "right";
    if (strong) {
      var b = document.createElement("strong");
      b.textContent = text;
      td.appendChild(b);
    } else {
      td.textContent = text;
    }
    return td;
  }

  function render(index) {
    var s = sessions[index];
    dateLabel.textContent = s.date;
    summary.textContent =
      s.n.toLocaleString("en-US") + " rendimientos hasta esta sesión. Corte del 70%: " +
      s.cut_date + "; evaluación desde el " + s.test_start + " (" +
      s.n_test.toLocaleString("en-US") + " sesiones). Ventana elegida: M" + s.chosen +
      ", con una volatilidad pronosticada para la sesión siguiente de " +
      percent(s.vol_chosen, 3) + " diaria (" + percent(s.vol_chosen * Math.sqrt(252), 1) +
      " anualizada).";

    var minima = losses.map(function (loss) {
      return Math.min.apply(null, s.windows.map(function (w) { return w[loss.key]; }));
    });

    body.innerHTML = "";
    s.windows.forEach(function (w) {
      var tr = document.createElement("tr");
      var label = document.createElement("td");
      label.textContent = "M" + w.m + (w.m === s.chosen ? " (elegida)" : "");
      if (w.m === s.chosen) {
        label.style.fontWeight = "700";
      }
      tr.appendChild(label);
      losses.forEach(function (loss, k) {
        tr.appendChild(cell(fixed(w[loss.key] * loss.scale, loss.digits), w[loss.key] === minima[k]));
      });
      tr.appendChild(cell(percent(w.vol, 3), false));
      body.appendChild(tr);
    });
  }

  slider.min = 0;
  slider.max = sessions.length - 1;
  slider.value = sessions.length - 1;
  slider.addEventListener("input", function (event) {
    render(Number(event.target.value));
  });
  render(sessions.length - 1);
  // The container ships hidden so that a reader without JavaScript sees only the
  // static table and figures, never an empty control.
  root.hidden = false;
})();
