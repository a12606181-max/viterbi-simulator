/* ============================================================================
  Веб-симулятор: Свёрточный код (rate 1/2, k=1, n=2) + декодер Витерби (hard)
  Всё написано максимально "по-учебному" и с комментариями для новичка.
============================================================================ */

/** -------------------- Утилиты DOM -------------------- */
const $ = (sel) => document.querySelector(sel);
const $$ = (sel) => Array.from(document.querySelectorAll(sel));

const el = {
  inputBits: $("#inputBits"),
  nVal: $("#nVal"),
  kVal: $("#kVal"),
  terminate: $("#terminate"),

  tabOct: $("#tabOct"),
  tabBin: $("#tabBin"),
  octBox: $("#octBox"),
  binBox: $("#binBox"),
  genOct1: $("#genOct1"),
  genOct2: $("#genOct2"),
  genBin1: $("#genBin1"),
  genBin2: $("#genBin2"),

  berRange: $("#berRange"),
  berNumber: $("#berNumber"),
  fixedErrors: $("#fixedErrors"),
  berControls: $("#berControls"),
  fixedControls: $("#fixedControls"),

  seed: $("#seed"),
  regenNoise: $("#regenNoise"),
  runBtn: $("#runBtn"),
  clearBtn: $("#clearBtn"),

  msg: $("#msg"),

  outOriginal: $("#outOriginal"),
  outEncoded: $("#outEncoded"),
  outNoisy: $("#outNoisy"),
  outDecoded: $("#outDecoded"),
  chErr: $("#chErr"),
  decErr: $("#decErr"),

  viterbiLog: $("#viterbiLog"),

  authorBtn: $("#authorBtn"),
  modalOverlay: $("#modalOverlay"),
  closeModal: $("#closeModal"),
  chartErrorsByPos: $("#chartErrorsByPos"),
  chartBerBar: $("#chartBerBar"),
  chartViterbiMetric: $("#chartViterbiMetric"),

};

/** -------------------- Переключение вкладок генераторов -------------------- */
function setGenTab(mode) {
  if (mode === "oct") {
    el.tabOct.classList.add("active");
    el.tabBin.classList.remove("active");
    el.octBox.classList.remove("hidden");
    el.binBox.classList.add("hidden");
  } else {
    el.tabBin.classList.add("active");
    el.tabOct.classList.remove("active");
    el.binBox.classList.remove("hidden");
    el.octBox.classList.add("hidden");
  }
}

el.tabOct.addEventListener("click", () => setGenTab("oct"));
el.tabBin.addEventListener("click", () => setGenTab("bin"));

/** -------------------- Выбор режима шума -------------------- */
function setNoiseMode(mode) {
  if (mode === "ber") {
    el.berControls.classList.remove("hidden");
    el.fixedControls.classList.add("hidden");
  } else {
    el.fixedControls.classList.remove("hidden");
    el.berControls.classList.add("hidden");
  }
}

$$('input[name="noiseMode"]').forEach(r => {
  r.addEventListener("change", () => setNoiseMode(r.value));
});

/** -------------------- Синхронизация BER slider <-> number -------------------- */
el.berRange.addEventListener("input", () => {
  el.berNumber.value = el.berRange.value;
});

el.berNumber.addEventListener("input", () => {
  let v = Number(el.berNumber.value);
  if (Number.isNaN(v)) v = 0;
  v = Math.max(0, Math.min(0.3, v));
  el.berNumber.value = v.toFixed(2);
  el.berRange.value = String(v);
});

/** -------------------- Сообщения (красиво) -------------------- */
function showMsg(text) {
  el.msg.textContent = text;
  el.msg.classList.remove("hidden");
}
function hideMsg() {
  el.msg.classList.add("hidden");
  el.msg.textContent = "";
}

/** -------------------- Seeded RNG (Mulberry32) --------------------
  Чтобы "новый шум" был воспроизводимым при заданном seed.
  Если seed пустой — используем текущее время (каждый раз разный).
------------------------------------------------------------------- */
function hashSeedToInt(seedStr) {
  // Простая строковая хеш-функция → 32-bit int
  let h = 2166136261;
  for (let i = 0; i < seedStr.length; i++) {
    h ^= seedStr.charCodeAt(i);
    h = Math.imul(h, 16777619);
  }
  return h >>> 0;
}

function mulberry32(a) {
  return function () {
    let t = a += 0x6D2B79F5;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

/** -------------------- Парсинг и валидация входа -------------------- */
function normalizeBits(str) {
  return (str || "").replace(/\s+/g, "");
}

function validateBits(str) {
  const s = normalizeBits(str);
  if (s.length === 0) return { ok: false, err: "Введи бинарную строку (например: 1011010010)." };
  if (!/^[01]+$/.test(s)) return { ok: false, err: "Бинарная строка должна содержать только 0 и 1 (пробелы можно)." };
  return { ok: true, bits: s };
}

function validateNK() {
  const n = Number(el.nVal.value);
  const k = Number(el.kVal.value);

  if (n !== 2 || k !== 1) {
    return { ok: false, err: "В этой версии поддерживается только (n,k) = (2,1). Поставь n=2 и k=1." };
  }
  return { ok: true, n, k };
}

/** -------------------- Генераторы: oct или bin --------------------
  Мы получаем два генератора в виде массивов бит (например 111 -> [1,1,1]).
  Длина генератора = K (constraint length).
------------------------------------------------------------------- */
function parseOctGen(octStr) {
  const s = (octStr || "").trim();
  if (!/^[0-7]+$/.test(s)) return null;
  const val = parseInt(s, 8);
  // Переводим в бинарную строку без ведущих нулей:
  const bin = val.toString(2);
  return bin;
}

function parseBinGen(binStr) {
  const s = (binStr || "").trim();
  if (!/^[01]+$/.test(s)) return null;
  return s;
}

function getGenerators() {
  // Можно вводить и там и там — мы берём "активную вкладку"
  const usingOct = !el.octBox.classList.contains("hidden");
  let g1bin = null, g2bin = null;

  if (usingOct) {
    const b1 = parseOctGen(el.genOct1.value);
    const b2 = parseOctGen(el.genOct2.value);
    if (!b1 || !b2) {
      return { ok: false, err: "Восьмеричные генераторы должны быть числами 0…7… (например 7 и 5)." };
    }
    g1bin = b1; g2bin = b2;

    // Для удобства: синхронизируем бинарные поля (видно будет при переключении)
    el.genBin1.value = g1bin;
    el.genBin2.value = g2bin;
  } else {
    const b1 = parseBinGen(el.genBin1.value);
    const b2 = parseBinGen(el.genBin2.value);
    if (!b1 || !b2) {
      return { ok: false, err: "Бинарные генераторы должны состоять только из 0/1 (например 111 и 101)." };
    }
    g1bin = b1; g2bin = b2;

    // Для удобства: синхронизируем oct поля (через parseInt)
    el.genOct1.value = parseInt(g1bin, 2).toString(8);
    el.genOct2.value = parseInt(g2bin, 2).toString(8);
  }

  // Длины должны совпадать (иначе K непонятен)
  if (g1bin.length !== g2bin.length) {
    return { ok: false, err: "Длины генераторов должны совпадать. Например: 111 и 101 (оба длины 3)." };
  }

  // Минимально разумно K>=2
  const K = g1bin.length;
  if (K < 2) {
    return { ok: false, err: "Constraint length K должен быть >= 2 (генератор длиной хотя бы 2 бита)." };
  }

  const g1 = g1bin.split("").map(x => Number(x));
  const g2 = g2bin.split("").map(x => Number(x));

  return { ok: true, K, g1, g2, g1bin, g2bin };
}

/** -------------------- Свёрточный кодер (k=1, n=2) --------------------
  register[0] = текущий входной бит (самый "новый")
  register[K-1] = самый "старый"
  Выходы:
   y1 = XOR(register[i] & g1[i] по i)
   y2 = XOR(register[i] & g2[i] по i)
------------------------------------------------------------------- */
function convEncode(inputBits, g1, g2, terminate) {
  const K = g1.length;
  const reg = new Array(K).fill(0);

  // Если terminate=true — добавим хвост из K-1 нулей
  const tail = terminate ? "0".repeat(K - 1) : "";
  const bits = (inputBits + tail).split("").map(Number);

  const out = []; // массив 0/1 длины 2*T
  for (const b of bits) {
    // сдвиг: новый бит в начало
    reg.pop();
    reg.unshift(b);

    const y1 = xorDot(reg, g1);
    const y2 = xorDot(reg, g2);

    out.push(y1, y2);
  }

  return {
    encoded: out,           // массив бит
    encodedStr: out.join(""),
    K,
    totalInputBits: bits.length, // включая хвост
    tailLen: terminate ? (K - 1) : 0
  };
}

function xorDot(reg, g) {
  let acc = 0;
  for (let i = 0; i < reg.length; i++) {
    if (g[i] === 1) acc ^= reg[i];
  }
  return acc;
}

/** -------------------- Канал: внесение ошибок --------------------
  Возвращаем noisyBits и errorMask (где 1 означает "бит перевёрнут")
------------------------------------------------------------------- */
function applyNoise(encodedBits, mode, ber, fixedCount, rng) {
  const n = encodedBits.length;
  const noisy = encodedBits.slice();
  const mask = new Array(n).fill(0);

  if (mode === "ber") {
    for (let i = 0; i < n; i++) {
      if (rng() < ber) {
        noisy[i] ^= 1;
        mask[i] = 1;
      }
    }
  } else {
    // fixedCount: выберем уникальные позиции
    const count = Math.max(0, Math.min(n, fixedCount));
    const chosen = new Set();
    while (chosen.size < count) {
      const idx = Math.floor(rng() * n);
      chosen.add(idx);
    }
    for (const idx of chosen) {
      noisy[idx] ^= 1;
      mask[idx] = 1;
    }
  }

  return { noisy, mask };
}

/** -------------------- Витерби (hard decision) --------------------
  Состояния: 2^(K-1), где K = длина генераторов.
  state хранит K-1 предыдущих бит (без текущего входа).
  При входе u (0/1) формируем регистр [u, ...stateBits], считаем выход (2 бита),
  nextState = (u + stateBits[0..K-3]) (сдвиг)
------------------------------------------------------------------- */
function viterbiDecode(receivedBits, g1, g2, terminate, origLen, tailLen) {
  const K = g1.length;
  const m = K - 1;
  const numStates = 1 << m;

  // Сколько "символов" по 2 бита
  const T = Math.floor(receivedBits.length / 2);

  // pathMetric[t][s] нам не надо хранить полностью — достаточно текущего и следующего
  const INF = 1e9;
  let pm = new Array(numStates).fill(INF);
  let pmNext = new Array(numStates).fill(INF);

  // Для traceback: predecessor[t][s] и chosenBit[t][s]
  const pred = Array.from({ length: T }, () => new Array(numStates).fill(0));
  const predBit = Array.from({ length: T }, () => new Array(numStates).fill(0));

  // Стартовое состояние 0 (все нули)
  pm[0] = 0;

  // Упрощённый лог: на каждом t запомним лучший state и его метрику
  const briefLog = [];

  for (let t = 0; t < T; t++) {
    pmNext.fill(INF);

    const r0 = receivedBits[2 * t];
    const r1 = receivedBits[2 * t + 1];

    for (let s = 0; s < numStates; s++) {
      const baseMetric = pm[s];
      if (baseMetric >= INF) continue;

      // Два возможных перехода: u=0 и u=1
      for (let u = 0; u <= 1; u++) {
        const { out0, out1, nextState } = trellisStep(s, u, g1, g2, m);

        const bm = (out0 !== r0) + (out1 !== r1); // метрика Хэмминга для пары
        const cand = baseMetric + bm;

        // Если нашли лучший путь в nextState — обновляем
        if (cand < pmNext[nextState]) {
          pmNext[nextState] = cand;
          pred[t][nextState] = s;
          predBit[t][nextState] = u;
        }
      }
    }

    // Зафиксируем лучший state на этом шаге (для таблицы)
    let bestS = 0;
    let bestM = pmNext[0];
    for (let s = 1; s < numStates; s++) {
      if (pmNext[s] < bestM) {
        bestM = pmNext[s];
        bestS = s;
      }
    }

    briefLog.push({
      t: t + 1,
      received: `${r0}${r1}`,
      bestState: bestS,
      metric: bestM,
      bit: predBit[t][bestS],
    });

    // Переходим к следующему шагу
    [pm, pmNext] = [pmNext, pm];
  }

  // Конечное состояние:
  // - если terminate=true: логично закончить в state=0
  // - иначе: берём минимальную метрику среди всех states
  let finalState = 0;
  if (!terminate) {
    let bestS = 0;
    let bestM = pm[0];
    for (let s = 1; s < numStates; s++) {
      if (pm[s] < bestM) {
        bestM = pm[s];
        bestS = s;
      }
    }
    finalState = bestS;
  }

  // Traceback: идём назад и собираем биты
  const decoded = new Array(T).fill(0);
  let s = finalState;
  for (let t = T - 1; t >= 0; t--) {
    const u = predBit[t][s];
    decoded[t] = u;
    s = pred[t][s];
  }

  // decoded сейчас длины T (включая хвост, если он был)
  // Уберём хвост, чтобы сравнивать с исходной строкой
  const decodedUseful = decoded.slice(0, origLen);

  return {
    decodedBits: decodedUseful,
    decodedStr: decodedUseful.join(""),
    briefLog,
  };
}

function trellisStep(state, u, g1, g2, m) {
  // Разложим state в биты длины m (старшие/младшие — не важно, если согласованно)
  // Будем считать так: stateBits[0] — "самый новый" из прошлых (рядом с входом)
  const stateBits = intToBits(state, m);

  // Регистр длины K: [u, ...stateBits]
  const reg = [u, ...stateBits];

  const out0 = xorDot(reg, g1);
  const out1 = xorDot(reg, g2);

  // nextStateBits: [u, stateBits[0..m-2]]
  const nextBits = [u, ...stateBits.slice(0, m - 1)];
  const nextState = bitsToInt(nextBits);

  return { out0, out1, nextState };
}

function intToBits(x, len) {
  const bits = new Array(len).fill(0);
  for (let i = 0; i < len; i++) {
    bits[i] = (x >> i) & 1; // i=0 — младший бит
  }
  // Переведём в "удобный" порядок: [новее, ..., старее]
  // Здесь сделаем просто reverse, чтобы было визуально привычнее (не критично)
  return bits.reverse();
}

function bitsToInt(bits) {
  // bits: [новее...старее], переведём обратно в int
  let x = 0;
  const b = bits.slice().reverse(); // теперь младший бит первый
  for (let i = 0; i < b.length; i++) x |= (b[i] & 1) << i;
  return x >>> 0;
}

/** -------------------- Подсветка отличий -------------------- */
function renderWithDiff(aStr, bStr) {
  // Возвращаем HTML, где символы, отличающиеся от bStr, подсвечены
  const L = Math.max(aStr.length, bStr.length);
  let html = "";
  for (let i = 0; i < L; i++) {
    const ca = aStr[i] ?? "";
    const cb = bStr[i] ?? "";
    if (ca === "") continue;
    const bad = (cb !== "" && ca !== cb);
    html += `<span class="${bad ? "bit-bad" : "bit-ok"}">${escapeHtml(ca)}</span>`;
  }
  return html;
}

function escapeHtml(s) {
  return String(s)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#039;");
}

function groupPairs(bitStr) {
  // Для закодированной/искажённой строк: "110010..." -> "11 00 10 ..."
  const s = bitStr;
  const out = [];
  for (let i = 0; i < s.length; i += 2) out.push(s.slice(i, i + 2));
  return out.join(" ");
}

/** -------------------- Проценты ошибок -------------------- */
function bitErrorRate(aBits, bBits) {
  const n = Math.min(aBits.length, bBits.length);
  if (n === 0) return { errors: 0, total: 0, pct: 0 };
  let e = 0;
  for (let i = 0; i < n; i++) if (aBits[i] !== bBits[i]) e++;
  return { errors: e, total: n, pct: (e / n) * 100 };
}

/** -------------------- Таблица лога Витерби -------------------- */
function renderViterbiLog(rows) {
  if (!rows || rows.length === 0) {
    el.viterbiLog.innerHTML = `<tr><td colspan="5" class="muted">Нет данных.</td></tr>`;
    return;
  }

  const html = rows.map(r => {
    return `
      <tr>
        <td>${r.t}</td>
        <td class="mono">${r.received}</td>
        <td class="mono">${r.bestState}</td>
        <td class="mono">${r.metric}</td>
        <td class="mono">${r.bit}</td>
      </tr>
    `;
  }).join("");

  el.viterbiLog.innerHTML = html;
}

/** -------------------- Глобальное состояние "шума" --------------------
  Чтобы кнопка "Сгенерировать новый шум" меняла ошибки, но не трогала всё остальное.
------------------------------------------------------------------- */
let lastNoise = null; // { mask, noisyBits, seedUsed }
/** ===================== Графики (Chart.js) ===================== */
let chartErrorsByPos = null;
let chartBerBar = null;
let chartViterbiMetric = null;
// Ограничение, чтобы графики не становились "бесконечными"
const MAX_POINTS = 120;
const MAX_METRIC_POINTS = 200;

function downsampleArray(arr, maxPoints) {
  if (arr.length <= maxPoints) return { data: arr.slice(), labels: arr.map((_, i) => i + 1) };
  const step = arr.length / maxPoints;
  const out = [];
  const labels = [];
  for (let i = 0; i < maxPoints; i++) {
    const idx = Math.floor(i * step);
    out.push(arr[idx]);
    labels.push(idx + 1);
  }
  return { data: out, labels };
}

function aggregateMaskToRate(mask, maxBars) {
  // Если маска длинная — агрегируем в окна и считаем долю ошибок в окне (0..1)
  if (mask.length <= maxBars) {
    return { values: mask.slice(), labels: mask.map((_, i) => String(i + 1)), isAggregated: false };
  }

  const windowSize = Math.ceil(mask.length / maxBars);
  const windows = Math.ceil(mask.length / windowSize);

  const values = [];
  const labels = [];

  for (let w = 0; w < windows; w++) {
    const start = w * windowSize;
    const end = Math.min(mask.length, start + windowSize);
    let sum = 0;
    for (let i = start; i < end; i++) sum += mask[i];
    values.push(sum / (end - start));
    labels.push(`${start + 1}-${end}`);
  }

  return { values, labels, isAggregated: true };
}

function ensureChartsReady() {
  if (typeof Chart === "undefined") return;
  if (!el.chartErrorsByPos || !el.chartBerBar || !el.chartViterbiMetric) return;
  if (chartErrorsByPos && chartBerBar && chartViterbiMetric) return;

  // 1) Ошибки по позициям (аккуратный bar)
  chartErrorsByPos = new Chart(el.chartErrorsByPos, {
    type: "bar",
    data: {
      labels: [],
      datasets: [
        { label: "До Витерби (канал)", data: [] },
        { label: "После Витерби", data: [] },
      ],
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      animation: false,
      scales: {
        y: {
          min: 0,
          max: 1,
          title: { display: true, text: "Ошибка (0/1) или доля ошибок в окне" },
          grid: { display: false },
        },
        x: {
          ticks: { maxRotation: 0, autoSkip: true },
          title: { display: true, text: "Позиция бита / окно" },
          grid: { display: false },
        },
      },
      plugins: {
        legend: { display: true },
        tooltip: {
          callbacks: {
            label: (ctx) => {
                const v = Number(ctx.raw);
                if (v === 0 || v === 1) return `${ctx.dataset.label}: ${v}`;
                return `${ctx.dataset.label}: ${(v * 100).toFixed(1)}%`;
            },

          },
        },
      },
    },
  });

  // 2) BER до/после (bar)
  chartBerBar = new Chart(el.chartBerBar, {
    type: "bar",
    data: {
      labels: ["До декодирования", "После Витерби"],
      datasets: [{ label: "BER (%)", data: [0, 0] }],
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      animation: false,
      scales: {
        y: { min: 0, max: 10, title: { display: true, text: "BER (%)" } },
      },
      plugins: { legend: { display: false } },
    },
  });

  // 3) Метрика Витерби (line)
  chartViterbiMetric = new Chart(el.chartViterbiMetric, {
    type: "line",
    data: {
      labels: [],
      datasets: [{ label: "Лучшая метрика", data: [], pointRadius: 0, tension: 0.15 }],
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      animation: false,
      scales: {
       x: {
           title: { display: true, text: "Шаг (t)" },
           ticks: { autoSkip: true, maxRotation: 0 },
       },

        y: { title: { display: true, text: "Метрика" } },
      },
      plugins: { legend: { display: true } },
    },
  });
}


function updateCharts({ errMaskBefore, errMaskAfter, berBeforePct, berAfterPct, viterbiMetricByStep }) {
  ensureChartsReady();
  if (!chartErrorsByPos || !chartBerBar || !chartViterbiMetric) return;

  // --- 1) Ошибки по позициям (не бесконечно: либо 0/1, либо доля по окнам)
  const a = aggregateMaskToRate(errMaskBefore, MAX_POINTS);
  const b = aggregateMaskToRate(errMaskAfter, MAX_POINTS);

  // Общие labels (берём тот набор, который длиннее)
  const labels = a.labels.length >= b.labels.length ? a.labels : b.labels;

  function padTo(labels, srcLabels, values) {
    const m = new Map();
    for (let i = 0; i < srcLabels.length; i++) m.set(srcLabels[i], values[i]);
    return labels.map(l => (m.has(l) ? m.get(l) : null));
  }

  chartErrorsByPos.data.labels = labels;
  chartErrorsByPos.data.datasets[0].data = padTo(labels, a.labels, a.values);
  chartErrorsByPos.data.datasets[1].data = padTo(labels, b.labels, b.values);
  chartErrorsByPos.update();

  // --- 2) BER до/после (аккуратный масштаб)
  chartBerBar.data.datasets[0].data = [berBeforePct, berAfterPct];
  const top = Math.max(5, Math.ceil(Math.max(berBeforePct, berAfterPct) / 5) * 5);
  chartBerBar.options.scales.y.max = top;
  chartBerBar.update();

  // --- 3) Метрика Витерби по шагам (downsample, чтобы не был бесконечным)
  const ds = downsampleArray(viterbiMetricByStep, MAX_METRIC_POINTS);
  chartViterbiMetric.data.labels = ds.labels;
  chartViterbiMetric.data.datasets[0].data = ds.data;
  chartViterbiMetric.update();
}
function resetCharts() {
  ensureChartsReady();

  if (chartErrorsByPos) {
    chartErrorsByPos.data.labels = [];
    chartErrorsByPos.data.datasets.forEach(ds => (ds.data = []));
    chartErrorsByPos.update();
  }

  if (chartBerBar) {
    chartBerBar.data.datasets[0].data = [0, 0];
    chartBerBar.update();
  }

  if (chartViterbiMetric) {
    chartViterbiMetric.data.labels = [];
    chartViterbiMetric.data.datasets[0].data = [];
    chartViterbiMetric.update();
  }
}




/** -------------------- Основной запуск симуляции -------------------- */
function runSimulation({ regenerateNoise = false } = {}) {
  hideMsg();

  // 1) Валидация входа
  const vb = validateBits(el.inputBits.value);
  if (!vb.ok) return showMsg(vb.err);

  const nk = validateNK();
  if (!nk.ok) return showMsg(nk.err);

  const gens = getGenerators();
  if (!gens.ok) return showMsg(gens.err);

  const inputStr = vb.bits;
  const terminate = !!el.terminate.checked;

  // 2) Кодирование
  const enc = convEncode(inputStr, gens.g1, gens.g2, terminate);

  // 3) RNG
  // Если seed пустой — добавим "соль" = текущее время, чтобы каждый запуск отличался
  const userSeed = (el.seed.value || "").trim();
  const baseSeed = userSeed.length ? hashSeedToInt(userSeed) : (Date.now() >>> 0);
  // Чтобы "новый шум" менялся даже с тем же seed — сделаем дополнительный смещение
  // (если regenerateNoise=true — меняем offset)
  const noiseOffset = regenerateNoise ? (Math.floor(Math.random() * 1e9) >>> 0) : 0;
  const seedUsed = (baseSeed ^ noiseOffset) >>> 0;
  const rng = mulberry32(seedUsed);

  // 4) Режим шума
  const mode = ($$('input[name="noiseMode"]').find(r => r.checked)?.value) || "ber";
  const ber = Math.max(0, Math.min(0.3, Number(el.berNumber.value)));
  const fixedCount = Math.max(0, Math.min(20, Number(el.fixedErrors.value)));

  // 5) Применение шума
  // Если пользователь нажал "Запустить симуляцию" — шум генерируем (и запоминаем).
  // Если он нажал "Сгенерировать новый шум" — шум всегда новый.
  // Если regenerateNoise=false и lastNoise уже есть — можно использовать lastNoise
  // (но мы делаем проще: каждый "Запуск" генерирует шум, а кнопка "Новый шум" тоже).
  const noise = applyNoise(enc.encoded, mode, ber, fixedCount, rng);

  lastNoise = { noisyBits: noise.noisy, mask: noise.mask, seedUsed };

  // 6) Декодирование Витерби
  const vit = viterbiDecode(
    noise.noisy,
    gens.g1,
    gens.g2,
    terminate,
    inputStr.length,
    enc.tailLen
  );

  // 7) Вывод 4 строк
  el.outOriginal.innerHTML = renderWithDiff(inputStr, vit.decodedStr);
  el.outEncoded.innerHTML = renderWithDiff(groupPairs(enc.encodedStr), groupPairs(enc.encodedStr)); // без ошибок
  el.outNoisy.innerHTML = renderWithDiff(groupPairs(noise.noisy.join("")), groupPairs(enc.encodedStr));
  el.outDecoded.innerHTML = renderWithDiff(vit.decodedStr, inputStr);

  // 8) Ошибки (%)
  const ch = bitErrorRate(enc.encoded, noise.noisy);
  const de = bitErrorRate(inputStr.split("").map(Number), vit.decodedBits);

  el.chErr.textContent = `${ch.errors}/${ch.total} = ${ch.pct.toFixed(2)}%`;
  el.decErr.textContent = `${de.errors}/${de.total} = ${de.pct.toFixed(2)}%`;
    // 8.5) Данные для графиков
  const errMaskBefore = noise.mask.slice();

  const origBitsArr = inputStr.split("").map(Number);
  const decBitsArr = vit.decodedBits;
  const errMaskAfter = origBitsArr.map((b, i) => (b !== (decBitsArr[i] ?? 0) ? 1 : 0));

  const viterbiMetricByStep = (vit.briefLog || []).map(r => r.metric);

  updateCharts({
    errMaskBefore,
    errMaskAfter,
    berBeforePct: ch.pct,
    berAfterPct: de.pct,
    viterbiMetricByStep,
  });


  // 9) Лог Витерби
  renderViterbiLog(vit.briefLog);
}

/** -------------------- Очистка -------------------- */
function clearAll() {
  hideMsg();
  el.outOriginal.textContent = "—";
  el.outEncoded.textContent = "—";
  el.outNoisy.textContent = "—";
  el.outDecoded.textContent = "—";
  el.chErr.textContent = "—";
  el.decErr.textContent = "—";
  el.viterbiLog.innerHTML = `<tr><td colspan="5" class="muted">Запусти симуляцию, чтобы увидеть лог.</td></tr>`;
  lastNoise = null;
  resetCharts();

}

/** -------------------- События кнопок -------------------- */
el.runBtn.addEventListener("click", () => runSimulation({ regenerateNoise: false }));
el.regenNoise.addEventListener("click", () => runSimulation({ regenerateNoise: true }));
el.clearBtn.addEventListener("click", clearAll);

/** -------------------- Модалка автора -------------------- */
function openModal() {
  el.modalOverlay.classList.remove("hidden");
}
function closeModal() {
  el.modalOverlay.classList.add("hidden");
}
el.authorBtn.addEventListener("click", openModal);
el.closeModal.addEventListener("click", closeModal);
el.modalOverlay.addEventListener("click", (e) => {
  if (e.target === el.modalOverlay) closeModal();
});
document.addEventListener("keydown", (e) => {
  if (e.key === "Escape") closeModal();
});

/** -------------------- Автовалидация ввода (мягкая) -------------------- */
el.inputBits.addEventListener("input", () => {
  const s = normalizeBits(el.inputBits.value);
  // Если есть запрещённые символы — покажем, но не мешаем печатать
  if (s.length > 0 && !/^[01]+$/.test(s)) {
    showMsg("В бинарной строке допустимы только 0 и 1 (пробелы можно).");
  } else {
    hideMsg();
  }
});

/** -------------------- Стартовая демонстрация --------------------
  Чтобы пример по умолчанию "сразу работал" — запустим симуляцию один раз.
  (Пользователь всё равно может нажать запуск ещё раз.)
------------------------------------------------------------------- */
setTimeout(() => {
  try { runSimulation({ regenerateNoise: false }); } catch (e) { /* молча */ }
}, 80);
