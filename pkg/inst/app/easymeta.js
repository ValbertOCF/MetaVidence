/* ============================================================
   MetaVidence, usability enhancements
   - converts each text-only step indicator into a visual stepper
   - groups the parameters step into collapsible sections
   - scrolls to the top whenever the user changes page or step
   ============================================================ */

(function () {
  'use strict';

  function scrollToTop() {
    window.scrollTo({ top: 0, behavior: 'smooth' });
  }

  /* ---------- visual stepper -------------------------------- */

  var STEP_LABELS = ['Data', 'Parameters', 'Results'];

  function buildStepper(container) {
    var holder = document.createElement('div');
    holder.className = 'em-stepper';
    holder.setAttribute('aria-hidden', 'true');

    STEP_LABELS.forEach(function (label, i) {
      if (i > 0) {
        var line = document.createElement('span');
        line.className = 'em-step-line';
        holder.appendChild(line);
      }
      var step = document.createElement('span');
      step.className = 'em-step';

      var dot = document.createElement('span');
      dot.className = 'em-step-dot';
      dot.textContent = String(i + 1);

      var text = document.createElement('span');
      text.className = 'em-step-label';
      text.textContent = label;

      step.appendChild(dot);
      step.appendChild(text);
      holder.appendChild(step);
    });

    container.appendChild(holder);
    return holder;
  }

  function updateStepper(container) {
    var textEl = container.querySelector('.shiny-text-output');
    var stepper = container.querySelector('.em-stepper');
    if (!textEl || !stepper) return;

    var match = (textEl.textContent || '').match(/Step\s+(\d)/i);
    var current = match ? parseInt(match[1], 10) : 1;

    var steps = stepper.querySelectorAll('.em-step');
    var lines = stepper.querySelectorAll('.em-step-line');

    Array.prototype.forEach.call(steps, function (step, idx) {
      var n = idx + 1;
      step.classList.toggle('is-active', n === current);
      step.classList.toggle('is-done', n < current);
      var dot = step.querySelector('.em-step-dot');
      if (dot) dot.textContent = n < current ? '✓' : String(n);
    });

    Array.prototype.forEach.call(lines, function (line, idx) {
      line.classList.toggle('is-done', idx + 1 < current);
    });
  }

  function enhanceSteppers() {
    var indicators = document.querySelectorAll('.step-indicator');
    Array.prototype.forEach.call(indicators, function (el) {
      if (el.getAttribute('data-em-ready')) {
        updateStepper(el);
        return;
      }
      el.setAttribute('data-em-ready', '1');
      buildStepper(el);
      updateStepper(el);

      var textEl = el.querySelector('.shiny-text-output');
      if (textEl) {
        new MutationObserver(function () {
          updateStepper(el);
        }).observe(textEl, { childList: true, characterData: true, subtree: true });
      }
    });
  }

  /* ---------- parameter sections ------------------------------
     Splits each .parameter-grid into collapsible groups:
       1. Outcome settings            (always open)
       2. Statistical settings        (collapsed, advanced)
       3. Forest plot customization   (collapsed, cosmetic only)
     Subgroup and meta-regression are not here: those columns are chosen in
     step 3, inside the result card, in single-outcome and in batch mode alike.
     Grouping is based on the Shiny input ids inside each field,
     so it works for every module without touching the R markup.
     ------------------------------------------------------------ */

  var FOREST_RE = /(col_square|col_square_lines|col_points|col_a$|col_b$|forest_cols|forest_sort)/;
  /* What the outcome IS: its name, which direction is bad, the effect measure,
     how the arms are called, which two tests are being compared, and which
     treatment is the reference. Everything else describes how the model is
     estimated and lives in the advanced section. A module can legitimately end
     up with no advanced fields left. The empty section is then hidden by
     pruneEmptyParameterSections(). */
  var OUTCOME_RE = /(_outcome[\s-]|_outcome$|_outcome_direction|_sm[\s-]|_sm$|_label_e|_label_c|_test_a|_test_b|_reference|_small_values)/;

  function fieldIds(child) {
    var ids = [];
    if (child.id) ids.push(child.id);
    var tagged = child.querySelectorAll('[id]');
    Array.prototype.forEach.call(tagged, function (el) {
      if (el.id) ids.push(el.id);
    });
    return ids.join(' ');
  }

  function buildSection(title, hint, open) {
    var details = document.createElement('details');
    details.className = 'em-param-section';
    if (open) details.open = true;

    var summary = document.createElement('summary');
    summary.className = 'em-param-summary';

    var titleEl = document.createElement('span');
    titleEl.className = 'em-param-title';
    titleEl.textContent = title;

    var hintEl = document.createElement('span');
    hintEl.className = 'em-param-hint';
    hintEl.textContent = hint;

    var toggleEl = document.createElement('span');
    toggleEl.className = 'em-param-toggle';

    summary.appendChild(titleEl);
    summary.appendChild(hintEl);
    summary.appendChild(toggleEl);
    details.appendChild(summary);

    var grid = document.createElement('div');
    grid.className = 'em-param-grid';
    details.appendChild(grid);

    return details;
  }

  function organizeParameterGrids() {
    var grids = document.querySelectorAll('.parameter-grid');
    Array.prototype.forEach.call(grids, function (grid) {
      if (grid.getAttribute('data-em-grouped')) return;
      grid.setAttribute('data-em-grouped', '1');

      var children = Array.prototype.slice.call(grid.children);
      if (children.length < 4) return;

      var outcome = [];
      var stat = [];
      var forest = [];

      children.forEach(function (child) {
        var ids = fieldIds(child);
        if (FOREST_RE.test(ids)) {
          forest.push(child);
        } else if (OUTCOME_RE.test(ids)) {
          outcome.push(child);
        } else {
          stat.push(child);
        }
      });

      if (!forest.length) return;

      grid.classList.add('em-param-sections');

      var sections = [];
      if (outcome.length) {
        sections.push({ items: outcome, el: buildSection('Outcome settings', 'What you are measuring and how the groups are named', true) });
      }
      if (stat.length) {
        sections.push({ items: stat, el: buildSection('Statistical settings (advanced)', 'Model, estimation method and prediction interval', false) });
      }
      if (forest.length) {
        sections.push({ items: forest, el: buildSection('Forest plot customization', 'Colors and extra plot columns, optional', false) });
      }
      sections.forEach(function (section) {
        var target = section.el.querySelector('.em-param-grid');
        section.items.forEach(function (item) {
          target.appendChild(item);
        });
        grid.appendChild(section.el);
      });
    });
  }

  /* ---------- data-entry cards --------------------------------
     Rebuilds every card that offers both a file upload and a paste
     area (the "Single outcome" card of each module) into a tabbed
     importer: Upload file / Paste data / Example. The .xlsx template
     button rides along in the Upload panel, and the required-columns
     guide sits between the tab strip and the panels.
     The long "required columns" guide becomes a collapsible block.
     Only DOM nodes are moved, so all Shiny inputs keep their bindings.
     ------------------------------------------------------------ */

  var TAB_ICONS = {
    upload: '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 16V4m0 0L7 9m5-5l5 5"/><path d="M4 20h16"/></svg>',
    paste: '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="6" y="5" width="12" height="16" rx="2"/><path d="M9 5V3h6v2"/><path d="M9 11h6M9 15h4"/></svg>',
    example: '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 3l1.9 5.1L19 10l-5.1 1.9L12 17l-1.9-5.1L5 10l5.1-1.9z"/><path d="M19 15l.9 2.1L22 18l-2.1.9L19 21l-.9-2.1L16 18l2.1-.9z"/></svg>'
  };

  function buildInputTab(def, onClick) {
    var tab = document.createElement('button');
    tab.type = 'button';
    tab.className = 'em-input-tab';
    tab.innerHTML = (TAB_ICONS[def.key] || '') + '<span>' + def.label + '</span>';
    tab.addEventListener('click', onClick);
    return tab;
  }

  function enhanceDataCards() {
    var cards = document.querySelectorAll('.analysis-card');
    Array.prototype.forEach.call(cards, function (card) {
      if (card.getAttribute('data-em-input')) return;

      var fileGroup = null;
      var pasteGroup = null;
      var containers = card.querySelectorAll('.shiny-input-container');
      Array.prototype.forEach.call(containers, function (group) {
        if (!fileGroup && group.querySelector('input[type=file]')) fileGroup = group;
        if (!pasteGroup && group.querySelector('textarea')) pasteGroup = group;
      });
      if (!fileGroup || !pasteGroup) return;

      card.setAttribute('data-em-input', '1');
      card.classList.add('em-input-enhanced');

      var exampleBtn = card.querySelector('.action-button.primary-action:not(.outline-action)');
      var templateBtn = card.querySelector('a.shiny-download-link');
      var pasteBtn = card.querySelector('.action-button.outline-action');
      var reqCols = card.querySelector('.required-columns');
      var hr = card.querySelector('hr');
      if (hr) hr.style.display = 'none';

      var anchor = exampleBtn || fileGroup;
      var tabs = document.createElement('div');
      tabs.className = 'em-input-tabs';
      var panels = document.createElement('div');
      panels.className = 'em-input-panels';
      card.insertBefore(tabs, anchor);
      card.insertBefore(panels, tabs.nextSibling);

      function makePanel(name) {
        var panel = document.createElement('div');
        panel.className = 'em-input-panel';
        panel.setAttribute('data-name', name);
        panels.appendChild(panel);
        return panel;
      }

      /* The .xlsx template belongs here, next to the file picker: whoever is
         about to upload a spreadsheet is exactly who needs the blank one. */
      var uploadPanel = makePanel('upload');
      var uploadRow = document.createElement('div');
      uploadRow.className = 'em-upload-row';
      uploadRow.appendChild(fileGroup);
      if (templateBtn) uploadRow.appendChild(templateBtn);
      uploadPanel.appendChild(uploadRow);

      var pastePanel = makePanel('paste');
      pastePanel.appendChild(pasteGroup);
      if (pasteBtn) pastePanel.appendChild(pasteBtn);

      var examplePanel = makePanel('example');
      var exampleHint = document.createElement('p');
      exampleHint.className = 'help-text em-example-hint';
      exampleHint.textContent = 'New here? Load a ready-made dataset with 12 studies to explore the whole workflow.';
      examplePanel.appendChild(exampleHint);
      var exampleRow = document.createElement('div');
      exampleRow.className = 'em-example-row';
      if (exampleBtn) exampleRow.appendChild(exampleBtn);
      examplePanel.appendChild(exampleRow);

      /* Right below the tabs, before the panels: the column guide applies to
         all three import routes, not only to whichever one is open. */
      if (reqCols) {
        var details = document.createElement('details');
        details.className = 'em-cols-details';
        var summary = document.createElement('summary');
        summary.className = 'em-cols-summary';
        summary.innerHTML = '<span class="em-cols-title">Required columns guide</span><span class="em-cols-chevron">▾</span>';
        details.appendChild(summary);
        card.insertBefore(details, panels);
        details.appendChild(reqCols);
      }

      var defs = [
        { key: 'upload', label: 'Upload file' },
        { key: 'paste', label: 'Paste data' },
        { key: 'example', label: 'Example' }
      ];

      function activate(key) {
        var buttons = tabs.querySelectorAll('.em-input-tab');
        Array.prototype.forEach.call(buttons, function (button, i) {
          button.classList.toggle('is-active', defs[i].key === key);
        });
        var panelList = panels.querySelectorAll('.em-input-panel');
        Array.prototype.forEach.call(panelList, function (panel) {
          panel.style.display = panel.getAttribute('data-name') === key ? '' : 'none';
        });
      }

      defs.forEach(function (def) {
        tabs.appendChild(buildInputTab(def, function () { activate(def.key); }));
      });

      activate('upload');
    });
  }

  /* ---------- statistical parameter tooltips ------------------
     A "?" icon is appended to parameter labels, keyed by the Shiny
     input id. Purely explanatory, nothing about the analysis
     changes. Some tips inspect the select's options so the same
     suffix (e.g. _sm) gets module-appropriate wording.
     ------------------------------------------------------------ */

  function smTip(input) {
    var opts = input && input.tagName === 'SELECT'
      ? Array.prototype.map.call(input.options, function (o) { return o.value; })
      : [];
    if (opts.indexOf('PLOGIT') !== -1) {
      return 'Scale on which the proportions are pooled. PLOGIT pools on the logit scale, PFT applies the Freeman-Tukey double arcsine transformation, PRAW pools the proportions untransformed.';
    }
    if (opts.indexOf('MRAW') !== -1) {
      return 'MRAW pools the means in the original measurement unit. MLN pools the means on the log scale.';
    }
    if (opts.indexOf('OR') !== -1 || opts.indexOf('RR') !== -1 || opts.indexOf('HR') !== -1) {
      return 'The pooled effect metric: RR = risk ratio, OR = odds ratio, RD = risk difference (absolute), HR = hazard ratio (time-to-event). Ratio measures are pooled on the log scale.';
    }
    if (opts.indexOf('SMD') !== -1) {
      return 'MD = mean difference, expressed in the original measurement unit. SMD = standardized mean difference (Hedges’ g), expressed in standard deviation units.';
    }
    return 'The effect metric that will be pooled across studies.';
  }

  function methodTip(input) {
    var opts = input && input.tagName === 'SELECT'
      ? Array.prototype.map.call(input.options, function (o) { return o.value.toUpperCase(); })
      : [];
    if (opts.indexOf('MH') !== -1 || opts.indexOf('PETO') !== -1) {
      return 'How the study estimates are weighted and combined. Inverse uses inverse-variance weighting, MH uses the Mantel-Haenszel weights, Peto uses the one-step Peto odds ratio, GLMM fits a generalized linear mixed model to the counts.';
    }
    if (opts.indexOf('GLMM') !== -1) {
      return 'GLMM fits a generalized linear mixed model to the event counts. Inverse applies inverse-variance weighting to the transformed proportions.';
    }
    return 'How the individual study estimates are weighted and combined into the pooled result.';
  }

  var HELP_TIPS = [
    { re: /_outcome_direction$/, tip: 'Tells MetaVidence whether higher values of this outcome are good or bad. It only sets the “Favors experimental / Favors control” labels under the forest plot. It does not change any numbers.' },
    { re: /_outcome$/, tip: 'Free text used in plot titles and exported file names, for example “Mortality”.' },
    { re: /_model$/, tip: 'Fixed-effect assumes every study estimates the same true effect. Random-effects lets the true effect vary between studies. “Both” reports the two models side by side.' },
    { re: /_method_tau$/, tip: 'Estimator of the between-study variance (τ²), which drives the random-effects weights, I² and the prediction interval. REML is restricted maximum likelihood; DL is the DerSimonian-Laird estimator.' },
    { re: /_method_i2$/, tip: 'How I² is computed. “Q” uses the Higgins and Thompson definition, I² = (Q − df)/Q. “From tau-squared” derives I² from the estimated τ². The two can give different values on the same data.' },
    { re: /_method_random_ci$/, tip: 'How the confidence interval of the random-effects estimate is computed. “classic” is the Wald-type interval, “HK” the Hartung-Knapp interval, “KR” the Kenward-Roger interval.' },
    { re: /_method_predict$/, tip: 'Method used to compute the prediction interval. Only relevant when the prediction interval is enabled.' },
    { re: /_method$/, tip: methodTip },
    { re: /_sm$/, tip: smTip },
    { re: /_prediction$/, tip: 'Adds a prediction interval to the forest plot: the range where the true effect of a new study is expected to fall. Needs at least 3 studies.' },
    { re: /_label_e$/, tip: 'Label shown for the experimental (intervention) group in plots and tables.' },
    { re: /_label_c$/, tip: 'Label shown for the control (comparator) group in plots and tables.' },
    { re: /_reference$/, tip: 'The comparator treatment: every other treatment in the network is contrasted against it in the forest plot and league table.' },
    { re: /_small_values$/, tip: 'Declares which direction of your outcome counts as favourable: “good” when smaller values are desirable, “bad” when larger values are desirable. It affects rankograms and P-scores, not the pooled estimates.' },
    { re: /_test_a$/, tip: 'Which value of your “test” column is treated as Test A in the comparison, plots and summary tables.' },
    { re: /_test_b$/, tip: 'Which value of your “test” column is treated as Test B in the comparison, plots and summary tables.' },
    { re: /_subgroup$/, tip: 'Optional. Pick a categorical column from your data (e.g. Region) to pool each group separately and test whether the effect differs between groups.' },
    { re: /_metareg$/, tip: 'Optional. Pick a numeric column (e.g. mean age) to test whether it explains variation in the effect sizes, shown as a bubble plot with a regression test.' },
    { re: /_forest_cols$/, tip: 'Extra columns from your dataset to display next to the studies in the forest plot.' }
  ];

  function buildHelpIcon(text) {
    var icon = document.createElement('span');
    icon.className = 'em-help';
    icon.setAttribute('tabindex', '0');
    icon.setAttribute('role', 'note');
    icon.setAttribute('aria-label', text);
    icon.appendChild(document.createTextNode('?'));
    var tip = document.createElement('span');
    tip.className = 'em-help-tip';
    tip.textContent = text;
    icon.appendChild(tip);
    icon.addEventListener('click', function (event) {
      /* keep the click from activating the label and focusing the input */
      event.preventDefault();
      event.stopPropagation();
      icon.focus();
    });
    return icon;
  }

  function attachHelpIcons() {
    var labels = document.querySelectorAll('.parameter-grid label[for]');
    Array.prototype.forEach.call(labels, function (label) {
      if (label.getAttribute('data-em-help')) return;
      var forId = label.getAttribute('for');
      if (!forId) return;
      var entry = null;
      for (var i = 0; i < HELP_TIPS.length; i++) {
        if (HELP_TIPS[i].re.test(forId)) { entry = HELP_TIPS[i]; break; }
      }
      if (!entry) return;
      label.setAttribute('data-em-help', '1');
      var text = typeof entry.tip === 'function'
        ? entry.tip(document.getElementById(forId))
        : entry.tip;
      if (!text) return;
      label.appendChild(buildHelpIcon(text));
    });
  }

  /* ---------- download bridge --------------------------------
     Clicking a Shiny download link makes the browser fetch the URL
     from the browser process, which bypasses the shinylive service
     worker: the request reaches the static host, which has no such
     path, and the download fails as "file not available".

     Fetching from the page instead does go through the service
     worker, so we pull the bytes here and hand the browser a Blob.
     Harmless in a normally served app, where the same fetch simply
     hits the Shiny server.
     ------------------------------------------------------------ */

  function filenameFromResponse(response, url) {
    var disposition = response.headers.get('content-disposition') || '';
    var utf8 = disposition.match(/filename\*=UTF-8''([^;]+)/i);
    if (utf8) {
      try { return decodeURIComponent(utf8[1]); } catch (e) { /* fall through */ }
    }
    var plain = disposition.match(/filename="?([^";]+)"?/i);
    if (plain) return plain[1];
    var tail = url.split('?')[0].split('/').pop();
    return tail || 'download';
  }

  function saveBlob(blob, filename) {
    var objectUrl = URL.createObjectURL(blob);
    var tmp = document.createElement('a');
    tmp.href = objectUrl;
    tmp.download = filename;
    tmp.style.display = 'none';
    document.body.appendChild(tmp);
    tmp.click();
    document.body.removeChild(tmp);
    setTimeout(function () { URL.revokeObjectURL(objectUrl); }, 60000);
  }

  function installDownloadBridge() {
    document.addEventListener('click', function (event) {
      var link = event.target.closest && event.target.closest('a.shiny-download-link');
      if (!link || link.classList.contains('is-disabled')) return;
      var href = link.getAttribute('href');
      if (!href) return;

      event.preventDefault();
      var url = new URL(href, window.location.href).href;

      fetch(url)
        .then(function (response) {
          if (!response.ok) throw new Error('HTTP ' + response.status);
          var filename = filenameFromResponse(response, url);
          return response.blob().then(function (blob) { saveBlob(blob, filename); });
        })
        .catch(function (error) {
          /* Last resort: let the browser try the plain navigation. */
          console.error('MetaVidence download failed:', error);
          window.location.href = url;
        });
    });
  }

  /* ---------- wire-up ---------------------------------------- */

  document.addEventListener('DOMContentLoaded', function () {
    enhanceSteppers();
    organizeParameterGrids();
    enhanceDataCards();
    attachHelpIcons();
    installDownloadBridge();
  });

  if (window.jQuery) {
    window.jQuery(document).on('shiny:inputchanged', function (event) {
      if (!event || !event.name) return;
      if (event.name === 'pages' || /_steps$/.test(event.name)) {
        scrollToTop();
      }
    });

    /* step labels render after Shiny connects, refresh steppers */
    window.jQuery(document).on('shiny:value', function (event) {
      if (!event || !event.name) return;
      if (/_step_label$/.test(event.name)) {
        setTimeout(enhanceSteppers, 0);
      }
      if (/_data_check$/.test(event.name)) {
        setTimeout(function () { decorateDataCheck(event.name); }, 0);
      }
      if (/_status$/.test(event.name) && typeof event.value === 'string' && /loaded:|imported/i.test(event.value)) {
        setTimeout(function () { pulseNextButton(event.name); }, 0);
      }
    });
  }

  /* ---------- pulse the "Next" button after data loads -------- */

  function pulseNextButton(outputName) {
    var statusEl = document.getElementById(outputName);
    if (!statusEl) return;
    var pane = statusEl.closest('.tab-pane');
    if (!pane || !pane.classList.contains('active')) return;
    var next = pane.querySelector('.step-actions .run-action');
    if (!next || next.offsetParent === null) return;
    next.classList.remove('em-attention');
    void next.offsetWidth; /* restart the animation */
    next.classList.add('em-attention');
    setTimeout(function () { next.classList.remove('em-attention'); }, 2600);
  }

  /* ---------- data check table: colored pass/fail icons ------- */

  var CHECK_BAD_RE = /missing|must be|greater than|no rows|blank|error|cannot|invalid|not numeric|fix the/i;

  function classifyCheckRow(item, result) {
    var it = (item || '').toLowerCase();
    var r = (result || '').trim();
    var rl = r.toLowerCase();
    if (/problem/.test(it)) return rl === 'none' ? 'ok' : 'bad';
    if (/^status$/.test(it)) return 'neutral';
    if (CHECK_BAD_RE.test(rl)) return 'bad';
    if (/found$/.test(rl) || /^yes\b/.test(rl)) return 'ok';
    if (/^no\b/.test(rl)) return 'bad';
    if (/^none detected$/.test(rl)) return 'neutral';
    if (/^\d+$/.test(r)) return parseInt(r, 10) > 0 ? 'ok' : 'bad';
    return 'ok';
  }

  var CHECK_GLYPHS = { ok: '✓', bad: '✕', neutral: '•' };

  function decorateDataCheck(outputName) {
    var container = document.getElementById(outputName);
    if (!container) return;
    var rows = container.querySelectorAll('tbody tr');
    Array.prototype.forEach.call(rows, function (row) {
      var cells = row.querySelectorAll('td');
      if (cells.length < 2) return;
      var old = cells[1].querySelector('.em-check-ic');
      if (old) old.remove();
      var kind = classifyCheckRow(cells[0].textContent, cells[1].textContent);
      var icon = document.createElement('span');
      icon.className = 'em-check-ic is-' + kind;
      icon.textContent = CHECK_GLYPHS[kind];
      cells[1].insertBefore(icon, cells[1].firstChild);
    });
  }
})();
