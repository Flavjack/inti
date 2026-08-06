/* -------------------------------------------------------------------------
 * TARPUY application UI helpers
 * -------------------------------------------------------------------------
 * Scope: inst/tarpuy only.
 *
 * This file contains presentation-only browser behavior for TARPUY:
 * - localized scroll handling for previews and wide components;
 * - responsive viewport classes and CSS variables;
 * - safe restoration of page and component scroll positions;
 * - visual loading states;
 * - accessibility and focus assistance for Bootstrap/Shiny modals.
 *
 * It intentionally contains no experimental-design logic, no fieldbook data
 * processing, and no direct access to Google Sheets.
 * ---------------------------------------------------------------------- */

(function (window, document, $) {
  "use strict";

  if (window.TarpuyUI && window.TarpuyUI.initialized) {
    return;
  }

  var TarpuyUI = window.TarpuyUI || {};
  var root = document.documentElement;

  var SELECTORS = {
    googlePreview: ".gsheet-preview-wrapper",
    localScroll: [
      ".tarpuy-summary-scroll",
      ".sketch-preview-image",
      ".sketch-canvas-scroll",
      ".table-responsive",
      ".dataTables_scrollBody",
      "[data-tarpuy-scroll-container]"
    ].join(","),
    modal: ".modal",
    loadingTarget: "[data-tarpuy-loading-target]",
    disableWhileBusy: "[data-tarpuy-disable-while-busy]"
  };

  var state = {
    pageScroll: { x: 0, y: 0 },
    previewLockOwner: null,
    previewLockTimer: null,
    previewUnlockTimer: null,
    busyCount: 0,
    activeLoadingTargets: new Set(),
    tabScroll: new Map(),
    modalStack: [],
    resizeFrame: null,
    shinyHandlersRegistered: false,
    initialized: false
  };

  /* ---------------------------------------------------------------------
   * General utilities
   * ------------------------------------------------------------------ */

  function asElement(value) {
    if (!value) return null;
    if (value.nodeType === 1) return value;

    if (typeof value === "string") {
      try {
        return document.querySelector(value);
      } catch (error) {
        return null;
      }
    }

    return null;
  }

  function toNumber(value, fallback) {
    var number = Number(value);
    return Number.isFinite(number) ? number : fallback;
  }

  function currentScroll() {
    return {
      x: window.scrollX || window.pageXOffset || 0,
      y: window.scrollY || window.pageYOffset || 0
    };
  }

  function setAriaBusy(element, busy) {
    if (!element) return;

    if (busy) {
      element.setAttribute("aria-busy", "true");
    } else {
      element.removeAttribute("aria-busy");
    }
  }

  function safeFocus(element) {
    if (!element || typeof element.focus !== "function") return;

    try {
      element.focus({ preventScroll: true });
    } catch (error) {
      element.focus();
    }
  }

  function getTabKey(tabElement) {
    if (!tabElement) return null;

    return (
      tabElement.getAttribute("data-bs-target") ||
      tabElement.getAttribute("data-target") ||
      tabElement.getAttribute("href") ||
      tabElement.id ||
      null
    );
  }

  /* ---------------------------------------------------------------------
   * Responsive viewport behavior
   * ------------------------------------------------------------------ */

  function viewportMode(width) {
    if (width >= 1200) return "desktop";
    if (width >= 768) return "tablet";
    return "compact";
  }

  function updateViewportState() {
    var width = Math.max(
      document.documentElement.clientWidth || 0,
      window.innerWidth || 0
    );
    var height = Math.max(
      document.documentElement.clientHeight || 0,
      window.innerHeight || 0
    );
    var mode = viewportMode(width);

    root.style.setProperty("--tarpuy-vh", String(height * 0.01) + "px");
    root.style.setProperty("--tarpuy-viewport-width", String(width) + "px");
    root.style.setProperty("--tarpuy-viewport-height", String(height) + "px");
    root.setAttribute("data-tarpuy-viewport", mode);

    ["desktop", "tablet", "compact"].forEach(function (name) {
      root.classList.toggle("tarpuy-viewport-" + name, name === mode);
    });

    var detail = { width: width, height: height, mode: mode };
    document.dispatchEvent(
      new CustomEvent("tarpuy:viewportchange", { detail: detail })
    );
  }

  function scheduleViewportUpdate() {
    if (state.resizeFrame !== null) {
      window.cancelAnimationFrame(state.resizeFrame);
    }

    state.resizeFrame = window.requestAnimationFrame(function () {
      state.resizeFrame = null;
      updateViewportState();
      refreshScrollableContainers(document);
    });
  }

  /* ---------------------------------------------------------------------
   * Localized preview and component scrolling
   * ------------------------------------------------------------------ */

  function lockPageScroll(owner) {
    if (!owner || state.previewLockOwner === owner) return;

    window.clearTimeout(state.previewUnlockTimer);
    window.clearTimeout(state.previewLockTimer);

    state.previewLockTimer = window.setTimeout(function () {
      if (!document.documentElement.contains(owner)) return;

      state.pageScroll = currentScroll();
      state.previewLockOwner = owner;
      root.classList.add("gsheet-scroll-lock");
      owner.classList.add("is-scroll-active");
    }, 100);
  }

  function unlockPageScroll(owner, immediate) {
    window.clearTimeout(state.previewLockTimer);
    window.clearTimeout(state.previewUnlockTimer);

    var unlock = function () {
      if (owner && state.previewLockOwner && owner !== state.previewLockOwner) {
        return;
      }

      var saved = state.pageScroll;
      var activeOwner = state.previewLockOwner;

      root.classList.remove("gsheet-scroll-lock");

      if (activeOwner) {
        activeOwner.classList.remove("is-scroll-active");
      }

      state.previewLockOwner = null;

      window.requestAnimationFrame(function () {
        window.scrollTo(saved.x, saved.y);
      });
    };

    if (immediate) {
      unlock();
    } else {
      state.previewUnlockTimer = window.setTimeout(unlock, 180);
    }
  }

  function isMouseLikePointer(event) {
    return !event.pointerType || event.pointerType === "mouse" || event.pointerType === "pen";
  }

  function handlePreviewPointerEnter(event) {
    if (!isMouseLikePointer(event)) return;
    lockPageScroll(event.currentTarget);
  }

  function handlePreviewPointerLeave(event) {
    if (!isMouseLikePointer(event)) return;
    unlockPageScroll(event.currentTarget, false);
  }

  function canScroll(element, deltaX, deltaY) {
    var canScrollX = element.scrollWidth > element.clientWidth + 1;
    var canScrollY = element.scrollHeight > element.clientHeight + 1;

    var movesX = false;
    var movesY = false;

    if (canScrollX && deltaX !== 0) {
      movesX = deltaX < 0
        ? element.scrollLeft > 0
        : element.scrollLeft + element.clientWidth < element.scrollWidth - 1;
    }

    if (canScrollY && deltaY !== 0) {
      movesY = deltaY < 0
        ? element.scrollTop > 0
        : element.scrollTop + element.clientHeight < element.scrollHeight - 1;
    }

    return movesX || movesY;
  }

  function containWheel(event) {
    var container = event.target.closest(SELECTORS.localScroll);
    if (!container) return;

    if (canScroll(container, event.deltaX, event.deltaY)) {
      event.stopPropagation();
    }
  }

  function updateScrollClasses(container) {
    if (!container) return;

    var maxX = Math.max(0, container.scrollWidth - container.clientWidth);
    var maxY = Math.max(0, container.scrollHeight - container.clientHeight);
    var scrollableX = maxX > 1;
    var scrollableY = maxY > 1;

    container.classList.toggle("is-scrollable-x", scrollableX);
    container.classList.toggle("is-scrollable-y", scrollableY);
    container.classList.toggle("is-scroll-start", !scrollableX || container.scrollLeft <= 1);
    container.classList.toggle("is-scroll-end", !scrollableX || container.scrollLeft >= maxX - 1);
    container.classList.toggle("is-scroll-top", !scrollableY || container.scrollTop <= 1);
    container.classList.toggle("is-scroll-bottom", !scrollableY || container.scrollTop >= maxY - 1);

    if ((scrollableX || scrollableY) && !container.hasAttribute("tabindex")) {
      container.setAttribute("tabindex", "0");
    }
  }

  function refreshScrollableContainers(scope) {
    var parent = scope && scope.querySelectorAll ? scope : document;

    if (parent.matches && parent.matches(SELECTORS.localScroll)) {
      updateScrollClasses(parent);
    }

    parent.querySelectorAll(SELECTORS.localScroll).forEach(function (container) {
      updateScrollClasses(container);
    });
  }

  function handleLocalScroll(event) {
    var container = event.target.closest(SELECTORS.localScroll);
    if (container) updateScrollClasses(container);
  }

  function bindGooglePreview(preview) {
    if (!preview || preview.dataset.tarpuyScrollBound === "true") return;

    preview.dataset.tarpuyScrollBound = "true";
    preview.addEventListener("pointerenter", handlePreviewPointerEnter);
    preview.addEventListener("pointerleave", handlePreviewPointerLeave);
    preview.addEventListener("focusin", function () {
      lockPageScroll(preview);
    });
    preview.addEventListener("focusout", function (event) {
      if (!preview.contains(event.relatedTarget)) {
        unlockPageScroll(preview, false);
      }
    });

    var frame = preview.querySelector("iframe");
    if (frame) {
      frame.addEventListener("load", function () {
        preview.classList.add("is-loaded");
        preview.classList.remove("tarpuy-loading");
        setAriaBusy(preview, false);
      });
    }
  }

  function bindDynamicElements(scope) {
    var parent = scope && scope.querySelectorAll ? scope : document;

    parent.querySelectorAll(SELECTORS.googlePreview).forEach(bindGooglePreview);
    refreshScrollableContainers(parent);
  }

  /* ---------------------------------------------------------------------
   * Scroll restoration
   * ------------------------------------------------------------------ */

  function saveCurrentTabScroll(tabElement) {
    var key = getTabKey(tabElement);
    if (!key) return;
    state.tabScroll.set(key, currentScroll());
  }

  function restoreTabScroll(tabElement) {
    var key = getTabKey(tabElement);
    if (!key) return;

    var saved = state.tabScroll.get(key) || { x: 0, y: 0 };

    window.requestAnimationFrame(function () {
      window.scrollTo(saved.x, saved.y);
    });
  }

  function restoreScroll(options) {
    var settings = options || {};
    var behavior = settings.behavior === "smooth" ? "smooth" : "auto";
    var target = asElement(settings.selector);

    if (target) {
      target.scrollTo({
        left: toNumber(settings.x, target.scrollLeft),
        top: toNumber(settings.y, target.scrollTop),
        behavior: behavior
      });
      updateScrollClasses(target);
      return;
    }

    window.scrollTo({
      left: toNumber(settings.x, state.pageScroll.x),
      top: toNumber(settings.y, state.pageScroll.y),
      behavior: behavior
    });
  }

  function scrollToElement(options) {
    var settings = options || {};
    var target = asElement(settings.selector);
    if (!target) return;

    target.scrollIntoView({
      behavior: settings.behavior === "smooth" ? "smooth" : "auto",
      block: settings.block || "nearest",
      inline: settings.inline || "nearest"
    });
  }

  /* ---------------------------------------------------------------------
   * Loading states
   * ------------------------------------------------------------------ */

  function setLoading(target, loading) {
    var element = asElement(target);
    if (!element) return;

    var isLoading = loading !== false;

    element.classList.toggle("tarpuy-loading", isLoading);
    setAriaBusy(element, isLoading);

    if (isLoading) {
      state.activeLoadingTargets.add(element);
    } else {
      state.activeLoadingTargets.delete(element);
    }
  }

  function clearLoadingTargets() {
    state.activeLoadingTargets.forEach(function (element) {
      if (document.documentElement.contains(element)) {
        element.classList.remove("tarpuy-loading");
        setAriaBusy(element, false);
      }
    });

    state.activeLoadingTargets.clear();
  }

  function setGlobalBusy(busy) {
    if (busy) {
      state.busyCount += 1;
    } else {
      state.busyCount = Math.max(0, state.busyCount - 1);
    }

    var isBusy = state.busyCount > 0;
    root.classList.toggle("tarpuy-app-busy", isBusy);
    setAriaBusy(document.body, isBusy);

    document.querySelectorAll(SELECTORS.disableWhileBusy).forEach(function (element) {
      if (isBusy) {
        if (!element.hasAttribute("data-tarpuy-was-disabled")) {
          element.setAttribute("data-tarpuy-was-disabled", element.disabled ? "true" : "false");
        }
        element.disabled = true;
      } else {
        var wasDisabled = element.getAttribute("data-tarpuy-was-disabled") === "true";
        element.disabled = wasDisabled;
        element.removeAttribute("data-tarpuy-was-disabled");
      }
    });

    if (!isBusy) clearLoadingTargets();
  }

  function handleLoadingTrigger(event) {
    var trigger = event.target.closest(SELECTORS.loadingTarget);
    if (!trigger) return;

    var selector = trigger.getAttribute("data-tarpuy-loading-target");
    if (selector) setLoading(selector, true);
  }

  /* ---------------------------------------------------------------------
   * Modal support
   * ------------------------------------------------------------------ */

  function modalFocusable(modal) {
    return modal.querySelector(
      "[autofocus], .tarpuy-modal-primary, .modal-footer .btn-primary, " +
      ".modal-body input:not([disabled]), .modal-body select:not([disabled]), " +
      ".modal-body textarea:not([disabled]), .modal-body button:not([disabled]), " +
      ".modal-header .btn-close, .modal-header .close"
    );
  }

  function handleModalShow(event) {
    state.modalStack.push({
      modal: event.target,
      focus: document.activeElement,
      scroll: currentScroll()
    });
  }

  function handleModalShown(event) {
    var modal = event.target;
    modal.setAttribute("aria-modal", "true");
    modal.setAttribute("role", modal.getAttribute("role") || "dialog");

    window.requestAnimationFrame(function () {
      safeFocus(modalFocusable(modal));
      refreshScrollableContainers(modal);
    });
  }

  function handleModalHidden(event) {
    var modal = event.target;
    var index = -1;

    for (var i = state.modalStack.length - 1; i >= 0; i -= 1) {
      if (state.modalStack[i].modal === modal) {
        index = i;
        break;
      }
    }

    if (index === -1) return;

    var record = state.modalStack.splice(index, 1)[0];

    window.requestAnimationFrame(function () {
      window.scrollTo(record.scroll.x, record.scroll.y);

      if (record.focus && document.documentElement.contains(record.focus)) {
        safeFocus(record.focus);
      }
    });
  }

  /* ---------------------------------------------------------------------
   * Shiny message handlers
   * ------------------------------------------------------------------ */

  function registerShinyHandlers() {
    if (state.shinyHandlersRegistered) return;

    if (!window.Shiny || typeof window.Shiny.addCustomMessageHandler !== "function") {
      return;
    }

    state.shinyHandlersRegistered = true;

    window.Shiny.addCustomMessageHandler("tarpuy:set-loading", function (message) {
      var payload = message || {};
      setLoading(payload.selector, payload.loading !== false);
    });

    window.Shiny.addCustomMessageHandler("tarpuy:restore-scroll", function (message) {
      restoreScroll(message || {});
    });

    window.Shiny.addCustomMessageHandler("tarpuy:scroll-to", function (message) {
      scrollToElement(message || {});
    });

    window.Shiny.addCustomMessageHandler("tarpuy:focus", function (message) {
      var target = asElement(message && message.selector);
      safeFocus(target);
    });

    window.Shiny.addCustomMessageHandler("tarpuy:refresh-ui", function () {
      scheduleViewportUpdate();
      bindDynamicElements(document);
    });
  }

  /* ---------------------------------------------------------------------
   * Mutation observer
   * ------------------------------------------------------------------ */

  function observeDynamicUI() {
    if (typeof window.MutationObserver !== "function") return;

    var observer = new MutationObserver(function (mutations) {
      mutations.forEach(function (mutation) {
        mutation.addedNodes.forEach(function (node) {
          if (!node || node.nodeType !== 1) return;

          if (node.matches && node.matches(SELECTORS.googlePreview)) {
            bindGooglePreview(node);
          }

          bindDynamicElements(node);
        });
      });
    });

    observer.observe(document.body, {
      childList: true,
      subtree: true
    });

    TarpuyUI.observer = observer;
  }

  /* ---------------------------------------------------------------------
   * Event registration and public API
   * ------------------------------------------------------------------ */

  function registerEvents() {
    window.addEventListener("resize", scheduleViewportUpdate, { passive: true });
    window.addEventListener("orientationchange", scheduleViewportUpdate, { passive: true });

    document.addEventListener("wheel", containWheel, { passive: true, capture: true });
    document.addEventListener("scroll", handleLocalScroll, true);
    document.addEventListener("click", handleLoadingTrigger, true);

    document.addEventListener("visibilitychange", function () {
      if (document.hidden) {
        unlockPageScroll(null, true);
      }
    });

    window.addEventListener("blur", function () {
      unlockPageScroll(null, true);
    });

    if ($) {
      $(document).on("show.bs.tab", "[data-bs-toggle='tab'], [data-toggle='tab']", function (event) {
        var active = document.querySelector(
          ".nav-link.active[data-bs-toggle='tab'], .nav-link.active[data-toggle='tab'], " +
          ".navbar-nav .active > a[data-toggle='tab']"
        );
        saveCurrentTabScroll(active || event.currentTarget);
      });

      $(document).on("shown.bs.tab", "[data-bs-toggle='tab'], [data-toggle='tab']", function (event) {
        restoreTabScroll(event.currentTarget);
        scheduleViewportUpdate();
      });

      $(document).on("show.bs.modal", SELECTORS.modal, handleModalShow);
      $(document).on("shown.bs.modal", SELECTORS.modal, handleModalShown);
      $(document).on("hidden.bs.modal", SELECTORS.modal, handleModalHidden);

      $(document).on("shiny:busy", function () {
        setGlobalBusy(true);
      });

      $(document).on("shiny:idle", function () {
        state.busyCount = 1;
        setGlobalBusy(false);
      });

      $(document).on("shiny:connected", function () {
        registerShinyHandlers();
        bindDynamicElements(document);
        scheduleViewportUpdate();
      });
    }
  }

  function init() {
    if (state.initialized) return;

    state.initialized = true;
    TarpuyUI.initialized = true;

    updateViewportState();
    bindDynamicElements(document);
    registerEvents();
    registerShinyHandlers();
    observeDynamicUI();
  }

  TarpuyUI.init = init;
  TarpuyUI.updateViewport = scheduleViewportUpdate;
  TarpuyUI.refresh = function () {
    bindDynamicElements(document);
    scheduleViewportUpdate();
  };
  TarpuyUI.setLoading = setLoading;
  TarpuyUI.restoreScroll = restoreScroll;
  TarpuyUI.scrollTo = scrollToElement;
  TarpuyUI.unlockPreview = function () {
    unlockPageScroll(null, true);
  };

  window.TarpuyUI = TarpuyUI;

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init, { once: true });
  } else {
    init();
  }
})(window, document, window.jQuery);
