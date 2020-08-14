;(function(window, document, GOVUK, $) {
  GOVUK.details.init();

  var errorSummary = document.querySelector('.error-summary');
  if (errorSummary) {
    errorSummary.focus();
  }

  var stepByStepNavigation = new GOVUK.Modules.StepByStepNavigation()
  const stepBySteps = document.querySelectorAll('.app-step-nav')
  if (stepBySteps.length) {
    stepBySteps.forEach(function (s) {
      stepByStepNavigation.start($(s))
    })
  }

  var countryEl = document.querySelector("#countryCode");
  var lang = GOVUK.getCookie("PLAY_LANG")
  if (countryEl && (!lang || lang === "en")) {
    openregisterLocationPicker({
      selectElement: countryEl,
      name: 'countryCode-name',
      url: '/capital-gains-tax-uk-property/assets/location-autocomplete-graph.json',
      defaultValue: ''
    });

    var wrapper = document.querySelector('.country-code-wrapper');

    function resetSelectIfEmpty(e) {
      if (e.target.id === 'countryCode') {
        var val = e.target.value.trim();
        var countrySelect = document.querySelector("#countryCode-select");
        if (countrySelect) {
          var countriesArray = Array.prototype.slice.call(countrySelect.options);
          var matches = countriesArray.filter(function (o) {
            return o.text !== '' && o.text === val
          });
          if (!matches.length) {
            countrySelect.value = ''
          }
        }
      }
    }

    wrapper.addEventListener('change', resetSelectIfEmpty);
  }

  var urBanner = document.querySelector('.ur-banner')
  if (urBanner) {
    var urBannerCookieName = 'mtdpurr'
    var hasDismissed = GOVUK.getCookie(urBannerCookieName)

    function removeUrBanner () {
      urBanner.parentNode.removeChild(urBanner)
    }

    function dismissUrBanner (e) {
      if (e.target && e.target.id === 'dismiss-ur-banner') {
        e.preventDefault()
        GOVUK.setCookie(urBannerCookieName, 'suppress_for_all_services', { days: 30 })
        removeUrBanner()
      }
    }

    function showUrBanner () {
      urBanner.addEventListener('click', dismissUrBanner)
      urBanner.classList.remove('js-hidden')
    }

    if (hasDismissed) {
      removeUrBanner()
    } else {
      showUrBanner()
    }
  }

  if (window.jsConfig && window.jsConfig.timeoutEnabled) {
    GOVUK.sessionTimeout({
      timeout: window.jsConfig.timeout,
      countdown: window.jsConfig.countdown,
      keep_alive_url: window.jsConfig.keep_alive_url,
      logout_url: window.jsConfig.logout_url,
      timed_out_url: window.jsConfig.timed_out_url
    })
  }

})(window, document, GOVUK, window.jQuery);
