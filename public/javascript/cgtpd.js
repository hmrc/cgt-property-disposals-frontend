;(function(window, document) {
  if (window.Element && !Element.prototype.closest) {
    Element.prototype.closest =
      function(s) {
        var matches = (this.document || this.ownerDocument).querySelectorAll(s),
          i,
          el = this;
        do {
          i = matches.length;
          while (--i >= 0 && matches.item(i) !== el) {};
        } while ((i < 0) && (el = el.parentElement));
        return el;
      };
  }

  var exclude = '[data-ga-event="false"]';
  var GA_ENABLED = typeof window.ga === 'function';
  // wrapper function to ensure ga is
  // available in the environment
  function sendToGA() {
    if (GA_ENABLED) {
      window.ga.apply(null, arguments)
    }
  }

  function textContent(el) {
    return el.textContent.trim().replace(/\r?\n|\r/g, ' - ')
  }

  function radioLegendText(el) {
    var fieldset = el.closest('FIELDSET');
    var legend = fieldset.querySelector('legend');
    return legend ? textContent(legend) : textContent(document.querySelector('h1'))
  }

  function labelText(el) {
    var fieldId = el.getAttribute('data-focuses') || el.id;
    var label = document.querySelector('label[for="' + fieldId + '"]');
    if (label) {
      return textContent(label);
    }
    var legend = document.querySelector('fieldset#' + fieldId + ' legend');
    if (legend) {
      return textContent(legend);
    }

    return textContent(document.querySelector('h1'));
  }

  function sendErrorToGA(link) {
    // Google Analytics event reporting, using template:
    // ga('send', 'event', [eventCategory], [eventAction], [eventLabel], [eventValue], [fieldsObject])
    sendToGA('send', 'event', 'error', labelText(link), textContent(link))
  }

  var errorSummary = document.querySelector('.error-summary');
  if (errorSummary) {
    errorSummary.focus();
    var errorSummaryListItems = document.querySelectorAll('.error-summary-list li a');
    for (var l = 0; l < errorSummaryListItems.length; l++) {
      sendErrorToGA(errorSummaryListItems[l])
    }
  }

  function sendDetailsClickToGA(detailsEl) {
    return function(event) {
      var link = event.target;
      var pageHeading = document.querySelector('h1');
      sendToGA('send', 'event', detailsEl.hasAttribute('open') ? 'disclosure-close' : 'disclosure-open', textContent(pageHeading), textContent(link))
    }
  }

  var detailsElements = document.querySelectorAll('details');
  if (detailsElements.length) {
    for(var i = 0; i < detailsElements.length; i++) {
      var detailsEl = detailsElements[i];
      var summaryLink = detailsEl.querySelector('.summary');
      summaryLink.addEventListener('click', sendDetailsClickToGA(detailsEl))
    }
  }

  function submitAfterAllEvents (length, eventIndex, form) {
    return function () {
      if (length === eventIndex + 1) {
        form.submit()
      }
    }
  }

  function sendSelectionsToGA(form, radios, checkboxes) {
    return function (event) {
      var checkedRadios = Array.prototype.slice.call(radios).filter(function (r) { return r.checked });
      var checkedCheckboxes = Array.prototype.slice.call(checkboxes).filter(function (c) { return c.checked });
      var totalEvents = checkedRadios.length + checkedCheckboxes.length;
      // only intercept the submission if there are checked items to report on and we have GA
      if (totalEvents > 0 && GA_ENABLED) {
        event.preventDefault();
        var r = 0;
        if (checkedRadios.length) {
          for (r; r < checkedRadios.length; r++) {
            var radio = checkedRadios[r];
            sendToGA(
              'send',
              'event',
              'radio-selected',
              radioLegendText(radio),
              labelText(radio),
              {
                hitCallback: submitAfterAllEvents(totalEvents, r, form)
              })
          }
        }
        if (checkedCheckboxes.length) {
          for (var c = 0; c < checkedCheckboxes.length; c++) {
            var checkbox = checkedCheckboxes[c];
            sendToGA(
              'send',
              'event',
              'checkbox-selected',
              labelText(checkbox),
              'checked',
              {
                hitCallback: submitAfterAllEvents(totalEvents, c + r, form)
              })
          }
        }
      }
    }
  }

  var radioElements = document.querySelectorAll('input[type="radio"]');
  var checkBoxElements = document.querySelectorAll('input[type="checkbox"]');
  if (radioElements.length || checkBoxElements.length) {
    var form = document.querySelector('form');
    form.addEventListener('submit', sendSelectionsToGA(form, radioElements, checkBoxElements))
  }

  function sendLinkToGA(event) {
    console.log('link clicked ' + textContent(event.target))
    sendToGA('send', 'event', 'link-click', textContent(document.querySelector('h1')), textContent(event.target))
  }

  var linkElements = document.querySelectorAll('a:not(' + exclude + ')');
  if (linkElements.length) {
    for (var le = 0; le  < linkElements.length; le ++) {
      linkElements[le].addEventListener('click', sendLinkToGA)
    }
  }

  var countryEl = document.querySelector("#countryCode");
  if (countryEl) {
    openregisterLocationPicker({
      selectElement: countryEl,
      url: '/cgt-property-disposals/assets/location-autocomplete-graph.json',
      defaultValue: ''
    });
    // patch to ensure clearing the pseudo input also clears the select element
    // this is an active issue on accessible-autocomplete
    // https://github.com/alphagov/accessible-autocomplete/issues/260
    // when it is resolved we can remove this code
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

})(window, document);
