;(function(window, document, GOVUK) {
  GOVUK.details.init();

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

  function sendToGA() {
    if (GA_ENABLED) {
      window.ga.apply(null, arguments)
    }
  }

  if (window.jsConfig.gaCustomDimensions.length) {
    for(var i = 0; i < window.jsConfig.gaCustomDimensions.length; i++) {
      var dimension = window.jsConfig.gaCustomDimensions[i];
      sendToGA('set', dimension.name, dimension.value)
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

  function handleFileUploadError (input, msg) {
    $('.file-upload-error').remove();
    var errorEl = '<span class="error-message file-upload-error" role="alert">' + msg + '</span>';
    $(errorEl).insertBefore($(input))
    window.setTimeout(function () {
      document.querySelector('button[type="submit"]').removeAttribute('disabled')
    }, 400);
  }

  function validateFile(file, form, singleFileUpload, submitForm) {
    if (!file) {
      return handleFileUploadError(singleFileUpload, "Upload a file containing your supporting evidence")
    }

    var contentTypes = "application/vnd.ms-excel,application/vnd.openxmlformats-officedocument.spreadsheetml.sheet,application/vnd.oasis.opendocument.spreadsheet,application/pdf,application/msword,application/vnd.openxmlformats-officedocument.wordprocessingml.document,application/vnd.oasis.opendocument.text,text/plain,image/png,image/jpeg";

    if (file.type === "" || contentTypes.indexOf(file.type) === -1) {
      return handleFileUploadError(singleFileUpload, "The file type " + file.type.toString() + " is not supported.");
    }

    if (file.size > (3145728)) {
      return handleFileUploadError(singleFileUpload, "The file must be less than 3MB in size");
    }
    if (submitForm) {
      form.submit();
    }
  }

  function validateFileUpload(form, singleFileUpload) {
    return function (e)  {
      e.preventDefault();
      validateFile(e.target.files[0], form, singleFileUpload, false)
    }
  }

  function validateFormSubmission(form, singleFileUpload) {
    return function(e)  {
      e.preventDefault();
      var file = singleFileUpload.files[0];
      validateFile(file, form, singleFileUpload, true);
    }
  }

  var singleFileUpload = document.querySelector('input[type="file"]');

  if (singleFileUpload) {
    var form = document.querySelector('form');
    singleFileUpload.addEventListener('change', validateFileUpload(form, singleFileUpload));
    form.addEventListener('submit', validateFormSubmission(form, singleFileUpload))
  }

  function sendLinkToGA(event) {
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

  if (window.jsConfig && window.jsConfig.timeoutEnabled) {
    GOVUK.sessionTimeout({
      timeout: window.jsConfig.timeout,
      countdown: window.jsConfig.countdown,
      keep_alive_url: window.jsConfig.keep_alive_url,
      logout_url: window.jsConfig.logout_url,
      timed_out_url: window.jsConfig.timed_out_url
    })
  }

})(window, document, GOVUK);
