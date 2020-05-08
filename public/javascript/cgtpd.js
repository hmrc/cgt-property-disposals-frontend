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

  var errorSummary = document.querySelector('.error-summary');
  if (errorSummary) {
    errorSummary.focus();
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
      var errorMessage = "";
      if (singleFileUpload.getAttribute("data-is-mandatory-upload") === "true")
        errorMessage = "Upload a document showing how the Capital Gains Tax due was worked out"
      else
        errorMessage = "Upload a file containing your supporting evidence"

      return handleFileUploadError(singleFileUpload, errorMessage);
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
