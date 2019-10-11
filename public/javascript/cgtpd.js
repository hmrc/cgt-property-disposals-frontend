
var errorSummary = document.querySelector('.error-summary');
if (errorSummary) {
  errorSummary.focus()
}
var countryEl = document.querySelector("#countryCode");
if(countryEl) {
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
  function resetSelectIfEmpty (e) {
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
