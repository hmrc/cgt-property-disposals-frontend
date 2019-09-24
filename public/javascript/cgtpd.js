$('.error-summary').focus();
var countryEl = document.querySelector("#countryCode");
if(countryEl) {
  openregisterLocationPicker({
    selectElement: countryEl,
    url: '/cgt-property-disposals/assets/location-autocomplete-graph.json'
  });
}
