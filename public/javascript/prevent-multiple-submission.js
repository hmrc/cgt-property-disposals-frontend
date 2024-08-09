(function() {
     for (const form of document.getElementsByTagName("form")) {
       if (form.dataset.preventMultipleSubmit === "true") {
         form.addEventListener("submit", () => {
           for (const submit of form.querySelectorAll("button")) {
             submit.disabled = true;
           }
         });
       }
     }
 })();