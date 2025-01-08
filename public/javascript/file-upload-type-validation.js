(async function(document, window) {
    const fileUploadForm = document.getElementById('uploadForm')

    function getFileExtension(filePath){
        var fileSplit = filePath.split('.');
        var fileExt = '';
        if (fileSplit.length > 1) {
        fileExt = fileSplit[fileSplit.length - 1];
        }
        return fileExt;
    }

    function isLowerCase(str) {
      const regex = /^[a-z]+$/; // Matches only lowercase letters (a-z)
      return regex.test(str);
    }

    function renderFormError(uploadInput,errorType) {
        const existingErrorSummary = document.querySelector('.govuk-error-summary')
        const existingErrorMessage = document.querySelector('.govuk-error-message')
        if(existingErrorSummary) {
            existingErrorSummary.remove()
        }
        if(existingErrorMessage) {
            existingErrorMessage.remove()
        }
        const submitButton = document.getElementById('submitButton')
        const summaryTpl = document.getElementById(errorType+'-error-summary')
        const summaryContent = document.importNode(summaryTpl.content, true);
        const errorTpl = document.getElementById(errorType+'-error-message')
        const errorContent = document.importNode(errorTpl.content, true);
        const mainContent = document.querySelector('#main-content > div > div')
        const formGroup = mainContent.querySelector('.govuk-form-group')
        mainContent.prepend(summaryContent)
        const errorSummary = document.querySelector('.govuk-error-summary')
        formGroup.classList.add('govuk-form-group--error')
        formGroup.insertBefore(errorContent, uploadInput)
        uploadInput.setAttribute('aria-describedby', 'file-input-hint file-upload-error')
        uploadInput.classList.add('govuk-file-upload--error')
        errorSummary && errorSummary.focus()
        submitButton.removeAttribute('disabled')

    }

    if(fileUploadForm) {
        const uploadInput = document.getElementById('file')
        uploadInput.removeAttribute('required')
        fileUploadForm.setAttribute('novalidate', 'novalidate')
        fileUploadForm.addEventListener('submit', function(event) {
            document.getElementById('submitButton').setAttribute('disabled', 'true')
            const fileName = uploadInput.value
            const files = uploadInput.files
            var ext = getFileExtension(fileName)
            var allowedTypes = ['text/csv', 'text/plain'];
            var allowedExtensions = ['.txt','.csv']
            if(files.length === 0) {
                event.preventDefault()
                renderFormError(uploadInput,"empty-file")
            }
            else if(files[0] && (!allowedTypes.includes(files[0].type))) {
                if (ext =='') {
                    event.preventDefault();
                    renderFormError(uploadInput,"no-extension")
                }
                else{
                    event.preventDefault()
                    renderFormError(uploadInput,"wrong-extension")
                }
            }
            else if(files[0] && (files[0].size / (1024 * 1024)) > 3) {
                event.preventDefault()
                renderFormError(uploadInput,"large-file")
            }
            else if (ext =='') {
                    event.preventDefault();
                    renderFormError(uploadInput,"no-extension")
            }
            else if(!isLowerCase(ext)){
                    event.preventDefault();
                    renderFormError(uploadInput,"uppercase-extension")
            }
        })
    }
})(document, window)