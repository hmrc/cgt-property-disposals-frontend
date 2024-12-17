(async function(document, window) {
    function getFileExtension(filePath){
        var fileSplit = filePath.split('.');
        var fileExt = '';
        if (fileSplit.length > 1) {
        fileExt = fileSplit[fileSplit.length - 1];
        }
        return fileExt;
    }
    function renderFormError(uploadInput) {
        const existingErrorSummary = document.querySelector('.govuk-error-summary')
        const submitButton = document.getElementById('submitButton')
        if(existingErrorSummary) {
            existingErrorSummary.focus()
            submitButton.removeAttribute('disabled')
            return;
        }
        const errorPrefix = 'Error: '
        if(document.title.substring(0, 7) !== errorPrefix) {
            document.title = errorPrefix + document.title
        }
        const summaryTpl = document.getElementById('empty-file-error-summary')
        const errorTpl = document.getElementById('empty-file-error-message')
        const mainContent = document.querySelector('#main-content > div > div')
        const formGroup = mainContent.querySelector('.govuk-form-group')
        mainContent.prepend(summaryTpl.content)
        const errorSummary = document.querySelector('.govuk-error-summary')
        formGroup.classList.add('govuk-form-group--error')
        formGroup.insertBefore(errorTpl.content, uploadInput)
        uploadInput.setAttribute('aria-describedby', 'file-input-hint file-upload-error')
        uploadInput.classList.add('govuk-file-upload--error')
        errorSummary.focus()
        submitButton.removeAttribute('disabled')
    }

    const fileUploadForm = document.getElementById('uploadForm')
    if(fileUploadForm) {
        const uploadInput = document.getElementById('file')
        uploadInput.removeAttribute('required')
        fileUploadForm.setAttribute('novalidate', 'novalidate')
        fileUploadForm.addEventListener('submit', function(event) {
            document.getElementById('submitButton').setAttribute('disabled', 'true')
            const fileName = uploadInput.value
            const files = uploadInput.files
            if(files.length === 0) {
                event.preventDefault()
                const error = 'Select a file';
                renderFormError(uploadInput)
                //render the error.pattern1
            }
            else{
                var ext = getFileExtension(filePath);
                if (ext !='') {
                    event.preventDefault();
                    const error = 'Must include file type';
                    //render the error.pattern2
                }
                else if(!isLowerCase(ext)){
                    event.preventDefault();
                    const error = 'File type should be in lower case';
                    //render the error.pattern3
                }
            }
        })
    }
})(document, window)