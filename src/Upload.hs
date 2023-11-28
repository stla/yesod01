{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}
module Upload where

import Foundation
import Yesod.Core
import Yesod.Form.Jquery (YesodJquery (urlJqueryJs))
import GHC.Generics ( Generic )

data File = File {
    _filename :: String,
    _base64   :: String
} deriving (Show, Generic)
 
instance FromJSON File

getUploadR :: Handler Html
getUploadR = defaultLayout $ do
    setTitle "Upload"
    addStylesheetRemote "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css"
    getYesod >>= addScriptEither . urlJqueryJs
    [whamlet|
        <body>
            <div #myModal .modal .fade aria-hidden aria-labelledby=myModalLabel tabindex=-1>
                <div .modal-dialog .modal-dialog-centered">
                    <div .modal-content>
                        <div .modal-header>
                            <h1 .modal-title .fs-5>Multiplication
                        <div .modal-body>
                            <i>x</i>&times;<i>y</i> = <span id="result"></span>
                        <div .modal-footer>
                            <button type=button .btn .btn-secondary data-bs-dismiss=modal>Close
            <div .container-fluid>
                <div .row>
                    <input #file type=file .form-control>
    |]
    addScriptRemote "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js"
    toWidget script

script :: JavascriptUrl (Route App)
script = [julius|
$(function(){
    const myModalEl = document.getElementById("myModal");
    const myModal = new bootstrap.Modal(myModalEl);
    const resultEl = myModalEl.querySelector("#result");
    $("#file").on("change", function() {
        let file = this.files[0];
        let fileReader = new FileReader(); 
        fileReader.readAsDataURL(file);
        fileReader.onload = function() {
            let base64 = fileReader.result;
            $.ajax({
                contentType: "application/json",
                processData: false,
                url: "@{FileR}",
                type: "PUT",
                data: JSON.stringify({
                    _filename: file.name, 
                    _base64: base64
                }),
                success: function(result) {
                    resultEl.textContent = result;
                    myModal.show();
                },
                dataType: "text"
            });
        }; 
        fileReader.onerror = function() {
            alert(fileReader.error);
        }; 
    });
});
|]

putFileR :: Handler String
putFileR = do
    file <- requireCheckJsonBody :: Handler File
    return $ show $ _filename file
