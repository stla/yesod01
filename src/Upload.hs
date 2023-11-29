{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}
module Upload where

import Foundation
import Yesod.Core
import Yesod.Form.Jquery (YesodJquery (urlJqueryJs))
import GHC.Generics ( Generic )
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import System.IO.Temp ( getCanonicalTemporaryDirectory )

base64ToFile :: String -> FilePath -> IO FilePath
base64ToFile b64string fileName = do
    let bstring = B64.decodeLenient (BC.pack b64string)
    tmpDir <- getCanonicalTemporaryDirectory
    let filePath = tmpDir ++ "/" ++ fileName
    B.writeFile filePath bstring 
    return filePath

data File = File {
    _filename :: String,
    _base64   :: String
} deriving (Show, Generic)

instance FromJSON File

b64FileToFile :: File -> IO FilePath
b64FileToFile file = base64ToFile (_base64 file) (_filename file)

getUploadR :: Handler Html
getUploadR = defaultLayout $ do
    toWidgetHead [hamlet|
        <meta charset="UTF-8">
    |]
    setTitle "Upload"
    addStylesheetRemote "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css"
    getYesod >>= addScriptEither . urlJqueryJs
    [whamlet|
        <body>
            <div #myModal .modal .fade aria-hidden aria-labelledby=myModalLabel tabindex=-1>
                <div .modal-dialog .modal-dialog-centered">
                    <div .modal-content>
                        <div .modal-header>
                            <h1 .modal-title .fs-5>Upload successful
                        <div .modal-body>
                            <i>File saved to: </i> <span id="result"></span>
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
            let base64 = fileReader.result.split(",")[1];
            console.log(base64);
            $.ajax({
                contentType: "application/json; charset=UTF-8",
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
    liftIO $ b64FileToFile file 
