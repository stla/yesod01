{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}
module Upload where

import Foundation
import Yesod.Core
import Yesod.Form.Jquery ( YesodJquery (urlJqueryJs) )
import GHC.Generics ( Generic )
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import System.IO.Temp ( getCanonicalTemporaryDirectory, createTempDirectory )
import System.Process
import Text.Regex (mkRegex, subRegex)

replaceBackslahes :: String -> String
replaceBackslahes string = subRegex (mkRegex "\\\\") string "/"

base64ToFile :: String -> FilePath -> IO FilePath
base64ToFile b64string fileName = do
    let bstring = B64.decodeLenient (BC.pack b64string)
    tmpDir <- getCanonicalTemporaryDirectory
    dir <- createTempDirectory tmpDir "yesod"
    let filePath = dir ++ "/" ++ fileName
    B.writeFile filePath bstring 
    return $ replaceBackslahes dir

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
        <meta name="viewport" content="width=device-width, initial-scale=1">
    |]
    setTitle "Upload"
    addStylesheet $ StaticR bootstrap_5_3_2_css_bootstrap_min_css
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
    addScript $ StaticR bootstrap_5_3_2_js_bootstrap_bundle_min_js
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

rCommand :: FilePath -> String
rCommand outputDir = 
    "rmarkdown::render(\"static/R/report.Rmd\",output_dir=\"" ++ outputDir ++ "\")"

putFileR :: Handler String
putFileR = do
    file <- requireCheckJsonBody :: Handler File
    dir <- liftIO $ b64FileToFile file 
    liftIO $ print dir
    (exitcode, stdout, stderr) <- 
        liftIO $ readProcessWithExitCode "Rscript" ["-e", rCommand dir] ""
    liftIO $ print (exitcode, stdout, stderr)
    return dir
