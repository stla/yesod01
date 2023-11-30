{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}
module Plot where

import Foundation
import Yesod.Core
-- import Yesod.Form.Jquery ( YesodJquery (urlJqueryJs) )
import GHC.Generics                     ( Generic )
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as BC
import qualified Data.Text as T
import Network.Mime                     ( defaultMimeLookup )
import System.IO.Temp                   ( getCanonicalTemporaryDirectory, createTempDirectory )
import System.Process                   ( readProcessWithExitCode )
import System.Exit                      ( ExitCode(ExitSuccess) )
import Text.Regex                       ( mkRegex, subRegex )
import Control.Monad                    ( when )

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

fileToBase64 :: FilePath -> IO String
fileToBase64 filename = do
  file <- B.readFile filename
  return $ "data:" ++ BC.unpack (defaultMimeLookup $ T.pack filename)
             ++ ";base64," ++ BC.unpack (B64.encode file) 

getPlotR :: Handler Html
getPlotR = defaultLayout $ do
    toWidgetHead [hamlet|
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
    |]
    setTitle "Upload"
    addStylesheet $ StaticR bootstrap_5_3_2_css_bootstrap_min_css
    addStylesheet $ StaticR _DataTables_1_13_8_datatables_min_css
    -- getYesod >>= addScriptEither . urlJqueryJs
    [whamlet|
        <body>
            <div #myModal .modal .fade aria-hidden aria-labelledby=myModalLabel tabindex=-1>
                <div .modal-dialog .modal-dialog-centered">
                    <div .modal-content>
                        <div .modal-header>
                            <h1 .modal-title .fs-5>
                        <div .modal-body>
                            <span #result>
                        <div .modal-footer>
                            <button type=button .btn .btn-secondary data-bs-dismiss=modal>Close
            <img src=./static/images/haskell.png style=float:right;margin:10px;width:100px;>
            <div .container-fluid>
                $# TABS -------------------------------------------------------
                <ul .nav .nav-tabs>
                    <li .nav-item role=presentation>
                        <button #data-tab .nav-link .active data-bs-toggle=tab data-bs-target=#data-tab-pane type=button role=tab aria-controls=data-tab-pane aria-selected=true>Data
                    <li .nav-item role=presentation>
                        <button #plot-tab .nav-link .active data-bs-toggle=tab data-bs-target=#plot-tab-pane type=button role=tab aria-controls=plot-tab-pane aria-selected=true>Plot
                $# TABS CONTENTS ----------------------------------------------
                <div #tabContent .tab-content>
                    $# DATA TAB -----------------------------------------------
                    <div #data-tab-pane .tab-pane .fade .show .active role=tabpanel aria-labelledby=data-tab tabindex=0>
                        <div .row>
                            $# SIDEBAR ----------------------------------------
                            <div .col-4>
                                <div #sidebarData .offcanvas .offcanvas-start .show .text-bg-dark tabindex=-1 aria-labelledby=sidebarDataTitle>
                                    <div .offcanvas-header.>
                                        <h5 #sidebarDataTitle .offcanvas-title>Upload data
                                    <div .offcanvas-body>
                                        <h6>Upload a CSV file or a XLSX file.
                                        <h6 .text-body-secondary>If you upload a XLSX file, the data from the first sheet will be extracted.
                                        <br>
                                        <input #file type=file .form-control .btn .btn-info>
                            $# TABLE ------------------------------------------
                            <div .col-8>
                                <table #table .table-striped .table-bordered .table-hover>
                                    <thead>
                                        <tr role=row>
                                    <tbody>
                    $# PLOT TAB -----------------------------------------------
                    <div #plot-tab-pane .tab-pane .fade role=tabpanel aria-labelledby=plot-tab tabindex=0>
                        <div .row>
                            $# SIDEBAR ----------------------------------------
                            <div .col-4>
                                <div #sidebarPlot .offcanvas .offcanvas-start .show .text-bg-dark tabindex=-1 aria-labelledby=sidebarPlotTitle>
                                    <div .offcanvas-header.>
                                        <h5 #sidebarPlotTitle .offcanvas-title>Plot
                                    <div .offcanvas-body>
                                        <p>x and y selections
                            $# PLOT -------------------------------------------
                            <div .col-8>
                                <div #spinner .spinner-border .m-5 role=status style=display:none>
                                    <span .visually-hidden>Loading...
                                <img #plot width=400 height=auto>
    |]
    addScript $ StaticR jQuery_jquery_3_7_1_min_js
    addScript $ StaticR bootstrap_5_3_2_js_bootstrap_bundle_min_js
    addScript $ StaticR _DataTables_1_13_8_datatables_min_js
    addScript $ StaticR _PapaParse_papaparse_min_js
    addScript $ StaticR _SheetJS_xlsx_core_min_js
    toWidget script

script :: JavascriptUrl (Route App)
script = [julius|
function papaParse(csv) {
    Papa.parse(csv, {
        header: true,
        skipEmptyLines: true,
        dynamicTyping: true,
        complete: function(results) {
            if(results.errors.length != 0) {
                alert("Something is wrong with this CSV file.");
                console.log("Errors:", results.errors);
                throw new Error("Something is wrong with this CSV file.");
            }
            // Fill table -----------------------------------------------------
            let headers = "";
            for(let colname of results.meta.fields) {
                headers += "<th>" + colname + "</th>";
            }
            $("#table thead tr").append(headers);
            let columns = [];
            for(let colname of results.meta.fields) {
                columns.push({data: colname});
            }
            $("#table").DataTable({
                data: results.data,
                columns: columns
            });
            // Fill x & y dropdowns --------------------------------------------
            // ...
            // AJAX : send {x:[...],y:[...]} to R and get base64 of the plot
            // ...
            // on change x or y, do AJAX
            // ...
        }
    });
}
$(function(){
    const myModalEl = document.getElementById("myModal");
    const myModal   = new bootstrap.Modal(myModalEl);
    const resultEl  = myModalEl.querySelector("#result");
    const titleEl   = myModalEl.querySelector(".modal-title");
    $("#file").on("change", function(e) {
        $("#spinner").show();
        let file = e.target.files[0];
        let extension = file.name.split('.').pop().toLowerCase();
        // --------------------------------------------------------------------
        if(extension === "xlsx") {
            let reader = new FileReader();
			reader.onload = function(e) {
				let workbook;
				try {
					workbook = XLSX.read(e.target.result, {
						type: "binary"
					});
				} catch(err) {
					alert("Something is wrong with this XLSX file.");
					throw new Error(err);
				}
				let sheetNames = workbook.SheetNames;
                let sheet1 = sheetNames[0];
                let XLSXasCSV = 
                    XLSX.utils.sheet_to_csv(workbook.Sheets[sheet1]);
                papaParse(XLSXasCSV);
            }
            reader.onerror = function(err) {
				alert("I can't read this XLSX file!");
				throw new Error(err);
			};
			reader.readAsArrayBuffer(file);
        } else if(extension === "csv" || extension === "tsv") {
            papaParse(file);
        }
    });
});
|]

quote :: String -> String
quote x = "\"" ++ x ++ "\""

rCommand :: FilePath -> String -> String
rCommand outputDir fileName = 
    "rmarkdown::render(\"static/R/report.Rmd\",output_dir=" ++ 
        quote outputDir ++ ",params=list(upload=" ++ quote fileName ++ 
        ",tmpDir=" ++ quote outputDir ++ "))"

putPlotR :: Handler String
putPlotR = do
    file <- requireCheckJsonBody :: Handler File
    let fileName = _filename file
    dir <- liftIO $ b64FileToFile file 
    (exitcode, stdout, stderr) <- liftIO $ 
        readProcessWithExitCode "Rscript" ["-e", rCommand dir fileName] ""
    liftIO $ print (exitcode, stdout, stderr)
    when (exitcode /= ExitSuccess) $
        liftIO $ writeFile (dir ++ "/report.html") "" 
    base64 <- liftIO $ fileToBase64 (dir ++ "/report.html")
    let err = if exitcode == ExitSuccess then "" else stderr
    let string = err ++ "*::*::*::*::*" ++ base64
    return string
