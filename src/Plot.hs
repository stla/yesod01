{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}
module Plot where

import Foundation
import Yesod.Core
-- import Yesod.Form.Jquery ( YesodJquery (urlJqueryJs) )
import GHC.Generics                     ( Generic )
import System.Process                   ( readProcessWithExitCode )
import System.Exit                      ( ExitCode(ExitSuccess) )
import Text.Regex                       ( mkRegex, subRegex )
import Control.Monad                    ( when )
import GHC.Float (plusDouble)

data XY = XY {
    _x :: [String],
    _y :: [Double],
    _width :: Double,
    _height :: Double
} deriving (Show, Generic)

instance FromJSON XY

quote :: String -> String
quote x = "\\\"" ++ x ++ "\\\""

triBackslahes :: String -> String
triBackslahes string = subRegex (mkRegex "\"") string "\\\""

toJsonXY :: XY -> String
toJsonXY xy = "{" ++ quote "x" ++ ":" ++ triBackslahes (show (_x xy)) ++ "," ++
                quote "y" ++ ":" ++ show (_y xy) ++ "}"

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
            <img src=./static/images/haskell.png style=float:right;margin:5px;width:50px;>
            <div .container-fluid>
                $# TABS -------------------------------------------------------
                <ul .nav .nav-tabs role=tablist>
                    <li .nav-item role=presentation>
                        <button #data-tab .nav-link .active data-bs-toggle=tab data-bs-target=#data-tab-pane type=button role=tab aria-controls=data-tab-pane aria-selected=true>Data
                    <li .nav-item role=presentation>
                        <button #plot-tab .nav-link data-bs-toggle=tab data-bs-target=#plot-tab-pane type=button role=tab aria-controls=plot-tab-pane aria-selected=false>Plot
                $# TABS CONTENTS ----------------------------------------------
                <div #tabContent .tab-content>
                    $# DATA TAB -----------------------------------------------
                    <div #data-tab-pane .tab-pane .fade .show .active role=tabpanel aria-labelledby=data-tab tabindex=0>
                        <div .row>
                            $# SIDEBAR ----------------------------------------
                            <div .col-4>
                                <div #sidebarData .sidebar .card .text-bg-dark tabindex=-1 aria-labelledby=sidebarDataTitle>
                                    <div .sidebar-header .card-body>
                                        <h5 #sidebarDataTitle .card-title>Upload data
                                    <div .sidebar-body .card-body>
                                        <h6 .card-text>Upload a CSV file or a XLSX file.
                                        <p .card-text style=font-style:italic;>If you upload a XLSX file, the data from the first sheet will be extracted.
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
                                <div #sidebarPlot .sidebar .card .text-bg-dark tabindex=-1 aria-labelledby=sidebarPlotTitle>
                                    <div .card-body>
                                        <div .sidebar-header>
                                            <h5 #sidebarPlotTitle .card-title>Plot
                                        <div .sidebar-body>
                                            <fieldset #selectXY style=display:none;>
                                                <label for=selX>Select the <em>x</em> column
                                                <select .form-control #selX style=overflow-y:auto;>
                                                <br>
                                                <label for=selY>Select the <em>y</em> column
                                                <select .form-control #selY style=overflow-y:auto;>
                                <div #spinner .spinner-border .m-5 role=status style=display:none>
                                    <span .visually-hidden>Loading...
                            $# PLOT -------------------------------------------
                            <div .col-8>
                                <img #plot width=100% height=400px>
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
            let colNames = results.meta.fields;
            // Fill table -----------------------------------------------------
            let headers = "";
            for(let colname of colNames) {
                headers += "<th>" + colname + "</th>";
            }
            $("#table thead tr").append(headers);
            let columns = [];
            for(let colname of colNames) {
                columns.push({data: colname});
            }
            $("#table").DataTable({
                data: results.data,
                columns: columns
            });
            // make the "dataframe" in columns format
            let df = results.data;
            let dfcolumns = {};
            let dfx = {};
            for(let colname of colNames) {
                let column = [];
                let columnx = [];
                for(let j = 0; j < df.length; j++) {
                    column.push(df[j][colname]);
                    columnx.push(df[j][colname].toString());
                }
                dfcolumns[colname] = column;
                dfx[colname] = columnx;
            }            
            // Fill x & y dropdowns --------------------------------------------
            let $selsXY = $("#selX, #selY");
            let ncolumns = colNames.length;
            let size = ncolumns < 5 ? ncolumns : 5;
            $selsXY.attr("size", size);
            $(colNames).each(function(idx, item) {
                if(item != "") {
                    $selsXY.append($("<option>").attr("value", idx).text(item));
                }
            });
            let selX = document.querySelector("#selX");
            let selY = document.querySelector("#selY");
            selX.value = "0";
            selY.value = "1";
            $("#selectXY").show();
            // AJAX : send {x:[...],y:[...]} to R and get base64 of the plot
            let myModalEl = document.getElementById("myModal");
            let myModal   = new bootstrap.Modal(myModalEl);
            let resultEl  = myModalEl.querySelector("#result");
            let titleEl   = myModalEl.querySelector(".modal-title");
            let $selX = $("#selX");
            let $selY = $("#selY");
            plot(
                $selX, $selY, dfcolumns, dfx, colNames, 
                titleEl, resultEl, myModal
            );
            // on change x or y, do plot
            $selsXY.on("change", function() {
                plot(
                    $selX, $selY, dfcolumns, dfx, colNames, 
                    titleEl, resultEl, myModal
                );
            });
            // on resize, do plot
            $(window).on("resize", function() {
                plot(
                    $selX, $selY, dfcolumns, dfx, colNames, 
                    titleEl, resultEl, myModal
                );
            });
        }
    });
}

function plot(
    $selX, $selY, dfcolumns, dfx, colNames, titleEl, resultEl, myModal
) {
    $("#spinner").show();
    let xidx = $selX.val()
    let yidx = $selY.val();
    let x = dfx[colNames[xidx]];
    let y = dfcolumns[colNames[yidx]];
    let width = $("#plot").width();
    if(width === 0) {
        width = 400;
    }
    let height = $("#plot").height();
    if(height === 0) {
        height = 400;
    }    
    let XYw = JSON.stringify({_x: x, _y: y, _width: width, _height = height});
    $.ajax({
        contentType: "application/json; charset=UTF-8",
        processData: false,
        url: "@{PlotR}",
        type: "PUT",
        data: XYw,
        success: function(string) {
            $("#spinner").hide();
            let error_base64 = string.split("*::*::*::*::*");
            let error = error_base64[0];
            if(error === "") {
                let base64 = error_base64[1];
                $('#plot').attr("src", base64);
            } else {
                titleEl.textContent = "An error occured";
                resultEl.textContent = error;
                myModal.show();
            }
        },
        dataType: "text"
    });
}

$(function(){
    $("#file").on("change", function(e) {
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

quote' :: String -> String
quote' x = "\"" ++ x ++ "\""

rCommand :: String -> String -> String -> String
rCommand width height jsonString = 
    "w<-" ++ width ++ ";h<-" ++ height ++ 
        ";XY<-" ++ quote' jsonString ++ 
        ";source(\"static/R/ggplotXY.R\")"

putPlotR :: Handler String
putPlotR = do
    xywh <- requireCheckJsonBody :: Handler XY
    let jsonString = toJsonXY xywh
    let w = show (_width xywh)
    let h = show (_height xywh)
    (exitcode, stdout, stderr) <- liftIO $ 
        readProcessWithExitCode "Rscript" ["-e", rCommand w h jsonString] ""
    liftIO $ print (exitcode, stdout, stderr)
    let base64 = stdout
    let err = if exitcode == ExitSuccess then "" else stderr
    let string = err ++ "*::*::*::*::*" ++ base64
    return string
