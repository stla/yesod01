{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}
module Multiply where

import Foundation
import Yesod.Core
import Yesod.Form.Jquery (YesodJquery (urlJqueryJs))
import GHC.Generics ( Generic )

data Operation = Operation {
    _x :: Int,
    _y :: Int
} deriving (Show, Generic)
 
instance FromJSON Operation

getMultiplyR :: Handler Html
getMultiplyR = defaultLayout $ do
    setTitle "Multiplication"
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
                        <div .modal-footer>
                            <button type=button .btn .btn-secondary data-bs-dismiss=modal>Close
            <div .container-fluid>
                <div .row>
                    <h3>Enter <i>x</i> and <i>y</i>
                <div .row>
                    <div .col>
                        <div .form-floating>
                            <input #x type=number .form-control value=0>
                            <label for=x>Enter <i>x</i>
                    <div .col>
                        <div .form-floating>
                            <input #y type=number .form-control value=0>
                            <label for=y>Enter <i>y</i>
                <br>
                <div>
                    <button #submit type=button .btn .btn-primary>Calculate <i>x&times;y</i>
    |]
    addScriptRemote "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js"
    toWidget script

script :: JavascriptUrl (Route App)
script = [julius|
$(function(){
    const myModalEl = document.getElementById("myModal");
    const myModal = new bootstrap.Modal(myModalEl);
    const modalBody = myModalEl.querySelector(".modal-body");
    $("#submit").click(function(){
        $.ajax({
            contentType: "application/json",
            processData: false,
            url: "@{OperationR}",
            type: "PUT",
            data: JSON.stringify({
                _x: Number($("#x").val()), 
                _y: Number($("#y").val())
            }),
            success: function(result) {
                modalBody.textContent = result;
                myModal.show();
            },
            dataType: "text"
        });
    });
});
|]

putOperationR :: Handler String
putOperationR = do
    operation <- requireCheckJsonBody :: Handler Operation
    return $ show $ (_x operation) * (_y operation)
