{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Yesod.Core ( whamlet, setTitle, Yesod(defaultLayout), Html )

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Minimal Multifile"
    [whamlet|
        <p>
            <a href=@{AddR 5 7}>HTML addition
        <p>
            <a href=@{AddR 5 7}?_accept=application/json>JSON addition
        <p>
            <a href=@{MultiplyR}>AJAX multiplication
        <p>
            <a href=@{UploadR}>Upload a file
        <p>
            <a href=@{PlotR}>Upload a file and plot
    |]
