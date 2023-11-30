{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Yesod.Core ( mkYesodDispatch )
import Add        ( getAddR )
import Home       ( getHomeR )
import Multiply   ( putOperationR, getMultiplyR )
import Upload
import Plot

mkYesodDispatch "App" resourcesApp
