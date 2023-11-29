{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Foundation where

import Yesod.Core
    --( Yesod, mkYesodData, parseRoutesFile, RenderRoute(renderRoute) )
import Yesod.Form.Jquery ( YesodJquery )
import Yesod.Static
staticFiles "static"

data App = App
    { getStatic :: Static
    }

mkYesodData "App" $(parseRoutesFile "routes.yesodroutes")

instance Yesod App
instance YesodJquery App
