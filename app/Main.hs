import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core
import Yesod.Static

main :: IO ()
main = do 
    static@(Static settings) <- static "static"
    warp 3000 $ App static
