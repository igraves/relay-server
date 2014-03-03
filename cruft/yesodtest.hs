{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod
import           Yesod.Core.Json
import           Data.ByteString.Lazy

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
  /location LocUpdate POST
|]

instance Yesod HelloWorld

--getHomeR :: Handler Html
--getHomeR = defaultLayout [whamlet|Hello World!|]

postLocUpdate :: Handler Value 
postLocUpdate = undefined

main :: IO ()
main = warp 3000 HelloWorld
