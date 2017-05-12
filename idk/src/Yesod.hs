{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Foundation where

import Yesod as Y

data Idk = Idk

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod Idk

getHomeR :: Handler Html
getHomeR = defaultLayout $(whamletFile "web-src/hamlet/index.hamlet")

main :: IO ()
main = warp 3000 HelloWorld
