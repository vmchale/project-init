{-# LANGUAGE RecordWildCards #-}

module Lib
    ( exec
    ) where


import           Miso

type Model = Int

data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

exec :: IO ()
exec = startApp App {..}
  where
    initialAction = SayHelloWorld
    model  = 0
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = []

updateModel :: Action -> Model -> Effect Model Action
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp m = noEff m
updateModel SayHelloWorld m = m <# do
    putStrLn "Hello World" >> pure NoOp

viewModel :: Model -> View Action
viewModel x = div_ []
    [
      button_ [ onClick AddOne ] [ text "+" ]
    , text (show x)
    , button_ [ onClick SubtractOne ] [ text "-" ]
    ]
