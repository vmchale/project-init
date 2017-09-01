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
  deriving (Show, Eq)

exec :: IO ()
exec = startApp App {..}
  where
    initialAction = NoOp
    model  = 0
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = []

updateModel :: Action -> Model -> Effect Model Action
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp m = noEff m

viewModel :: Model -> View Action
viewModel x = div_ []
    [
      button_ [ onClick AddOne ] [ text "+" ]
    , text (show x)
    , button_ [ onClick SubtractOne ] [ text "-" ]
    ]
