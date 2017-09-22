module Blah exposing (..)

import Html exposing (..)
import CreditCard as CC


main =
    beginnerProgram
        { model = model
        , view = view
        , update = \_ _ -> model
        }


model : CC.CreditCard
model =
    CC.initCreditCard


view model =
    div [] [ text (toString model.name.value) ]
