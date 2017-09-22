module Blah exposing (..)

import BodyBuilder exposing (..)
import CreditCard as CC


main : Program Never Model Model
main =
    BodyBuilder.program
        { init = init
        , update = \msg model -> model ! []
        , subscriptions = always Sub.none
        , view = view
        }


type alias Model =
    CC.CreditCard


init : ( Model, Cmd msg )
init =
    CC.initCreditCardDefault ! []


view : Model -> Node interactiveContent NotPhrasing Spanning NotListElement msg
view { holderName, number, expiration, cvc, issuer } =
    div []
        [ p [] [ text (holderName |> CC.displayField) ]
        , p [] [ text (number |> CC.displayField) ]
        , p [] [ text (expiration |> CC.displayField) ]
        , p [] [ text (cvc |> CC.displayField) ]
        ]
