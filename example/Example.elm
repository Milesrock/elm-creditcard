module Example exposing (..)

import BodyBuilder exposing (..)
import Elegant exposing (..)
import CreditCard as CC exposing (Valid(..))
import Color


main : Program Never Model Msg
main =
    BodyBuilder.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { creditCard : CC.CreditCard }


init : ( Model, Cmd Msg )
init =
    { creditCard = CC.initialCreditCard } ! []



-- UPDATE


type Msg
    = UpdateCreditCard CC.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCreditCard creditCardMsg ->
            { model | creditCard = CC.updateCreditCard creditCardMsg model.creditCard } ! []



-- VIEW


containerStyle =
    style
        [ marginAuto
        , Elegant.width (Px 300)
        , fontFamilySansSerif
        , marginVertical huge
        , padding large
        , borderSolid
        , borderColor (Color.rgb 200 200 200)
        , borderWidth 1
        ]


formComponentStyle color =
    style
        [ displayBlock
        , padding tiny
        , borderSolid
        , borderColor (Color.rgb 200 200 200)
        , borderWidth 1
        , marginVertical medium
        , borderRadius 4
        , backgroundColor color
        , fullWidth
        , textCenter
        ]


inputField placeholderValue msg field display =
    let
        inputBackgroundColor =
            case field.valid of
                NotTested ->
                    transparent

                Tested True ->
                    Color.rgb 200 240 220

                Tested False ->
                    Color.rgb 250 215 220
    in
        inputText
            [ placeholder placeholderValue
            , onInput (UpdateCreditCard << msg)
            , value (display field)
            , formComponentStyle inputBackgroundColor
            ]


view ({ creditCard } as model) =
    div
        [ containerStyle
        ]
        [ inputField "Name" CC.UpdateCardHolderName creditCard.cardHolderNameField .value
        , inputField "Card number" CC.UpdateCardNumber creditCard.cardNumberField (.value >> CC.displayCardNumber)
        , inputField "Expiration" CC.UpdateExpiration creditCard.expirationField .value
        , inputField "Cvc" CC.UpdateCvc creditCard.cvcField .value
        , div
            [ onClick (UpdateCreditCard CC.ValidateCreditCard)
            , style [ cursorPointer ]
            , formComponentStyle (Color.rgb 0 125 255)
            ]
            [ text "Valider la carte" ]
        ]
