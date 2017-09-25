module Blah exposing (..)

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



-- INIT


type alias Model =
    { creditCard : CC.CreditCard }


init : ( Model, Cmd Msg )
init =
    { creditCard = CC.initCreditCardDefault } ! []



-- UPDATE


type Msg
    = UpdateCreditCard CC.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCreditCard creditCardMsg ->
            { model | creditCard = CC.update creditCardMsg model.creditCard } ! []



-- VIEW


displayIssuer : Maybe CC.Issuer -> String
displayIssuer issuer =
    case issuer of
        Nothing ->
            ""

        Just i ->
            toString i


containerStyle :
    { a | style : StyleAttribute }
    -> { a | style : StyleAttribute }
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


formComponentStyle :
    Color.Color
    -> { a | style : StyleAttribute }
    -> { a | style : StyleAttribute }
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
        , fontSize (Px 14)
        ]


inputField : String -> CC.Field -> Node Interactive phrasingContent spanningContent listContent Msg
inputField placeholderValue field =
    let
        inputBackgroundColor =
            case CC.isValid field of
                NotTested ->
                    transparent

                Tested True ->
                    Color.rgb 200 240 220

                Tested False ->
                    Color.rgb 250 215 220
    in
        inputText
            [ placeholder placeholderValue
            , onInput (UpdateCreditCard << CC.SetValue field)
            , value (CC.displayField field)
            , formComponentStyle inputBackgroundColor
            ]


view : Model -> Node Interactive NotPhrasing Spanning NotListElement Msg
view { creditCard } =
    div [ containerStyle ]
        [ inputField "Name" creditCard.holderName
        , inputField "Card number" creditCard.number
        , p [ style [ textCenter ] ] [ text (displayIssuer creditCard.issuer) ]
        , inputField "Expiration" creditCard.expiration
        , inputField "Cvc" creditCard.cvc
        , div
            [ onClick (UpdateCreditCard CC.Validate)
            , style [ cursorPointer, fontSize (Px 14) ]
            , formComponentStyle (Color.rgb 0 125 255)
            ]
            [ text "Valider la carte" ]
        ]
