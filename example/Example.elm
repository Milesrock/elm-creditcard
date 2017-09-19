module Example exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import CreditCard


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { creditCard : CreditCard.CreditCard }


model : Model
model =
    { creditCard = CreditCard.initialCreditCard }



-- UPDATE


type Msg
    = UpdateCreditCard CreditCard.Msg
    | ValidateCreditCard


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCreditCard creditCardMsg ->
            { model | creditCard = (CreditCard.updateCreditCard creditCardMsg model.creditCard) }

        ValidateCreditCard ->
            { model | creditCard = (CreditCard.updateCreditCard CreditCard.ValidateCreditCard model.creditCard) }



-- VIEW


view : Model -> Html Msg
view ({ creditCard } as model) =
    div []
        [ input
            [ type_ "text"
            , placeholder "Name"
            , onInput (UpdateCreditCard << CreditCard.UpdateName)
            , value (creditCard.name |> CreditCard.displayName)
            , style
                [ if creditCard.name.valid == Just False then
                    ( "backgroundColor", "red" )
                  else if creditCard.name.valid == Just True then
                    ( "backgroundColor", "green" )
                  else
                    ( "backgroundColor", "transparent" )
                ]
            ]
            []
        , input
            [ type_ "text"
            , placeholder "Card number"
            , onInput (UpdateCreditCard << CreditCard.UpdateCardNumber)
            , value (creditCard.cardNumber |> CreditCard.displayCardNumber)
            , style
                [ if creditCard.cardNumber.valid == Just False then
                    ( "backgroundColor", "red" )
                  else if creditCard.cardNumber.valid == Just True then
                    ( "backgroundColor", "green" )
                  else
                    ( "backgroundColor", "transparent" )
                ]
            ]
            []
        , input
            [ type_ "text"
            , placeholder "Expiration"
            , onInput (UpdateCreditCard << CreditCard.UpdateExpiration)
            , value (creditCard.expiration |> CreditCard.displayExpiration)
            , style
                [ if creditCard.expiration.valid == Just False then
                    ( "backgroundColor", "red" )
                  else if creditCard.expiration.valid == Just True then
                    ( "backgroundColor", "green" )
                  else
                    ( "backgroundColor", "transparent" )
                ]
            ]
            []
        , input
            [ type_ "text"
            , placeholder "Cvc"
            , onInput (UpdateCreditCard << CreditCard.UpdateCvc)
            , value (creditCard.cvc |> CreditCard.displayCvc)
            , style
                [ if creditCard.cvc.valid == Just False then
                    ( "backgroundColor", "red" )
                  else if creditCard.cvc.valid == Just True then
                    ( "backgroundColor", "green" )
                  else
                    ( "backgroundColor", "transparent" )
                ]
            ]
            []
        , button [ onClick ValidateCreditCard ] [ text "Valider la carte" ]
        , p [] [ text (toString model.creditCard) ]
        ]
