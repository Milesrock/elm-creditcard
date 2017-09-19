module Example exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import CreditCard exposing (Valid(..))


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


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCreditCard creditCardMsg ->
            { model | creditCard = CreditCard.updateCreditCard creditCardMsg model.creditCard }



-- VIEW


view : Model -> Html Msg
view ({ creditCard } as model) =
    div []
        [ input
            [ type_ "text"
            , placeholder "Name"
            , onInput (UpdateCreditCard << CreditCard.UpdateName)
            , value (creditCard.cardHolderNameField.value)
            , style
                [ if creditCard.cardHolderNameField.valid == Tested False then
                    ( "backgroundColor", "red" )
                  else if creditCard.cardHolderNameField.valid == Tested True then
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
            , value (creditCard.cardNumberField.value |> CreditCard.displayCardNumber)
            , style
                [ if creditCard.cardNumberField.valid == Tested False then
                    ( "backgroundColor", "red" )
                  else if creditCard.cardNumberField.valid == Tested True then
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
            , value (creditCard.expirationField.value)
            , style
                [ if creditCard.expirationField.valid == Tested False then
                    ( "backgroundColor", "red" )
                  else if creditCard.expirationField.valid == Tested True then
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
            , value (creditCard.cvcField.value)
            , style
                [ if creditCard.cvcField.valid == Tested False then
                    ( "backgroundColor", "red" )
                  else if creditCard.cvcField.valid == Tested True then
                    ( "backgroundColor", "green" )
                  else
                    ( "backgroundColor", "transparent" )
                ]
            ]
            []
        , button [ onClick (UpdateCreditCard CreditCard.ValidateCreditCard) ] [ text "Valider la carte" ]
        , p [] [ text (toString model.creditCard) ]
        ]
