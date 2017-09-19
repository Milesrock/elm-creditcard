module CreditCard exposing (..)

import CreditCard.Type exposing (..)
import CreditCard.Constant as Constant
import CreditCard.Helper as Helper


-- INIT


type alias CreditCard =
    { name : Name
    , cardNumber : CardNumber
    , expiration : Expiration
    , cvc : Cvc
    , provider : Maybe Provider
    }


initialCreditCard : CreditCard
initialCreditCard =
    { name = Name "" Nothing
    , cardNumber = CardNumber "" Nothing
    , expiration = Expiration "" TwoOrFourDigits Nothing
    , cvc = Cvc "" Nothing
    , provider = Nothing
    }



-- UPDATE


type Msg
    = UpdateName String
    | UpdateCardNumber String
    | UpdateExpiration String
    | UpdateCvc String
    | ValidateCreditCard


updateCreditCard : Msg -> CreditCard -> CreditCard
updateCreditCard msg creditCard =
    case msg of
        UpdateName name ->
            { creditCard | name = Helper.updateName creditCard.name name }

        UpdateCardNumber cardNumber ->
            { creditCard | cardNumber = Helper.updateCardNumber creditCard.cardNumber cardNumber }

        UpdateExpiration expiration ->
            let
                limitLength =
                    case creditCard.expiration.yearFormat of
                        TwoDigits ->
                            2

                        _ ->
                            4
            in
                { creditCard | expiration = Helper.updateExpiration creditCard.expiration limitLength expiration }

        UpdateCvc cvc ->
            { creditCard | cvc = Helper.updateCvc creditCard.cvc cvc }

        ValidateCreditCard ->
            validateCreditCard creditCard



-- DISPLAY


displayName : Name -> String
displayName =
    .value


displayCardNumber : CardNumber -> String
displayCardNumber =
    .value
        >> Helper.putEvery " " Constant.cardNumberBlockLength


displayExpiration : Expiration -> String
displayExpiration =
    .value


displayCvc : Cvc -> String
displayCvc =
    .value



-- VALIDATE


validateCreditCard : CreditCard -> CreditCard
validateCreditCard creditCard =
    { creditCard
        | name = Helper.validateName creditCard.name
        , cardNumber = Helper.validateCardNumber creditCard.cardNumber
        , expiration = Helper.validateExpiration creditCard.expiration
        , cvc = Helper.validateCvc creditCard.cvc
    }
