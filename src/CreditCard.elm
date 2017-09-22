module CreditCard
    exposing
        ( CreditCard
        , YearFormat
        , Issuer
        , Msg(..)
        , initCreditCard
        , initCreditCardDefault
        , displayField
        , update
        , validate
        )

{-|
@docs CreditCard
@docs YearFormat
@docs Issuer
@docs Msg
@docs initCreditCard
@docs initCreditCardDefault
@docs displayField
@docs update
@docs validate
-}

import CreditCard.Constant as Constant
import CreditCard.Helper as Helper
import Regex


{-
   MODEL
-}


{-| -}
type alias CreditCard =
    { holderName : Field
    , number : Field
    , expiration : Field
    , cvc : Field
    , issuer : Maybe Issuer
    }


{-| -}
initCreditCardDefault : CreditCard
initCreditCardDefault =
    initCreditCard TwoOrFourDigits True


{-| -}
initCreditCard : YearFormat -> Bool -> CreditCard
initCreditCard yearFormat separateDisplay =
    { holderName = HolderName (initFieldContent ())
    , number = Number (initFieldContent ())
    , expiration =
        Expiration
            (initFieldContent
                (initExpirationOptions yearFormat separateDisplay)
            )
    , cvc = Cvc (initFieldContent ())
    , issuer = Nothing
    }


type Field
    = HolderName (FieldContent String ())
    | Number (FieldContent String ())
    | Expiration (FieldContent ExpirationType ExpirationOptions)
    | Cvc (FieldContent Int ())


type alias FieldContent valueType optionsType =
    { value : Maybe valueType
    , valid : Valid
    , options : optionsType
    }


initFieldContent : b -> FieldContent a b
initFieldContent fieldOptions =
    { value = Nothing
    , valid = NotTested
    , options = fieldOptions
    }


type alias ExpirationType =
    { month : String
    , year : String
    }


type alias ExpirationOptions =
    { yearFormat : YearFormat
    , separateDisplay : Bool
    }


initExpirationOptions : YearFormat -> Bool -> ExpirationOptions
initExpirationOptions yearFormat separateDisplay =
    { yearFormat = yearFormat
    , separateDisplay = separateDisplay
    }


type Valid
    = NotTested
    | Tested Bool


{-| -}
type Issuer
    = Visa
    | Mastercard
    | AmericanExpress
    | DinersClub
    | Discover
    | JCB
    | Other


{-| -}
type YearFormat
    = TwoDigits
    | FourDigits
    | TwoOrFourDigits



{-
   DISPLAY
-}


displayCardHolder : String -> String
displayCardHolder value =
    value


displayNumber : String -> String
displayNumber value =
    Helper.putEvery " " Constant.cardNumberBlockLength value


displayExpiration : ExpirationType -> String
displayExpiration value =
    value.month ++ "/" ++ value.year


displayCvc : Int -> String
displayCvc value =
    toString value


displayMaybeValue : Maybe a -> (a -> String) -> String
displayMaybeValue maybeValue displayFunction =
    case maybeValue of
        Nothing ->
            ""

        Just value ->
            displayFunction value


{-| -}
displayField : Field -> String
displayField field =
    case field of
        HolderName fieldContent ->
            displayMaybeValue fieldContent.value displayCardHolder

        Number fieldContent ->
            displayMaybeValue fieldContent.value displayNumber

        Expiration fieldContent ->
            displayMaybeValue fieldContent.value displayExpiration

        Cvc fieldContent ->
            displayMaybeValue fieldContent.value displayCvc



{-
   UPDATE
-}
-- FIELDS UPDATE FUNCTIONS


updateCreditCard : CreditCard -> CreditCard
updateCreditCard creditCard =
    creditCard


updateField : Field -> String -> CreditCard -> CreditCard
updateField field input =
    case field of
        HolderName fieldContent ->
            updateCreditCard

        Number fieldContent ->
            updateCreditCard

        Expiration fieldContent ->
            updateCreditCard

        Cvc fieldContent ->
            updateCreditCard



-- FIELDS VALIDATION FUNCTION


validateHolderName : String -> Bool
validateHolderName =
    Regex.contains (Regex.regex "^[A-Za-z'-. ]{2,26}$")


validateNumber : String -> Bool
validateNumber =
    Regex.contains (Regex.regex "^\\d{12,19}$")


validateExpiration : ExpirationOptions -> ExpirationType -> Bool
validateExpiration options value =
    let
        month =
            value.month |> String.toInt

        year =
            value.year |> String.toInt

        isValidMonth =
            case month of
                Ok x ->
                    x <= 12

                Err _ ->
                    False

        isValidYear =
            case year of
                Ok x ->
                    case options.yearFormat of
                        TwoDigits ->
                            x >= 17

                        FourDigits ->
                            x >= 2017

                        TwoOrFourDigits ->
                            ((x >= 17) && (x <= 99)) || (x >= 2017)

                Err _ ->
                    False
    in
        isValidMonth && isValidYear


validateCvc : Int -> Bool
validateCvc value =
    value >= 100 && value <= 9999


validateMaybeValue : Maybe a -> (a -> Bool) -> Bool
validateMaybeValue maybeValue validationFunction =
    case maybeValue of
        Nothing ->
            False

        Just value ->
            validationFunction value


validateField : Field -> Field
validateField field =
    case field of
        HolderName fieldContent ->
            HolderName { fieldContent | valid = Tested (validateMaybeValue fieldContent.value validateHolderName) }

        Number fieldContent ->
            Number { fieldContent | valid = Tested (validateMaybeValue fieldContent.value validateNumber) }

        Expiration fieldContent ->
            Expiration { fieldContent | valid = Tested (validateMaybeValue fieldContent.value (validateExpiration fieldContent.options)) }

        Cvc fieldContent ->
            Cvc { fieldContent | valid = Tested (validateMaybeValue fieldContent.value validateCvc) }


{-| -}
validate : CreditCard -> CreditCard
validate creditCard =
    { creditCard
        | holderName =
            validateField creditCard.holderName
        , number =
            validateField creditCard.number
        , expiration =
            validateField creditCard.expiration
        , cvc =
            validateField creditCard.cvc
    }


{-| -}
type Msg
    = UpdateField Field String
    | Validate


{-| -}
update : Msg -> CreditCard -> CreditCard
update msg =
    case msg of
        UpdateField field input ->
            updateField field input

        Validate ->
            validate
