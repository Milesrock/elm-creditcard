module CreditCard
    exposing
        ( CreditCard
        , Field
        , YearFormat
        , Issuer(..)
        , Valid(..)
        , Msg(..)
        , initCreditCard
        , initCreditCardDefault
        , displayField
        , update
        , isValid
        )

{-|
@docs CreditCard
@docs Field
@docs YearFormat
@docs Issuer
@docs Valid
@docs Msg
@docs initCreditCard
@docs initCreditCardDefault
@docs displayField
@docs update
@docs isValid
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
                (initExpirationOptions yearFormat)
            )
    , cvc = Cvc (initFieldContent ())
    , issuer = Nothing
    }


{-| Union type of all creditcard fields.
-}
type Field
    = HolderName (FieldContent ())
    | Number (FieldContent ())
    | Expiration (FieldContent ExpirationOptions)
    | Cvc (FieldContent ())


type alias FieldContent optionsType =
    { value : Maybe String
    , valid : Valid
    , options : optionsType
    }


initFieldContent : a -> FieldContent a
initFieldContent fieldOptions =
    { value = Nothing
    , valid = NotTested
    , options = fieldOptions
    }


type alias ExpirationOptions =
    { yearFormat : YearFormat
    }


initExpirationOptions : YearFormat -> ExpirationOptions
initExpirationOptions yearFormat =
    { yearFormat = yearFormat
    }


{-| -}
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


displayNumber : String -> String
displayNumber value =
    Helper.putEvery " " Constant.numberBlockLength value


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
            displayMaybeValue fieldContent.value identity

        Number fieldContent ->
            displayMaybeValue fieldContent.value displayNumber

        Expiration fieldContent ->
            displayMaybeValue fieldContent.value identity

        Cvc fieldContent ->
            displayMaybeValue fieldContent.value identity



{-
   UPDATE
-}
-- FIELDS UPDATE FUNCTIONS


partiallyValidExpiration : Int -> String -> Bool
partiallyValidExpiration limitLength =
    Regex.contains
        (Regex.regex ("^\\d{1,2}(?:/\\d{0," ++ (toString limitLength) ++ "})?$"))


formatHolderName : String -> String
formatHolderName =
    Helper.onlyHolderNameCharacters >> String.left Constant.holderNameMaxLength


formatCardNumber : String -> String
formatCardNumber =
    Helper.onlyNumbers >> String.left Constant.numberMaxLength


formatExpiration : FieldContent ExpirationOptions -> String -> String
formatExpiration fieldContent input =
    let
        limitLength =
            (if fieldContent.options.yearFormat == TwoDigits then
                2
             else
                4
            )

        threeDigitsCheck =
            Regex.contains (Regex.regex ("^\\d{3,}$"))

        insertSlashAtTwo =
            (String.left 2 input) ++ "/" ++ (String.dropLeft 2 input)
    in
        if partiallyValidExpiration limitLength input then
            input
        else if threeDigitsCheck input then
            insertSlashAtTwo
        else
            case fieldContent.value of
                Nothing ->
                    ""

                Just value ->
                    value


formatCvc : String -> String
formatCvc =
    Helper.onlyNumbers >> String.left Constant.cvcMaxLength


updateFieldContentOnSetValue : String -> FieldContent a -> FieldContent a
updateFieldContentOnSetValue setNewValue fieldContent =
    { fieldContent
        | value = Just setNewValue
        , valid = NotTested
    }


setHolderName : String -> FieldContent () -> CreditCard -> CreditCard
setHolderName newValue fieldContent creditCard =
    { creditCard | holderName = HolderName (updateFieldContentOnSetValue newValue fieldContent) }


setNumber : String -> FieldContent () -> CreditCard -> CreditCard
setNumber newValue fieldContent creditCard =
    { creditCard
        | number = Number (updateFieldContentOnSetValue newValue fieldContent)
        , issuer = identifyIssuer newValue
    }


setExpiration : String -> FieldContent ExpirationOptions -> CreditCard -> CreditCard
setExpiration newValue fieldContent creditCard =
    { creditCard | expiration = Expiration (updateFieldContentOnSetValue newValue fieldContent) }


setCvc : String -> FieldContent () -> CreditCard -> CreditCard
setCvc newValue fieldContent creditCard =
    { creditCard | cvc = Cvc (updateFieldContentOnSetValue newValue fieldContent) }


setValue : Field -> String -> CreditCard -> CreditCard
setValue field input =
    case field of
        HolderName fieldContent ->
            setHolderName (formatHolderName input) fieldContent

        Number fieldContent ->
            setNumber (formatCardNumber input) fieldContent

        Expiration fieldContent ->
            setExpiration (formatExpiration fieldContent input) fieldContent

        Cvc fieldContent ->
            setCvc (formatCvc input) fieldContent



-- FIELDS VALIDATION FUNCTION


validateHolderName : String -> Bool
validateHolderName =
    let
        validationRegex =
            "^[A-Za-z'-. ]{"
                ++ (toString Constant.holderNameMinLength)
                ++ ","
                ++ (toString Constant.holderNameMaxLength)
                ++ "}$"
    in
        Regex.contains (Regex.regex validationRegex)


validateNumber : String -> Bool
validateNumber =
    Regex.contains (Regex.regex "^\\d{12,19}$")


validateExpiration : ExpirationOptions -> String -> Bool
validateExpiration options value =
    let
        month =
            String.left 2 value |> String.toInt

        year =
            String.dropLeft 3 value |> String.toInt

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


validateCvc : String -> Bool
validateCvc value =
    let
        rangeRegexSnippet =
            (toString Constant.cvcMinLength) ++ "," ++ (toString Constant.cvcMaxLength)
    in
        Regex.contains (Regex.regex ("^\\d{" ++ rangeRegexSnippet ++ "}$")) value


validateMaybeValue : Maybe a -> (a -> Bool) -> Bool
validateMaybeValue maybeValue validationFunction =
    case maybeValue of
        Nothing ->
            False

        Just value ->
            validationFunction value


validateField : Field -> Field
validateField field =
    let
        updateValid fieldType fieldContent validationFunction =
            fieldType { fieldContent | valid = Tested (validateMaybeValue fieldContent.value validationFunction) }
    in
        case field of
            HolderName fieldContent ->
                updateValid HolderName fieldContent validateHolderName

            Number fieldContent ->
                updateValid Number fieldContent validateNumber

            Expiration fieldContent ->
                updateValid Expiration fieldContent (validateExpiration fieldContent.options)

            Cvc fieldContent ->
                updateValid Cvc fieldContent validateCvc


{-| -}
validateCreditCard : CreditCard -> CreditCard
validateCreditCard creditCard =
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
    = SetValue Field String
    | Validate


{-| -}
update : Msg -> CreditCard -> CreditCard
update msg =
    case msg of
        SetValue field input ->
            setValue field input

        Validate ->
            validateCreditCard


identifyIssuer : String -> Maybe Issuer
identifyIssuer cardNumber =
    if (String.length cardNumber) >= 4 then
        if Regex.contains (Regex.regex "^5[1-5]") cardNumber then
            Just Mastercard
        else if Regex.contains (Regex.regex "^4") cardNumber then
            Just Visa
        else if Regex.contains (Regex.regex "^3[47]") cardNumber then
            Just AmericanExpress
        else if Regex.contains (Regex.regex "^3(0[0-5]|[68])") cardNumber then
            Just DinersClub
        else if Regex.contains (Regex.regex "^6011") cardNumber then
            Just Discover
        else if Regex.contains (Regex.regex "^(3|2131|1800)") cardNumber then
            Just JCB
        else
            Just Other
    else
        Nothing


{-| -}
isValid : Field -> Valid
isValid field =
    case field of
        HolderName fieldContent ->
            fieldContent.valid

        Number fieldContent ->
            fieldContent.valid

        Expiration fieldContent ->
            fieldContent.valid

        Cvc fieldContent ->
            fieldContent.valid
