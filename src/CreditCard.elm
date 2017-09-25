module CreditCard
    exposing
        ( CreditCard
        , Valid(..)
        , Msg(..)
        , initialCreditCard
        , updateCreditCard
        , displayCardNumber
        )

{-|

@docs CreditCard
@docs Valid(..)
@docs Msg(..)
@docs initialCreditCard
@docs updateCreditCard
@docs displayCardNumber

-}

import CreditCard.Constant as Constant
import Regex


--
-- TYPES
--


{-| Usually set to NotTested. When testing a field it's set to Tested True or Tested False, depending on the result of the test.
-}
type Valid
    = NotTested
    | Tested Bool


type alias Field valueType optionsType =
    { valid : Valid
    , value : valueType
    , options : optionsType
    }


type alias StringField a =
    Field String a


type alias StringFieldWithoutOptions =
    StringField ()


initialStringFieldWithoutOptions : StringFieldWithoutOptions
initialStringFieldWithoutOptions =
    { value = ""
    , valid = NotTested
    , options = ()
    }


type alias Expiration =
    StringField { yearFormat : YearFormat }


initialExpiration : Expiration
initialExpiration =
    { value = ""
    , valid = NotTested
    , options = { yearFormat = TwoOrFourDigits }
    }


type YearFormat
    = TwoDigits
    | FourDigits
    | TwoOrFourDigits


type Provider
    = Mastercard
    | Visa
    | AmericanExpress
    | DinersClub
    | Discover
    | JCB
    | Other


{-| The main model for this package.
-}
type alias CreditCard =
    { cardHolderNameField : StringFieldWithoutOptions
    , cardNumberField : StringFieldWithoutOptions
    , cvcField : StringFieldWithoutOptions
    , expirationField : Expiration
    , provider : Maybe Provider
    }


{-| The function to initialize a new credit card model.
-}
initialCreditCard : CreditCard
initialCreditCard =
    { cardHolderNameField = initialStringFieldWithoutOptions
    , cardNumberField = initialStringFieldWithoutOptions
    , cvcField = initialStringFieldWithoutOptions
    , expirationField = initialExpiration
    , provider = Nothing
    }


{-| Updating messages.
-}
type Msg
    = UpdateCardHolderName String
    | UpdateCardNumber String
    | UpdateExpiration String
    | UpdateCvc String
    | ValidateCreditCard



--
-- TEXT FORMATTERS
--


splitEvery : Int -> String -> List String
splitEvery blockLength =
    let
        splittingRegex =
            "[0-9]{1," ++ (toString blockLength) ++ "}"

        regexList =
            Regex.find Regex.All (Regex.regex splittingRegex)
    in
        regexList >> List.map .match


putEvery : String -> Int -> String -> String
putEvery separator blockLength =
    splitEvery blockLength >> String.join separator


removeRegex : String -> String -> String
removeRegex regex =
    Regex.replace Regex.All (Regex.regex regex) (\_ -> "")


onlyNumbers : String -> String
onlyNumbers =
    removeRegex "\\D"


onlyNumbersAndSlash : String -> String
onlyNumbersAndSlash =
    Regex.replace Regex.All
        (Regex.regex "\\D")
        (\{ match } ->
            if match == "/" then
                "/"
            else
                ""
        )


formatName : String -> String
formatName =
    String.left Constant.nameMaxLength


formatCardNumber : String -> String
formatCardNumber =
    onlyNumbers >> String.left Constant.cardNumberMaxLength


formatCvc : String -> String
formatCvc =
    onlyNumbers >> String.left Constant.cvcMaxLength


identifyProvider : String -> Maybe Provider
identifyProvider cardNumber =
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



--
-- DISPLAY
--


{-| The function to display the card number with a space every 4 numbers.
-}
displayCardNumber : String -> String
displayCardNumber =
    putEvery " " Constant.cardNumberBlockLength



--
-- VALIDATIONS
--


partiallyValidExpiration : Int -> String -> Bool
partiallyValidExpiration limitLength =
    Regex.contains
        (Regex.regex ("^\\d{1,2}(?:/\\d{0," ++ (toString limitLength) ++ "})?$"))


validCardHolderName : String -> Bool
validCardHolderName =
    let
        rangeRegexSnippet =
            (toString Constant.nameMinLength) ++ "," ++ (toString Constant.nameMaxLength)

        validationRegex =
            "^[A-Za-z0-9 ]{" ++ rangeRegexSnippet ++ "}$"
    in
        Regex.contains (Regex.regex validationRegex)


validCardNumber : String -> Bool
validCardNumber =
    let
        rangeRegexSnippet =
            (toString Constant.cardNumberMinLength) ++ "," ++ (toString Constant.cardNumberMaxLength)

        validationRegex =
            "^\\d{" ++ rangeRegexSnippet ++ "}$"
    in
        Regex.contains (Regex.regex validationRegex)


validateName : StringFieldWithoutOptions -> StringFieldWithoutOptions
validateName field =
    field
        |> setValid (Tested (validCardHolderName field.value))


validateCardNumber : StringFieldWithoutOptions -> StringFieldWithoutOptions
validateCardNumber field =
    field
        |> setValid (Tested (validCardNumber field.value))


validateExpiration : Expiration -> Expiration
validateExpiration field =
    field
        |> setValid (Tested (validateExpirationDate field))


validateExpirationDate : Expiration -> Bool
validateExpirationDate { value, options } =
    let
        regexSnippet =
            case options.yearFormat of
                TwoDigits ->
                    "\\d{2}"

                FourDigits ->
                    "\\d{4}"

                TwoOrFourDigits ->
                    "\\d{2}(?:\\d{2})?"

        month =
            value
                |> String.left 2
                |> String.toInt

        year =
            value
                |> String.dropLeft 3
                |> String.toInt

        isValidFormat =
            Regex.contains (Regex.regex ("^\\d{2}/" ++ regexSnippet ++ "$")) value

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
        isValidFormat && isValidMonth && isValidYear


validateCvc : StringFieldWithoutOptions -> StringFieldWithoutOptions
validateCvc cvc =
    let
        rangeRegexSnippet =
            (toString Constant.cvcMinLength) ++ "," ++ (toString Constant.cvcMaxLength)

        isValid =
            Regex.contains (Regex.regex ("^\\d{" ++ rangeRegexSnippet ++ "}$")) cvc.value
    in
        { cvc | valid = Tested isValid }


validateCreditCard : CreditCard -> CreditCard
validateCreditCard creditCard =
    { creditCard
        | cardHolderNameField = validateName creditCard.cardHolderNameField
        , cardNumberField = validateCardNumber creditCard.cardNumberField
        , expirationField = validateExpiration creditCard.expirationField
        , cvcField = validateCvc creditCard.cvcField
    }



--
-- UPDATE
--


unsetValid : Field a b -> Field a b
unsetValid field =
    { field | valid = NotTested }


setValid : Valid -> Field a b -> Field a b
setValid valid field =
    { field | valid = valid }


setValue : a -> Field a b -> Field a b
setValue value field =
    { field | value = value }


updateCardHolderNameField : String -> CreditCard -> CreditCard
updateCardHolderNameField cardHolderName creditCard =
    { creditCard
        | cardHolderNameField =
            creditCard.cardHolderNameField
                |> unsetValid
                >> setValue (formatName cardHolderName)
    }


updateCardNumberAndProviderFields : String -> CreditCard -> CreditCard
updateCardNumberAndProviderFields cardNumber creditCard =
    let
        newCardNumberField =
            creditCard.cardNumberField
                |> unsetValid
                >> setValue (formatCardNumber cardNumber)
    in
        { creditCard
            | cardNumberField = newCardNumberField
            , provider = identifyProvider newCardNumberField.value
        }


updateExpirationField : String -> CreditCard -> CreditCard
updateExpirationField expiration creditCard =
    let
        limitLength =
            (if creditCard.expirationField.options.yearFormat == TwoDigits then
                2
             else
                4
            )

        threeDigitsCheck =
            Regex.contains (Regex.regex ("^\\d{3,}$"))

        insertSlashAtTwo =
            (String.left 2 expiration) ++ "/" ++ (String.dropLeft 2 expiration)
    in
        { creditCard
            | expirationField =
                creditCard.expirationField
                    |> unsetValid
                    >> (if partiallyValidExpiration limitLength expiration then
                            setValue expiration
                        else if threeDigitsCheck expiration then
                            setValue insertSlashAtTwo
                        else
                            identity
                       )
        }


updateCvcField : String -> CreditCard -> CreditCard
updateCvcField cvc creditCard =
    { creditCard
        | cvcField =
            creditCard.cvcField
                |> unsetValid
                >> setValue (formatCvc cvc)
    }


{-| The updating function for this package.
-}
updateCreditCard : Msg -> CreditCard -> CreditCard
updateCreditCard msg =
    case msg of
        UpdateCardHolderName name ->
            updateCardHolderNameField name

        UpdateCardNumber cardNumber ->
            updateCardNumberAndProviderFields cardNumber

        UpdateExpiration expiration ->
            updateExpirationField expiration

        UpdateCvc cvc ->
            updateCvcField cvc

        ValidateCreditCard ->
            validateCreditCard
