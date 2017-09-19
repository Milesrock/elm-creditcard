module CreditCard exposing (..)

import CreditCard.Constant as Constant
import Regex


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


initStringFieldWithoutOptions : StringFieldWithoutOptions
initStringFieldWithoutOptions =
    { value = ""
    , valid = NotTested
    , options = ()
    }


type alias Expiration =
    StringField { yearFormat : YearFormat }


initExpiration : Expiration
initExpiration =
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


type alias CreditCard =
    { cardHolderNameField : StringFieldWithoutOptions
    , cardNumberField : StringFieldWithoutOptions
    , cvcField : StringFieldWithoutOptions
    , expirationField : Expiration
    , provider : Maybe Provider
    }


initialCreditCard : CreditCard
initialCreditCard =
    { cardHolderNameField = initStringFieldWithoutOptions
    , cardNumberField = initStringFieldWithoutOptions
    , cvcField = initStringFieldWithoutOptions
    , expirationField = initExpiration
    , provider = Nothing
    }


type Msg
    = UpdateName String
    | UpdateCardNumber String
    | UpdateExpiration String
    | UpdateCvc String
    | ValidateCreditCard



-- Text Formatters


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



-- Display


displayCardNumber : String -> String
displayCardNumber =
    putEvery " " Constant.cardNumberBlockLength



-- VALIDATIONS


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
                    "\\d{2}\\d{2}?"

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
                    x >= 2017

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



-- UPDATE


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


updateCardNumberField : String -> CreditCard -> CreditCard
updateCardNumberField cardNumber creditCard =
    { creditCard
        | cardNumberField =
            creditCard.cardNumberField
                |> unsetValid
                >> setValue (formatCardNumber cardNumber)
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
    in
        { creditCard
            | expirationField =
                creditCard.expirationField
                    |> unsetValid
                    >> (if partiallyValidExpiration limitLength expiration then
                            setValue expiration
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


updateCreditCard : Msg -> CreditCard -> CreditCard
updateCreditCard msg =
    case msg of
        UpdateName name ->
            updateCardHolderNameField name

        UpdateCardNumber cardNumber ->
            updateCardNumberField cardNumber

        UpdateExpiration expiration ->
            updateExpirationField expiration

        UpdateCvc cvc ->
            updateCvcField cvc

        ValidateCreditCard ->
            validateCreditCard
