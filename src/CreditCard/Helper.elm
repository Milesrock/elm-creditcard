module CreditCard.Helper exposing (..)

import CreditCard.Type exposing (..)
import Regex
import CreditCard.Constant as Constant


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



-- UPDATE


updateName : Name -> String -> Name
updateName name nameValue =
    { name
        | value = formatName nameValue
        , valid = Nothing
    }


updateCardNumber : CardNumber -> String -> CardNumber
updateCardNumber cardNumber cardNumberValue =
    { cardNumber
        | value = formatCardNumber cardNumberValue
        , valid = Nothing
    }


updateExpiration : Expiration -> Int -> String -> Expiration
updateExpiration expiration limitLength expirationValue =
    { expiration
        | value = formatExpiration expiration.value expirationValue limitLength
        , valid = Nothing
    }


updateCvc : Cvc -> String -> Cvc
updateCvc cvc cvcValue =
    { cvc
        | value = formatCvc cvcValue
        , valid = Nothing
    }



-- FORMAT


formatName : String -> String
formatName =
    String.left Constant.nameMaxLength


formatCardNumber : String -> String
formatCardNumber =
    onlyNumbers >> String.left Constant.cardNumberMaxLength


formatExpiration : String -> String -> Int -> String
formatExpiration currentValue newValue limitLength =
    let
        expirationRegex =
            Regex.regex ("^\\d{1,2}(?:/\\d{0," ++ (toString limitLength) ++ "})?$")
    in
        if Regex.contains expirationRegex newValue then
            newValue
        else
            currentValue


formatCvc : String -> String
formatCvc =
    onlyNumbers >> String.left Constant.cvcMaxLength



-- VALIDATIONS


validateName : Name -> Name
validateName name =
    let
        rangeRegexSnippet =
            (toString Constant.nameMinLength) ++ "," ++ (toString Constant.nameMaxLength)

        validationRegex =
            "^[A-Za-z0-9 ]{" ++ rangeRegexSnippet ++ "}$"

        isValid =
            Regex.contains (Regex.regex validationRegex) name.value
    in
        { name | valid = Just isValid }


validateCardNumber : CardNumber -> CardNumber
validateCardNumber cardNumber =
    let
        rangeRegexSnippet =
            (toString Constant.cardNumberMinLength) ++ "," ++ (toString Constant.cardNumberMaxLength)

        validationRegex =
            "^\\d{" ++ rangeRegexSnippet ++ "}$"

        isValid =
            Regex.contains (Regex.regex validationRegex) cardNumber.value
    in
        { cardNumber | valid = Just isValid }


validateExpiration : Expiration -> Expiration
validateExpiration expiration =
    let
        isValid =
            validateExpirationDate expiration
    in
        { expiration | valid = Just isValid }


validateExpirationDate : Expiration -> Bool
validateExpirationDate { value, yearFormat } =
    let
        regexSnippet =
            case yearFormat of
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


validateCvc : Cvc -> Cvc
validateCvc cvc =
    let
        rangeRegexSnippet =
            (toString Constant.cvcMinLength) ++ "," ++ (toString Constant.cvcMaxLength)

        isValid =
            Regex.contains (Regex.regex ("^\\d{" ++ rangeRegexSnippet ++ "}$")) cvc.value
    in
        { cvc | valid = Just isValid }
