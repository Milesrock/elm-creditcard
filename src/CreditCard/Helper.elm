module CreditCard.Helper exposing (..)

import Regex


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
