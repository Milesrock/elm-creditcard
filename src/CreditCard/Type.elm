module CreditCard.Type exposing (..)


type Provider
    = Mastercard
    | Visa
    | AmericanExpress
    | DinersClub
    | Discover
    | JCB
    | Other


type alias Name =
    { value : String
    , valid : Maybe Bool
    }


type alias CardNumber =
    { value : String
    , valid : Maybe Bool
    }


type alias Expiration =
    { value : String
    , yearFormat : YearFormat
    , valid : Maybe Bool
    }


type alias Cvc =
    { value : String
    , valid : Maybe Bool
    }


type YearFormat
    = TwoDigits
    | FourDigits
    | TwoOrFourDigits
