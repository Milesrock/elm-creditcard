module CreditCard exposing (..)

{-
   MODEL
-}


type alias CreditCard =
    { holderName : Field
    , number : Field
    , expiration : Field
    , cvc : Field
    , issuer : Maybe Issuer
    }


initCreditCardDefault : CreditCard
initCreditCardDefault =
    initCreditCard TwoOrFourDigits True


initCreditCard : YearFormat -> Bool -> CreditCard
initCreditCard yearFormat separateDisplay =
    { holderName = HolderName (initFieldContent ())
    , number = Number (initFieldContent ())
    , expiration = Expiration (initFieldContent (initExpirationOptions yearFormat separateDisplay))
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
    { month : Int
    , year : Int
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


type Issuer
    = Visa
    | Mastercard
    | AmericanExpress
    | DinersClub
    | Discover
    | JCB
    | Other


type YearFormat
    = TwoDigits
    | FourDigits
    | TwoOrFourDigits



{-
   MSG
-}
