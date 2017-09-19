Faire la documentation

Faire une fonction d'initialisation de CB plus sexy.

Mettre en place la détection du provider :

```
cardProvider : String -> CardProvider
cardProvider cardNumber =
    case cardNumber of
        Regex.find "^5[1-5][0-9]{14}$" ->
            Mastercard
        Regex.find "^4[0-9]{12}([0-9]{3})?$" ->
            Visa
        Regex.find "^3[47][0-9]{13}$" ->
            American Express
        Regex.find "^3(0[0-5]|[68][0-9])[0-9]{11}$" ->
            Diners Club
        Regex.find "^6011[0-9]{12}$" ->
            Discover
        Regex.find "^(3[0-9]{4}|2131|1800)[0-9]{11}$" ->
            JCB
        _ ->
            Other
```
