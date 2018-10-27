data Date = Date Int Int Int
data Anniversary = Birthday String Date
                 | Wedding  String String Date

showDate :: Date -> String
showDate (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

showAnniversary :: Anniversary -> String
showAnniversary (Birthday name date) =
 name ++ " born " ++ showDate date
showAnniversary (Wedding name1 name2 date) =
 name1 ++ " married " ++ name2 ++ " on " ++ showDate date
