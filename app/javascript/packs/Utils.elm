module Utils exposing (..)


listContains : List a -> a -> Bool
listContains list value =
    (List.any (\i -> i == value) list)
