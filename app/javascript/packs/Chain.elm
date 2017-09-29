module Chain exposing (Chain(..), andThen, flatten)


type Chain intermidiate value
    = Continue intermidiate
    | Stop value


andThen : (a -> Chain b value) -> Chain a value -> Chain b value
andThen callback result =
    case result of
        Continue intermediate ->
            callback intermediate

        Stop value ->
            Stop value


flatten : Chain value value -> value
flatten chainValue =
    case chainValue of
        Continue value ->
            value

        Stop value ->
            value
