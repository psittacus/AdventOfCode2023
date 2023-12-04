module Trebuchet exposing (..)
import Browser
import Html exposing (Html, Attribute, button, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

main =
  Browser.sandbox { init = init, update = update, view = view }

type Msg = Translate | Draft String

type alias Model = { content : String } 

type IntOrStr = Nummer Int | Text String

init : Model 
init = { content = ""}

update : Msg -> Model -> Model
update msg model = 
    case msg of
        Translate ->
            extractNumbers model

        Draft input -> 
            { model | content = input }

extractNumbers : Model -> Model
extractNumbers model =  
    case (uncons model.content) of 
        Just (x, xs) -> 
            { model | content = numberOrString x ++ (extractNumbers { model | content = xs })}
        Nothing -> 
            { model | content = "" }

view : Model -> Html Msg
view model =
  div []
    [ input [ value model.content , onInput Draft ] []
    , button [ onClick Translate ] [text "Translate"]
    ]

numberOrString : IntOrStr -> String
numberOrString x = 
    case x of
        Nummer num -> 
            String.fromInt x
        Text str -> 
            str

