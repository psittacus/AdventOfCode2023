module Trebuchet exposing (..)
import Browser
import Html exposing (Html, Attribute, button, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String exposing (..)
import Char exposing (..)

main =
  Browser.sandbox { init = init, update = update, view = view }

type Msg = Translate | Draft String

type alias Model = { content : String , output : String} 

type IntOrStr = AnInt Int | AChar Char 

init : Model 
init = { content = "", output = ""}

update : Msg -> Model -> Model
update msg model = 
    case msg of
        Translate ->
            { model | content = model.content, output = getFirstAndLast (extractNumbers model).output }

        Draft input -> 
            { model | content = input }

extractNumbers : Model -> Model
extractNumbers model =  
    case (uncons model.content) of 
        Just (x, xs) -> 
            { model | output = numberOrString x ++ (extractNumbers {model | content = xs}).output}
        Nothing -> 
            { model | output = "", content = "" }

view : Model -> Html Msg
view model =
  div []
    [ input [ value model.content , onInput Draft ] []
    , button [ onClick Translate ] [text "Translate"]
    , div [] [text model.output]
    ]

numberOrString : Char -> String
numberOrString x = 
    if isDigit x then
        String.fromChar x
    else
        ""

getFirstAndLast : String -> String
getFirstAndLast str =
    if String.length str == 1 then
        repeat 2 str
    else if String.length str == 2 then
        str
    else if String.length str > 2 then
        left 1 str ++ right 1 str
    else
        "Number smaller than 1"

