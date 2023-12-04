module Trebuchet exposing (..)
import Browser
import Html exposing (Html, Attribute, button, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String exposing (..)
import List exposing (..)
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
            { model | content = model.content, output = String.fromInt(fromListToString (List.map getFirstAndLast (extractNumbers (split " " model.content))))}

        Draft input -> 
            { model | content = input }

fromListToString : List String -> Int 
fromListToString list = 
    case (head list) of
        Just x -> 
            case toInt x of 
                Just val -> 
                    val + fromListToString (drop 1 list)
                    
                Nothing -> 
                    0

        Nothing -> 
            0

extractNumbers : List String -> List String
extractNumbers str =  
    case (head str) of 
        Just x -> 
            singleton (returnJustNumbers x) ++ (extractNumbers (drop 1 str))
--            { model | output = numberOrString x ++ (extractNumbers {model | content = xs}).output}
        Nothing -> 
            []

returnJustNumbers: String -> String
returnJustNumbers str = case (uncons str) of
    Just (x, xs) -> 
        numberOrString x ++ returnJustNumbers xs
    Nothing -> 
        ""
    

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
        String.repeat 2 str
    else if String.length str == 2 then
        str
    else if String.length str > 2 then
        left 1 str ++ right 1 str
    else
        "Number smaller than 1"

