import Html exposing (beginnerProgram, program, div, button, text)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import String exposing (..)
import List exposing (..)
import Tuple exposing (..)
import Maybe exposing (..)
import Http
import Json.Decode as Decode
import Svg exposing (svg, rect, circle, line)
import Svg.Attributes 
import Svg.Attributes exposing (viewBox, rx, ry, x, y, x1, x2, y1, y2, cx, cy, r,stroke,strokeWidth, fill)

{-
todo: 
draw the hangman 
get a random word from a webservice like: http://www.setgetgo.com/randomword/get.php
create a welcome screen 
style with css 
-}


type GameState = Playing | Starting | Lost | Won

type Msg = Guess Char | Restart | GetRandomWord | NewWord (Result Http.Error String)

type alias Model = { guess : Char 
                   , hiddenWord : String
                   , guessedSequence : List (Int, Char)
                   , gameState : GameState
                   , failedSequence : List Char
                   }

--main : Program Never Model Msg
main =
  program { init = init, view = view, update = update , subscriptions=subscriptions}

defaultModel : Model
defaultModel = {guess=' ', hiddenWord="sisas", guessedSequence=[], gameState=Starting, failedSequence=[]}

init : (Model, Cmd Msg)
init = (defaultModel, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
    case model.gameState of
        -- Restart screen
        Won -> 
            div [class "hangman-app"]
                [ stylesheet
                , h1 [] [text "You've won"]
                , h2 [] [text <| "The hidden word was: " ++ model.hiddenWord]
                , button [onClick GetRandomWord] [text "Restart"]
                ]
        Lost -> 
            div [class "hangman-app"]
                [ stylesheet
                , h1 [] [text "You loose"]
                , h2 [] [text <| "The hidden word was: " ++ model.hiddenWord]
                , button [onClick Restart] [text "Restart"]
                ]
        -- Home screen
        Starting ->
            div [class "hangman-app"]
                [ stylesheet
                , h1 [] [text "Hangman Game"]
                , button [onClick Restart] [text "Start"]
                ]
        -- Normal Playing Screen
        Playing ->
            div [ class "hangman-app"]
                [ stylesheet
                , h1 [class "app-title"] [text "Hangman"]
                , div [class "hangman-draw"]
                      [ hangmanSvg (List.length model.failedSequence)
                      , h2 [class "guessed-letters"] [text <| showGuessSequence model.guessedSequence model.hiddenWord ]
                      ]
                , h2 [] [text <| "Tries remaining: " ++ toString (maxTries - List.length model.failedSequence)]
                , buttonArray ["a","b","c","d","e","f","g","h","i","j","k","l","m"] model
                , buttonArray ["n","o","p","q","r","s","t","u","v","w","x","y","z"] model
                     
                ]



buttonArray : List String -> Model -> Html Msg
buttonArray l m = 
    div [class "keyboard"] 
        (List.map (\x -> 
            case List.member (stringHead x) (List.append m.failedSequence (List.map second m.guessedSequence))  of
                True -> button [class "letter-button", disabled True][text x]
                False -> button [class "letter-button", onClick (Guess <| stringHead x)][text x]) l)


hangmanSvg n = 
    let 
        xcenter = "50"
        head = circle [cx xcenter, cy "30", r "15", fill "#fff"] []
        body = line [x1 xcenter, y1 "40", x2 xcenter, y2 "100", stroke "#fff", strokeWidth "2"][]
        rightFoot = line [x1 xcenter, y1 "100", x2 "70", y2 "140", stroke "#fff",strokeWidth "2"][]
        leftFoot = line [x1 xcenter, y1 "100", x2 "30", y2 "140", stroke "#fff",strokeWidth "2"][]
        rightHand = line [x1 xcenter, y1 "65", x2 "75", y2 "65", stroke "#fff", strokeWidth "2"][]
        leftHand = line [x1 xcenter, y1 "65", x2 "25", y2 "65", stroke "#fff", strokeWidth "2"][]
        poleBottom = line [x1 "0", y1 "160", x2 "60", y2 "160", stroke "#fff", strokeWidth "2"][]
        poleTop = line [x1 "0", y1 "0", x2 xcenter, y2 "0", stroke "#fff", strokeWidth "2"][]
        pole = line [x1 "0", y1 "0", x2 "0", y2 "160", stroke "#fff", strokeWidth "2"][]
        slipKnot = line [x1 xcenter, y1 "0", x2 xcenter, y2 "30", stroke "#fff", strokeWidth "2"][]
        hangman = [poleBottom, pole, poleTop, slipKnot, head, body, rightFoot, leftFoot, rightHand, leftHand]
    in svg [ Svg.Attributes.width "120", Svg.Attributes.height "160", viewBox "0 0 120 160" ] (List.take n hangman)
            
-- UPDATE


--update : Msg -> Model -> Model 
update : Msg -> Model -> (Model, Cmd Msg) 
update msg model =
  case msg of
    Guess char -> (updateGuessedWord {model | guess=char}, Cmd.none)
    Restart -> ({defaultModel | gameState=Playing}, Cmd.none)
    GetRandomWord -> (model, getRandomWord)
    NewWord (Err x) -> ({model | hiddenWord = toString x}, Cmd.none)
    NewWord (Ok newWord) -> ({defaultModel | hiddenWord = newWord, gameState=Playing}, Cmd.none)


updateGuessedWord : Model -> Model
updateGuessedWord model =
    let 
        guess = model.guess
        letterHasAlreadyBeenUsed = List.member guess (List.map second model.guessedSequence) || List.member guess model.failedSequence
        correctlyGuessed = indices (fromChar guess) (model.hiddenWord)
        occurrances = List.repeat (List.length correctlyGuessed) guess 
        locations = List.map2 (,) correctlyGuessed occurrances 
        newFailedSeq = if List.isEmpty correctlyGuessed && not letterHasAlreadyBeenUsed then guess :: model.failedSequence else model.failedSequence
        -- TODo: Append only if not alread used
        newGuessedSeq = List.append model.guessedSequence locations
        newGameState = 
            case ((List.length newGuessedSeq == String.length model.hiddenWord), maxTries - List.length newFailedSeq) of 
                (False, 0 ) -> Lost
                (True, _) -> Won
                (_, _) -> Playing
    in 
       { model | guessedSequence = newGuessedSeq
               , gameState=newGameState
               , failedSequence = newFailedSeq}


-- HTTP
getRandomWord : Cmd Msg
getRandomWord =
  let
    url =
      "https://randomuser.me/api/"
  in
    Http.send NewWord (Http.get url decodeRandomWord)

decodeRandomWord : Decode.Decoder String
decodeRandomWord =
  --Decode.string
  Decode.at ["results"] (Decode.index 0 (Decode.at ["name","first"] Decode.string))

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

---------------------- HELPERS ----------------------

stringHead : String -> Char
stringHead str =  withDefault ' ' <| Maybe.map first (uncons <| (slice 0 1) str) 

lookup : eq -> List (eq, a) -> Maybe a 
lookup e l = List.filter (\x -> e == first x) l |> List.head |> Maybe.map second

showGuessSequence : List (Int, Char) -> String -> String
showGuessSequence l w = 
    let 
        sortedSeq =  List.sortBy first l
        hiddenWordLength = String.length w
    in 
        List.map (\x -> lookup x l |> withDefault '_') (range 0 (hiddenWordLength - 1)) |> String.fromList


stringToList : String -> List Char
stringToList str = []


---------------------- CONSTANTS -----------------------
maxTries = 10 

---------------- STYLESHEETS --------------
stylesheet =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      "custom.css"
            ]
        children = []
    in 
        node tag attrs children
