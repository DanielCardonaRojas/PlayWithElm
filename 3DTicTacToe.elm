import Html exposing (beginnerProgram, program, div, button, text)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import String exposing (..)
import Tuple exposing (..)
import List exposing (..)
import Svg exposing (svg, rect, circle, line, polyline, g)
import Svg.Attributes 
import Svg.Events 
import Svg.Attributes exposing (viewBox, rx, ry, x, y, x1, x2, y1, y2, cx, cy, r,stroke,strokeWidth, fill,points)

import Tictactoe
import Tictactoe exposing (Player(..))

type alias Move = {board : Int, column : Int, row : Int}

type alias Model = { playerTurn:Tictactoe.Player
                   , playerMoves : List (Move, Player)
                   , didWin : Maybe Tictactoe.Player
                   }

type Msg = Restart | Play Move | Player Player | Won Player

main = program {init=init, view=view, update=update, subscriptions= subscriptions}


defaultModel = {playerTurn = PlayerX, playerMoves=[], didWin = Nothing}

init = (defaultModel, Cmd.none)

view : Model -> Html Msg
view model = 
    case model.didWin of
        Just p ->  div [class "tictactoe"][
                         stylesheet "custom.css"
                       , h2 [] [text <| (toString p) ++ " wins"]
                       , button [onClick Restart][text "Restart"]
                       ]
        Nothing -> 
            if List.length model.playerMoves /= (boardLength ^ 2) then 
                div [class "tictactoe"] [
                      stylesheet "custom.css"
                    , h1 [] [text "Tictactoe"]
                    , svg [ Svg.Attributes.width (toString boardSize)
                          , Svg.Attributes.height (toString boardSize)] (Tictactoe.renderBoard (getMovesForBoard 0 model))
                    , svg [ Svg.Attributes.width (toString boardSize)
                          , Svg.Attributes.height (toString boardSize)] (Tictactoe.renderBoard (getMovesForBoard 1 model))
                    , svg [ Svg.Attributes.width (toString boardSize)
                          , Svg.Attributes.height (toString boardSize)] (Tictactoe.renderBoard (getMovesForBoard 2 model))
                    ]
            else 

                div [class "tictactoe"][
                     stylesheet "custom.css"
                    , h2 [] [text "Tie"]
                    , button [onClick Restart][text "Restart"]
                    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg mod = 
    let
        stepPlayerTurn p = if p == PlayerX then PlayerO else PlayerX
        newPlayer = stepPlayerTurn mod.playerTurn

    in case msg of 
        Play m  -> ({mod | playerMoves = (m, newPlayer) :: mod.playerMoves, playerTurn = newPlayer, didWin = hasWon <| (m,newPlayer) :: mod.playerMoves}, Cmd.none)
        Restart -> (defaultModel, Cmd.none)
        _ -> (mod, Cmd.none)

subscriptions m = Sub.none

getMovesForBoard : Int -> Model -> List (Tictactoe.Move, Player)
getMovesForBoard n model  = 
    let
       movesOnBoard =  List.filter (\(m,p) -> .board m == n) (.playerMoves model)
    in List.map (\(mv, p) -> ({column = mv.column, row = mv.row }, p)) movesOnBoard

hasWon : List (Move,Player) -> Maybe Player
hasWon ls = Nothing

-------------- CONSTANTS --------------
boardLength = 3
boardSize = 300

--------------- HELPERS ---------------
stylesheet name =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      name
            ]
        children = []
    in 
        node tag attrs children
