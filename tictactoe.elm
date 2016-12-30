module Tictactoe exposing (Model, Move, Msg(..), Player(..) , view, update, defaultModel, renderBoard, renderBox)
import Html exposing (beginnerProgram, program, div, button, text)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import String exposing (..)
import Tuple exposing (..)
import List exposing (..)
import Svg exposing (svg, rect, circle, line, polyline, g, Svg)
import Svg.Attributes 
import Svg.Events 
import Svg.Attributes exposing (viewBox, rx, ry, x, y, x1, x2, y1, y2, cx, cy, r,stroke,strokeWidth, fill,points)
--import Mouse exposing (..)

type alias Positioned a =
      { a | column : Int, row : Int }
      
type alias Move = { column : Int, row : Int}

type alias Model = { playerTurn:Player
                   , playerMoves : List (Move, Player)
                   , didWin : Maybe Player
                   }

type Player = PlayerX | PlayerO

type Msg = Restart | Play Move | Player Player | Won Player


main = program {init=init, view=view, update=update, subscriptions= subscriptions}

defaultModel = {playerTurn = PlayerX, playerMoves=[], didWin = Nothing}

dummyModel = {playerTurn = PlayerX, playerMoves=[  (boardCenter, PlayerX) 
                                                   , (lowerLeft, PlayerO) 
                                                   , (lowerRight, PlayerX)
                                                   , (upperRight, PlayerX)
                                                   , (upperLeft, PlayerO)
                                                   , (lowerMiddle, PlayerX)]}
init = (defaultModel, Cmd.none)

-- VIEW
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
                    , renderGame model
                    ]
            else 

                div [class "tictactoe"][
                     stylesheet "custom.css"
                    , h2 [] [text "Tie"]
                    , button [onClick Restart][text "Restart"]
        
                    ]
-- | Can render an empty box or a box with a player figure in it
--renderBox : Move -> Maybe Player -> Svg Msg
renderBox : (Positioned a -> msg) -> Positioned a -> Maybe Player ->  Svg msg
renderBox tag position m =  
    let 
        widthFactor = 0.87
        --figureInset = (1 - widthFactor) * 0.5 * columWidth |> toString
        figureInset = "14"
        borderInset = "5"
        playerFigure player = 
            if player == PlayerO
            then 
                circle [columWidth * 0.5 |> toString |> cx, columWidth * 0.5 |> toString |> cy, columWidth * 0.35 |> toString |> r][]
            else 
                g [fill "none", stroke "#00f", strokeWidth "5"] 
                  [ line [x1 figureInset, y1 figureInset,  x2 <| toString (columWidth * widthFactor), y2 <| toString (columWidth * widthFactor)][]
                  , line [x1 <| toString (columWidth * widthFactor), y1 figureInset,  x2 figureInset, y2 <| toString (columWidth * widthFactor)][]
                  ]

        pixelsForColumn = toString (floor columWidth * .column position)
        pixelsForRow = toString (floor columWidth * .row position)

        boxWith html = 
            g [ "transform: translate(" ++ pixelsForColumn ++ "px, " ++ pixelsForRow ++ "px);" |> Svg.Attributes.style 
              --, Svg.Events.onClick (Play position)
              , Svg.Events.onClick (tag position)
              , Svg.Attributes.class "move-box"
              ] 
              [ line [stroke "#000", strokeWidth "5", toString 0 |> x1, toString 0 |> y1, toString 0 |> x2, toString columWidth |> y2][]
              , line [stroke "#000", strokeWidth "5", toString 0 |> x1, toString 0 |> y1, toString columWidth |> x2, toString 0 |> y2][]
              , line [stroke "#000", strokeWidth "5", toString columWidth |> x1, toString 0 |> y1, toString columWidth |> x2, toString columWidth |> y2][]
              , line [stroke "#000", strokeWidth "5", toString 0 |> x1, toString columWidth |> y1, toString columWidth |> x2, toString columWidth |> y2][]
              , html
              ]
    in
        case m of
            Just player -> boxWith <| playerFigure player
            Nothing -> boxWith <| rect [x borderInset, y borderInset, Svg.Attributes.width (toString <| columWidth * 0.9)
                                       , Svg.Attributes.height (toString <| columWidth * 0.9), fill "#eee"] []


renderBoard : List (Positioned a, Player) -> (Positioned a -> msg) -> List (Svg msg)
renderBoard playerMoves clicktag = List.map (uncurry <| renderBox clicktag) (boardMoves playerMoves)

renderGame : Model -> Html Msg
renderGame model = 
        svg [ Svg.Attributes.width (toString boardSize), Svg.Attributes.height (toString boardSize)] (renderBoard  model.playerMoves Play)


-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg mod = 
    let
        stepPlayerTurn p = if p == PlayerX then PlayerO else PlayerX
        newPlayer = stepPlayerTurn mod.playerTurn

    in case msg of 
        Play m  -> ({mod | playerMoves = (m, newPlayer) :: mod.playerMoves, playerTurn = newPlayer, didWin = hasWon <| (m,newPlayer) :: mod.playerMoves}, Cmd.none)
        Restart -> (defaultModel, Cmd.none)
        _ -> (mod, Cmd.none)


-- SUBSCRIPTIONS
subscriptions m = 
    let 

        handler p = 
            let 
                pos = {column = p.x // (floor columWidth), row = p.y // (floor columWidth) }
                hasBeenPlayed = List.member pos (List.map first m.playerMoves)
            in 
                if not hasBeenPlayed then Play pos else Player m.playerTurn

    in 
        --ups handler
        Sub.none


--HELPERS

hasWon : List (Move,Player) -> Maybe Player
hasWon moveList = 
    let
        playerMoves p = List.map (\(m,x) -> (m.row, m.column)) (List.filter (\(m,x) -> x == p) moveList)
        playerXMoves = playerMoves PlayerX
        playerOMoves = playerMoves PlayerO
        diagonals1 = [(0,0),(1,1),(2,2)]
        diagonals2 = [(0,2),(1,1),(2,0)]
        column c = List.map2 (,) (List.range 0 2) (List.repeat 3 c)
        row r = List.map2 (,) (List.repeat 3 r) (List.range 0 2) 
        containsSeq s l = List.length (List.filter (flip List.member s) l) == (List.length s)

        playerWins l = containsSeq diagonals2 l || containsSeq diagonals1 l || containsSeq (row 0) l || containsSeq (column 0) l || 
                       containsSeq (row 1) l || containsSeq (column 1) l || containsSeq (row 2) l || containsSeq (column 2) l
    in 
        case (playerWins playerXMoves, playerWins playerOMoves) of
            (True, _) -> Just PlayerX
            (_ , True) -> Just PlayerO
            (_,_) -> Nothing
        




boardMoves : List (Positioned a,Player) -> List (Positioned a, Maybe Player)
boardMoves playedMoves = 
    let
        lookup e l = List.filter (\x -> e == first x) l |> List.head |> Maybe.map second
        allPositions = [lowerRight,lowerLeft, lowerMiddle, upperRight, upperLeft, upperMiddle, centerLeft, centerRight, centerMiddle]
        --allPositions = [lowerRight]
    in
        List.map (\p -> flip lookup playedMoves p |> (\x -> (p,x))) allPositions





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

-- CONSTANTS
pieceWidth = 25
pieceHeight = 25

inset = (columWidth - pieceSize) * 0.5

columWidth = boardSize/3
pieceSize = columWidth * 0.7
boardSize = 300
boardLength = 3


boardCenter = {row = 1, column = 1}

--lowerRight a = {a | row = 2, column = 2}
lowerRight = {row = 2, column = 2}
lowerMiddle = {row = 2, column = 1}
lowerLeft = {row = 2, column = 0}
upperRight = {row = 0, column = 2}
upperLeft = {row = 0, column = 0}
upperMiddle = {row = 0, column = 1}
centerLeft = {row = 1, column = 0}
centerRight = {row = 1, column = 2}
centerMiddle = {row = 1, column = 1}
