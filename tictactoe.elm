
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
import Svg.Attributes exposing (viewBox, rx, ry, x, y, x1, x2, y1, y2, cx, cy, r,stroke,strokeWidth, fill,points)
import Mouse exposing (..)

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
                         h2 [] [text <| (toString p) ++ " wins"]
                       , button [onClick Restart][text "Restart"]
            
                       ]
        Nothing -> 
            if List.length model.playerMoves /= (boardLength ^ 2) then 
                div [class "tictactoe"] [renderBoard model]
            else 

                div [class "tictactoe"][
                      h2 [] [text "Tie"]
                    , button [onClick Restart][text "Restart"]
        
                    ]



renderBoard : Model -> Html Msg
renderBoard model = 
    let 
        pieceTop r = (columWidth * r + inset)
        pieceLeft c = (columWidth * c + inset)
        pieceRight c = (columWidth * c + inset) + pieceSize
        pieceBottom r = (columWidth * r + inset) + pieceSize

        pieceTopLeft c r = (pieceLeft c, pieceTop r)
        pieceTopRight c r = (pieceRight c,pieceTop r)
        pieceBottomRight c r = (pieceRight c, pieceBottom r)
        pieceBottomLeft c r = (pieceLeft c, pieceBottom r)

        pointss l = points <| String.concat (List.map (\(x,y) -> toString x ++ "," ++ toString y ++ " ") l)
        pieceCenter c r = (columWidth * c + (columWidth / 2), columWidth * r + (columWidth/2))
        pieceCenterX c r = pieceCenter c r |> first
        pieceCenterY c r = pieceCenter c r |> second
        board  = 
            g [fill "none", stroke "black"]
              [
                line [x1 <| toString 0, y1 <| toString columWidth,  x2 <| toString boardSize, y2 <| toString columWidth][]
              , line [x1 <| toString 0, y1 <| toString (columWidth * 2),  x2 <| toString boardSize, y2 <| toString (columWidth * 2)][]
              , line [y1 <| toString 0, x1 <| toString columWidth,  y2 <| toString boardSize, x2 <| toString columWidth][]
              , line [y1 <| toString 0, x1 <| toString (columWidth * 2),  y2 <| toString boardSize, x2 <| toString (columWidth * 2)][]
              ]
        
        cross c r = g [fill "none", stroke "#00f"] 
                      [ line [x1 <| toString (pieceLeft c), y1 <| toString (pieceTop r),  x2 <| toString (pieceRight c), y2 <| toString (pieceBottom r)][]
                      , line [x1 <| toString (pieceLeft c), y1 <| toString (pieceBottom r),  x2 <| toString (pieceRight c), y2 <| toString (pieceTop r)][]
                      ]

        circ c row = circle [cx ( pieceCenterX c row |> toString), cy (pieceCenterY c row |> toString), r (toString <| pieceSize * 0.5), fill "#f00", stroke "2"] []

        drawPieces l =
            List.map (\(position, p)  -> if p == PlayerX then cross (Basics.toFloat position.column) (Basics.toFloat position.row)
                                         else circ (Basics.toFloat position.column)  (Basics.toFloat position.row)) l
                                        
            
    in
        svg [ Svg.Attributes.width (toString boardSize), Svg.Attributes.height (toString boardSize)] (board :: (drawPieces model.playerMoves))

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

    in ups handler


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
        




-- CONSTANTS
pieceWidth = 25
pieceHeight = 25

inset = (columWidth - pieceSize) * 0.5

columWidth = boardSize/3
pieceSize = columWidth * 0.7
boardSize = 300
boardLength = 3


boardCenter = {row = 1, column = 1}
lowerRight = {row = 2, column = 2}
lowerMiddle = {row = 2, column = 1}
upperRight = {row = 0, column = 2}
upperLeft = {row = 0, column = 0}
lowerLeft = {row = 2, column = 0}
