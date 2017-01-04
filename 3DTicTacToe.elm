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

--type alias Move = {board : Int, column : Int, row : Int}
type alias Move = Tictactoe.Positioned {board : Int}

type alias Model = { playerTurn:Player
                   , playerMoves : List (Move, Player)
                   , didWin : Maybe Player
                   }

type Msg = Restart | Play Move | Player Player | Won Player

main = program {init=init, view=view, update=update, subscriptions= subscriptions}


defaultModel = {playerTurn = PlayerX, playerMoves=[], didWin = Nothing}

init = (defaultModel, Cmd.none)

view : Model -> Html Msg
view model = 
    case model.didWin of
        Just p ->  div [class "tictactoe cubic"][
                         stylesheet "custom.css"
                       , h2 [] [text <| (toString p) ++ " wins"]
                       , button [onClick Restart][text "Restart"]
                       ]
        Nothing -> 
            if List.length model.playerMoves /= (3 * boardLength ^ 2) then 
                div [class "tictactoe cubic"][
                      stylesheet "custom.css"
                    , h1 [] [text "Tictactoe"]
                    , svg [ Svg.Attributes.width (toString boardSize)
                          , Svg.Attributes.height (toString boardSize)] (Tictactoe.renderBoard allPositions (getMovesForBoard 0 model.playerMoves) (addBoardToMove 0 >> Play))
                    , svg [ Svg.Attributes.width (toString boardSize)
                          , Svg.Attributes.height (toString boardSize)] (Tictactoe.renderBoard allPositions (getMovesForBoard 1 model.playerMoves) (addBoardToMove 1 >> Play))
                    , svg [ Svg.Attributes.width (toString boardSize)
                          , Svg.Attributes.height (toString boardSize)] (Tictactoe.renderBoard allPositions (getMovesForBoard 2 model.playerMoves) (addBoardToMove 2 >> Play))
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


--getMovesForBoard : Int -> Model -> List (Tictactoe.Move, Player)
--getMovesForBoard : Int -> Model -> List (Move, Player)
getMovesForBoard : Int -> List (Move, Player) -> List (Tictactoe.Move, Player)
getMovesForBoard n playerMoves  = 
    let
       movesOnBoard =  List.filter (\(m,p) -> .board m == n) (playerMoves)
    in 
        List.map (\(mv, p) -> ({column = mv.column, row = mv.row }, p)) movesOnBoard
        --movesOnBoard

hasWon : List (Move,Player) -> Maybe Player
hasWon model = 
    let 
        movesForPlayer x = List.filter (\(m,p) -> p == x) model
        movesForPlayeR x = List.filterMap (\(m,p) -> if p == x then Just m else Nothing) model

        stackedAt p k = 
                   List.filter (\(m,p) -> m.column == k.column && m.row == k.row) (movesForPlayer p) 
                |> List.length |> (\x -> boardLength == x) 
                |> (\y -> if y then Just p else Nothing)

        join : Maybe (Maybe a) -> Maybe a
        join m = maybe Nothing (\x -> x) m

        toMaybe b v = if b then Just v else Nothing

        anyStack p = List.map (stackedAt p) allPositions |> List.filter (\x -> x /= Nothing) |> List.head |> join
        --anyDiagonal l p = List.filter (flip List.member l) (movesForPlayer p) |> (\x -> toMaybe (List.length x == boardLength) p)
        --anyDiagonal p = List.map (containsAll (movesForPlayeR p)) generateDiags |> any (\x -> x == True) |> (\x -> if x then Just p else Nothing)
        anyDiagonal p =  containsAll (movesForPlayeR p) (generateDiags !! 0 ?? []) |> (\x -> if x then Just p else Nothing)

    in
        Tictactoe.hasWon (getMovesForBoard 0 model)
    <|> Tictactoe.hasWon (getMovesForBoard 1 model)
    <|> Tictactoe.hasWon (getMovesForBoard 2 model)
    <|> anyStack PlayerX
    <|> anyStack PlayerO
    <|> anyDiagonal PlayerO
    <|> anyDiagonal PlayerX



(<|>) m n = 
    case m of
        Just x -> (Just x)
        Nothing -> n
    


addBoardToMove : Int -> Tictactoe.Move -> Move
addBoardToMove b mv = {board = b, row = mv.row, column = mv.column}

type alias Dim = Int
type Seq = Fixed Int | Asc | Desc

generateDiags : List (List Move)
generateDiags = 
    let
        zip3 = List.map3 (\a b c -> (a,b,c))
        gs = genSeq 3
        seqs s1 s2 s3 = zip3 (gs s1) (gs s2) (gs s3) |> List.map (\(x,y,z) -> {column = x, row = y, board = z}) 
    in
        [ 

          [{column=0, row=0,board=0},{column=1, row=1,board=1},{column=2, row=2,board=2}]
        , seqs Asc Asc Asc
        , seqs Asc Asc Desc
        , seqs Desc Desc Asc
        , seqs Desc Desc Desc
        , seqs (Fixed 0) Asc Asc
        , seqs (Fixed 0) Desc Desc
        , seqs Asc (Fixed 0) Asc
        , seqs Desc (Fixed 0) Desc
        , seqs (Fixed 2) Asc Asc
        , seqs (Fixed 2) Desc Desc
        , seqs Asc (Fixed 2) Asc
        , seqs Desc (Fixed 2) Desc
        ]

-- | isSubsetOf l k returns True if all elements of k are in l
isSubsetOf : List a -> List a  -> Bool
isSubsetOf l k = l //= k |> List.isEmpty

-- | genSeq is a utility to generate constant ascending or descending sequences
genSeq : Dim -> Seq -> List Int
genSeq m s = 
    case s of 
        Fixed c -> List.repeat m c
        Asc -> List.range 0 (m - 1)
        Desc -> List.range 0 (m - 1) |> List.reverse

-------------- CONSTANTS --------------
boardLength = 3
boardSize = 300

allPositions = [lowerRight,lowerLeft, lowerMiddle, upperRight, upperLeft, upperMiddle, centerLeft, centerRight, centerMiddle]

lowerRight = {row = 2, column = 2}
lowerMiddle = {row = 2, column = 1}
lowerLeft = {row = 2, column = 0}
upperRight = {row = 0, column = 2}
upperLeft = {row = 0, column = 0}
upperMiddle = {row = 0, column = 1}
centerLeft = {row = 1, column = 0}
centerRight = {row = 1, column = 2}
centerMiddle = {row = 1, column = 1}

--allPositions = List.concatMap (\b -> [lowerRight b, lowerLeft b, lowerMiddle b, upperRight b, upperLeft b, upperMiddle b, centerLeft b, centerRight b, centerMiddle b]) [0,1,2]


lowerRightB b = {board = b, row = 2, column = 2}
lowerMiddleB b = {board = b, row = 2, column = 1}
lowerLeftB b = {board = b, row = 2, column = 0}
upperRightB b = {board = b, row = 0, column = 2}
upperLeftB b = {board = b, row = 0, column = 0}
upperMiddleB b = {board = b, row = 0, column = 1}
--centerLeft b = {board = b, row = 1, column = 0}
--centerRight b = {board = b, row = 1, column = 2}
centerMiddleB b = {board = b, row = 1, column = 1}

--------------- HELPERS ---------------

delete  : a -> List a -> List a
delete a l =
    let 
        safeTail k = List.tail k |> maybe [] (\x -> x)
    in
        case List.head l of
            Nothing -> []
            Just x -> if a == x then safeTail l else x :: delete a (safeTail l)

(//=) l k = List.foldl delete k l

(!!) xs n = head (drop n xs) 

-- | containsAll l k verifies if each element in k is present in l
containsAll : List a -> List a -> Bool
containsAll l k = List.all (\x -> x == True) <| List.map (flip List.member l) k

--all : (a -> Bool) -> List a -> Bool
--all p = List.foldr (\x acc -> p x && acc ) True

--any : (a -> Bool) -> List a -> Bool
--any p = List.foldr (\x acc -> p x || acc ) False
--(!?) a c = maybe a (\x -> c)

maybe : b -> (a -> b) -> Maybe a -> b
maybe d f m = 
    case m of
        Nothing -> d
        Just x -> f x

fromMaybe d = maybe d (\x -> x)

(??) = flip fromMaybe

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
