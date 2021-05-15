module Chess.Logic exposing
    ( CastlingRight(..)
    , Game
    , Model
    , MoveKey
    , Msg
    , Piece(..)
    , PieceType(..)
    , Square(..)
    , Team(..)
    , addPiece
    , blankBoard
    , canAttack
    , canMoveTo
    , castlingMove
    , findChecks
    , findVectors
    , fromFen
    , init
    , init2
    , isCheckmate
    , makeMove
    , nextTurn
    , popPiece
    , removePiece
    , view
    )

--import Svg.Attributes exposing (style)
--import Page.Learn.Scenario exposing (BasicMove(..), Fen(..), Move, MoveCommand, PreviousMovesSafe(..))

import Chess.Position as Position exposing (Position(..))
import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes exposing (class, classList)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as D
import Json.Encode as E
import Page.Learn.Scenario exposing (BasicMove(..), Fen(..), Move, PreviousMovesSafe)
import Responsive exposing (Responsive)
import Set exposing (Set)
import Svg exposing (Svg, circle, g, path, svg)
import Svg.Attributes as SvgAttr exposing (cx, cy, d, height, id, r, style, transform, version, viewBox, width, x, y, z)


type Turn
    = Turn Team


type Team
    = Black
    | White


type PieceType
    = Monarch
    | Advisor
    | Rook
    | Bishop
    | Knight
    | Pawn


type Piece
    = Piece Team PieceType


type Square
    = Occupied Position Piece


type Model
    = Model AnimatedGame


type AnimatedGame
    = Animating { before : Game, after : Game }
    | DoneAnimating Game


type Historical
    = Historical (List ( BasicMove, Fen ))


type alias Game =
    -- TODO: Extract OccupiedSquares into new location (so that data structure used is opaque).
    -- Provide appropriate accessors / map functions.
    { occupiedSquares : Dict ( Int, Int ) Piece
    , turn : Team
    , enpassant : Maybe Position
    , castlingRights : List CastlingRight
    , moves : Dict MoveKey Move
    , movesByPosition : Dict PositionKey (List Move)
    , rawFen : String
    }


type CastlingRight
    = MonarchSide Team
    | AdvisorSide Team



-- Drag and drop pieces


type DragStuff
    = PieceInHand DragStuffInner
    | NoPieceInHand


type alias DragStuffInner =
    { x : Int
    , y : Int
    , piece : Piece
    , startingPosition : Position
    , dragState : DragState
    , currentPosition : Position
    }


type DragState
    = Moving Int Int Int Int


type Msg
    = Msg


{-| Make a move on the board.

NOTE: This function does NOT apply any restrictive game logic. It assumes that whatever is being sent is valid.

To ensure that you do not end up in a bad state you should first call: `canMoveTo` to find out validMoves and only then
call this function to execute one of the moves returned.

-}
makeMove : ( Int, Int ) -> ( Int, Int ) -> Game -> Game
makeMove squareFrom squareTo ({ occupiedSquares, turn, enpassant, castlingRights } as game) =
    case Dict.get squareFrom occupiedSquares of
        Nothing ->
            game

        Just ((Piece team Pawn) as piece) ->
            case enpassant of
                Nothing ->
                    Dict.remove squareFrom occupiedSquares
                        |> Dict.insert squareTo piece
                        |> (\updatedPieces -> { game | occupiedSquares = updatedPieces, turn = opponentTurn turn, castlingRights = castlingRightsOnCapture squareTo turn castlingRights })

                Just (Position col row) ->
                    if ( col, row ) == squareTo then
                        case findPawnThatMadeEnpassantMove game of
                            Nothing ->
                                -- This should be impossible.
                                Dict.remove squareFrom occupiedSquares
                                    |> Dict.insert squareTo piece
                                    |> (\updatedPieces -> { game | occupiedSquares = updatedPieces, turn = opponentTurn turn, castlingRights = castlingRightsOnCapture squareTo turn castlingRights })

                            Just (Position enpassantColumn enpassantRow) ->
                                Dict.remove squareFrom occupiedSquares
                                    |> Dict.remove ( enpassantColumn, enpassantRow )
                                    |> Dict.insert squareTo piece
                                    |> (\updatedPieces -> { game | occupiedSquares = updatedPieces, turn = opponentTurn turn })

                    else
                        Dict.remove squareFrom occupiedSquares
                            |> Dict.insert squareTo piece
                            |> (\updatedPieces -> { game | occupiedSquares = updatedPieces, turn = opponentTurn turn, castlingRights = castlingRightsOnCapture squareTo turn castlingRights })

        Just ((Piece team Monarch) as piece) ->
            let
                updatedPieces =
                    case castlingMove squareFrom squareTo of
                        Just rookMove ->
                            makeCastlingMove squareFrom squareTo occupiedSquares piece team turn game rookMove

                        Nothing ->
                            Dict.remove squareFrom occupiedSquares
                                |> Dict.insert squareTo piece
            in
            { game | occupiedSquares = updatedPieces, turn = opponentTurn turn, castlingRights = castlingRightsOnCapture squareTo turn (clearCastlingRights turn game.castlingRights) }

        Just ((Piece team Rook) as piece) ->
            let
                updatedCastlingRights =
                    case ( squareFrom, team ) of
                        ( ( 8, 8 ), Black ) ->
                            List.filter ((==) (MonarchSide Black)) castlingRights

                        ( ( 1, 8 ), Black ) ->
                            List.filter ((==) (AdvisorSide Black)) castlingRights

                        ( ( 8, 1 ), White ) ->
                            List.filter ((==) (MonarchSide White)) castlingRights

                        ( ( 1, 1 ), White ) ->
                            List.filter ((==) (AdvisorSide White)) castlingRights

                        ( _, _ ) ->
                            castlingRights
            in
            Dict.remove squareFrom occupiedSquares
                |> Dict.insert squareTo piece
                |> (\updatedPieces -> { game | occupiedSquares = updatedPieces, turn = opponentTurn turn, castlingRights = castlingRightsOnCapture squareTo turn updatedCastlingRights })

        Just piece ->
            Dict.remove squareFrom occupiedSquares
                |> Dict.insert squareTo piece
                |> (\updatedPieces -> { game | occupiedSquares = updatedPieces, turn = opponentTurn turn, castlingRights = castlingRightsOnCapture squareTo turn castlingRights })


castlingMove squareFrom squareTo =
    case ( squareFrom, squareTo ) of
        -- NOTE: Elm doesn't appear to allow us to pattern match on our nice Position.e8 style types :(
        -- Monarch starting square to castling square
        -- e8       g8
        ( ( 5, 8 ), ( 7, 8 ) ) ->
            Just { rookStarting = Position 8 8, rookEnding = Position 6 8 }

        -- e8       c8
        ( ( 5, 8 ), ( 3, 8 ) ) ->
            Just { rookStarting = Position 1 8, rookEnding = Position 4 8 }

        -- e1       c1
        ( ( 5, 1 ), ( 3, 1 ) ) ->
            Just { rookStarting = Position 1 1, rookEnding = Position 4 1 }

        -- e1       g1
        ( ( 5, 1 ), ( 7, 1 ) ) ->
            Just { rookStarting = Position 8 1, rookEnding = Position 6 1 }

        ( _, _ ) ->
            Nothing


removePiece square ({ occupiedSquares } as game) =
    Dict.remove (Position.toRaw square) occupiedSquares
        |> (\updatedSquares -> { game | occupiedSquares = updatedSquares })


popPiece square ({ occupiedSquares } as game) =
    let
        piece =
            Dict.get (Position.toRaw square) occupiedSquares
    in
    Dict.remove (Position.toRaw square) occupiedSquares
        |> (\updatedSquares -> { game | occupiedSquares = updatedSquares })
        |> Tuple.pair piece


addPiece piece square ({ occupiedSquares } as game) =
    Dict.insert (Position.toRaw square) piece occupiedSquares
        |> (\updatedSquares -> { game | occupiedSquares = updatedSquares })


findVectors position position2 team =
    let
        ( columnDelta, rowDelta ) =
            Position.compare position position2
    in
    case team of
        Black ->
            ( negate columnDelta, rowDelta )

        White ->
            ( columnDelta, negate rowDelta )


castlingRightsOnCapture squareTo team castlingRights =
    case ( squareTo, opponentTurn team ) of
        ( ( 8, 8 ), Black ) ->
            List.filter ((==) (MonarchSide Black)) castlingRights

        ( ( 1, 8 ), Black ) ->
            List.filter ((==) (AdvisorSide Black)) castlingRights

        ( ( 8, 1 ), White ) ->
            List.filter ((==) (MonarchSide White)) castlingRights

        ( ( 1, 1 ), White ) ->
            List.filter ((==) (AdvisorSide White)) castlingRights

        ( _, _ ) ->
            castlingRights


makeCastlingMove squareFrom squareTo occupiedSquares piece team turn game { rookStarting, rookEnding } =
    Dict.remove squareFrom occupiedSquares
        |> Dict.insert squareTo piece
        |> Dict.remove (Position.toRaw rookStarting)
        |> Dict.insert (Position.toRaw rookEnding) (Piece team Rook)


clearCastlingRights : Team -> List CastlingRight -> List CastlingRight
clearCastlingRights team castlingRights =
    List.filter
        (\castlingRight ->
            case castlingRight of
                AdvisorSide castlingRightTeam ->
                    castlingRightTeam == team

                MonarchSide castlingRightTeam ->
                    castlingRightTeam == team
        )
        castlingRights


findPawnThatMadeEnpassantMove : Game -> Maybe Position
findPawnThatMadeEnpassantMove { enpassant, turn } =
    Maybe.map
        (\(Position column row) ->
            case turn of
                Black ->
                    Position column (row + 1)

                White ->
                    Position column (row - 1)
        )
        enpassant


positionToSquareKey : Position -> ( Int, Int )
positionToSquareKey (Position column row) =
    ( column, row )


asht (Occupied position piece) b =
    Dict.insert (positionToSquareKey position) piece b


init2 : Fen -> Result String Model
init2 (Fen raw) =
    fromFen [] raw
        |> Result.map (Model << DoneAnimating)



--Game (List.foldl asht Dict.empty squares) turn enpassant castlingRights Dict.empty Dict.empty ""
-- NOTE: MetaGame isn't using this (instead it is using the fromFen function below).


init : List Square -> Team -> Maybe Position -> List CastlingRight -> Game
init squares turn enpassant castlingRights =
    Game (List.foldl asht Dict.empty squares) turn enpassant castlingRights Dict.empty Dict.empty ""


blankBoard : Game
blankBoard =
    Game Dict.empty Black Nothing [] Dict.empty Dict.empty ""



-- FORCING MOVES
-- CHECKMATE


isCheckmate : Game -> Bool
isCheckmate ({ occupiedSquares, turn } as game) =
    case findChecks game of
        [] ->
            False

        checks ->
            let
                occupiedAsList =
                    Dict.toList occupiedSquares

                -- TODO: pass in monarchs so that we know we have them.
                ( monarchLocation, monarch ) =
                    List.filter (sameTeam turn) occupiedAsList
                        |> List.filter isMonarch
                        |> List.head
                        |> Maybe.withDefault ( ( 1, 1 ), Piece turn Monarch )

                monarchVectors =
                    possibleMonarchVectors monarchLocation

                monarchIsAbleToEscape =
                    List.filter (\monarchMoveTo -> canMoveToSingle game monarchMoveTo occupiedSquares monarchLocation turn (horizontalMovement ++ diagonalMovement)) monarchVectors
                        |> List.isEmpty
                        |> not
            in
            monarchIsAbleToEscape
                || checksAreBlockable monarchLocation game checks
                || checksAreCapturable game checks
                |> not



-- TODO: type Checks = SingleCheck | DoubleCheck?


checksAreCapturable : Game -> List Square -> Bool
checksAreCapturable game checks =
    case checks of
        check :: [] ->
            checkIsCapturable game check

        checkA :: checkB :: _ ->
            -- Cannot escape double check by capturing!
            False

        _ ->
            -- No checks means no problems.
            True


checkIsCapturable : Game -> Square -> Bool
checkIsCapturable game (Occupied position piece) =
    canMoveTo position game
        |> List.isEmpty
        |> not


checksAreBlockable : ( Int, Int ) -> Game -> List Square -> Bool
checksAreBlockable monarchSquare game checks =
    case checks of
        -- NOTE: in Chess the maximum number of checks is 2.
        -- In Chinese Xiang Xi it is possible to have 3 or even 4 checks.
        check :: remainingChecks ->
            List.map (checkBlockable monarchSquare game) remainingChecks
                -- NOTE: This is certainly more complex than it needs to be. Future you, I owe you a beer.
                |> List.foldl Set.intersect (checkBlockable monarchSquare game check)
                |> Set.isEmpty
                |> not

        _ ->
            -- No checks means no problems.
            True


checkBlockable : ( Int, Int ) -> Game -> Square -> Set ( Int, Int )
checkBlockable ( monarchColumn, monarchRow ) ({ occupiedSquares } as game) (Occupied (Position checkingColumn checkingRow) checkingPiece) =
    pieceMovementToPath (Position monarchColumn monarchRow) occupiedSquares ( checkingColumn, checkingRow )
        |> List.map (\( routeSquareColumn, routeSquareRow ) -> canMoveTo (Position routeSquareColumn routeSquareRow) game)
        |> List.map (List.map (\(Position c r) -> ( c, r )))
        |> List.map Set.fromList
        |> List.foldl Set.union Set.empty


possibleMonarchVectors : ( Int, Int ) -> List Position
possibleMonarchVectors ( px, py ) =
    List.map (\( dx, dy ) -> ( dx + px, dy + py )) (horizontalMovement ++ diagonalMovement)
        |> List.filter (\( newColumn, newRow ) -> newColumn >= 1 && newColumn <= 8 && newRow >= 1 && newRow <= 8)
        |> List.map (\( a, b ) -> Position a b)



-- CHECK


findChecks : Game -> List Square
findChecks ({ occupiedSquares, turn } as game) =
    let
        occupiedAsList =
            Dict.toList occupiedSquares

        ( monarchLocation, monarch ) =
            List.filter (sameTeam turn) occupiedAsList
                |> List.filter isMonarch
                |> List.head
                |> Maybe.withDefault ( ( 1, 1 ), Piece turn Monarch )

        enemyTeam =
            List.filter (opponentTeam turn) occupiedAsList
                |> List.filter (isMonarch >> not)

        fromTuple ( a, b ) fn =
            fn a b
    in
    List.filter (\( pieceLocation, p ) -> pieceCanMoveTo { game | turn = opponentTurn turn } (fromTuple monarchLocation Position) occupiedSquares (opponentTurn turn) pieceLocation) enemyTeam
        |> List.map (\( pos, piece ) -> Occupied (fromTuple pos Position) piece)


nextTurn : Game -> Game
nextTurn g =
    { g | turn = opponentTurn g.turn }


opponentTurn : Team -> Team
opponentTurn turn =
    case turn of
        Black ->
            White

        White ->
            Black


isMonarch : ( a, Piece ) -> Bool
isMonarch ( _, Piece _ pieceType ) =
    case pieceType of
        Monarch ->
            True

        _ ->
            False


sameTeam : Team -> ( a, Piece ) -> Bool
sameTeam turn ( _, Piece pieceColor _ ) =
    pieceColor == turn


opponentTeam : Team -> ( a, Piece ) -> Bool
opponentTeam turn ( _, Piece pieceColor _ ) =
    pieceColor /= turn



-- MOVEMENT


pieceMovementToPath : Position -> Dict ( Int, Int ) Piece -> ( Int, Int ) -> List ( Int, Int )
pieceMovementToPath moveTo occupiedSquares occupied =
    case Dict.get occupied occupiedSquares of
        Nothing ->
            []

        Just (Piece _ Monarch) ->
            []

        Just (Piece team Advisor) ->
            findPath moveTo occupiedSquares occupied team [] (horizontalMovement ++ diagonalMovement)

        Just (Piece team Bishop) ->
            findPath moveTo occupiedSquares occupied team [] diagonalMovement

        Just (Piece team Rook) ->
            findPath moveTo occupiedSquares occupied team [] horizontalMovement

        Just (Piece team Knight) ->
            []

        Just (Piece team Pawn) ->
            []


pieceCanMoveTo : Game -> Position -> Dict ( Int, Int ) Piece -> Team -> ( Int, Int ) -> Bool
pieceCanMoveTo game ((Position squareToColumn squareToRow) as moveTo) occupiedSquares turn occupied =
    -- TODO: Should be a (Result MoveError Bool)
    -- type MoveError = NoPieceOnSquare | OpponentsPiece | ResultsInCheck
    case Dict.get occupied occupiedSquares of
        Nothing ->
            False

        Just piece ->
            case piece of
                Piece team Monarch ->
                    monarchCanMoveTo game moveTo occupiedSquares occupied team

                Piece team Advisor ->
                    canMoveToRepeating game moveTo occupiedSquares occupied team (horizontalMovement ++ diagonalMovement)

                Piece team Rook ->
                    canMoveToRepeating game moveTo occupiedSquares occupied team horizontalMovement

                Piece team Bishop ->
                    canMoveToRepeating game moveTo occupiedSquares occupied team diagonalMovement

                Piece team Knight ->
                    canMoveToSingle game moveTo occupiedSquares occupied team knightMovement

                Piece team Pawn ->
                    pawnCanMoveTo game moveTo occupiedSquares occupied team


doesNotLeadToCheck : ( Int, Int ) -> ( Int, Int ) -> Game -> Bool
doesNotLeadToCheck squareFrom squareTo game =
    makeMove squareFrom squareTo game
        |> (\virtualGame ->
                { virtualGame | turn = game.turn }
                    |> findChecks
                    |> List.isEmpty
           )


canAttack : Position -> Game -> List Position
canAttack moveTo ({ occupiedSquares, turn } as game) =
    let
        allOccupied =
            Dict.keys occupiedSquares
    in
    List.filter (pieceCanAttack game moveTo occupiedSquares turn) allOccupied
        |> List.map (\( x, y ) -> Position x y)


pieceCanAttack : Game -> Position -> Dict ( Int, Int ) Piece -> Team -> ( Int, Int ) -> Bool
pieceCanAttack game ((Position squareToColumn squareToRow) as moveTo) occupiedSquares turn occupied =
    case Dict.get occupied occupiedSquares of
        Nothing ->
            False

        Just piece ->
            case piece of
                Piece team Pawn ->
                    -- TODO: don't really like how this is structured.
                    pawnCanAttack game moveTo occupiedSquares occupied team

                _ ->
                    pieceCanMoveTo game moveTo occupiedSquares turn occupied


canMoveTo : Position -> Game -> List Position
canMoveTo moveTo ({ occupiedSquares, turn } as game) =
    let
        allOccupied =
            Dict.keys occupiedSquares
    in
    List.filter (pieceCanMoveTo game moveTo occupiedSquares turn) allOccupied
        |> List.map (\( x, y ) -> Position x y)


canMoveToSingle : Game -> Position -> Dict ( Int, Int ) Piece -> ( Int, Int ) -> Team -> List ( Int, Int ) -> Bool
canMoveToSingle ({ turn } as game) ((Position squareToColumn squareToRow) as moveTo) occupiedSquares occupied team vectors =
    turn
        == team
        && List.any (\vector -> checkMoveInDirection vector moveTo occupiedSquares occupied team) vectors
        && doesNotLeadToCheck occupied ( squareToColumn, squareToRow ) game


monarchCanMoveTo : Game -> Position -> Dict ( Int, Int ) Piece -> ( Int, Int ) -> Team -> Bool
monarchCanMoveTo ({ castlingRights } as game) moveTo occupiedSquares occupied team =
    let
        monarchInCheck =
            findChecks game
                |> (not << List.isEmpty)

        castlingMoves =
            if monarchInCheck then
                []

            else
                List.concatMap (findCastlingMove game moveTo occupiedSquares occupied team) castlingRights
    in
    canMoveToSingle game moveTo occupiedSquares occupied team (horizontalMovement ++ diagonalMovement ++ castlingMoves)


findCastlingMove : Game -> Position -> Dict ( Int, Int ) Piece -> ( Int, Int ) -> Team -> CastlingRight -> List ( Int, Int )
findCastlingMove game moveTo occupiedSquares occupied team castlingRight =
    -- NOTE: This implementation assumes that the monarch is in the typical starting location:
    -- e8 for black, and e1 for white.
    -- This implementation will NOT work if we ever choose to support Chess960.
    let
        pathIsNotBlocked path =
            List.filter (\squareTo -> Dict.member squareTo occupiedSquares) (List.map Position.toRaw path)
                |> List.isEmpty

        monarchDoesntMoveThroughCheck path =
            List.all (\squareTo -> doesNotLeadToCheck occupied squareTo game) (List.map Position.toRaw path)

        isValidCastlingRight castlingRightTeam path offset =
            if team == castlingRightTeam && pathIsNotBlocked path && monarchDoesntMoveThroughCheck path then
                [ ( offset, 0 ) ]

            else
                []
    in
    case castlingRight of
        AdvisorSide Black ->
            isValidCastlingRight Black [ Position.d8, Position.c8 ] -2

        AdvisorSide White ->
            isValidCastlingRight White [ Position.f1, Position.g1 ] -2

        MonarchSide Black ->
            isValidCastlingRight Black [ Position.f8, Position.g8 ] 2

        MonarchSide White ->
            isValidCastlingRight White [ Position.d1, Position.c1 ] 2


pawnCanAttack : Game -> Position -> Dict ( Int, Int ) Piece -> ( Int, Int ) -> Team -> Bool
pawnCanAttack ({ enpassant } as game) moveTo occupiedSquares occupied team =
    let
        possibleAttacks =
            case team of
                Black ->
                    -- Pawns attack diagonally.
                    [ ( 1, -1 ), ( -1, -1 ) ]

                White ->
                    [ ( 1, 1 ), ( -1, 1 ) ]

        possibleEnPassant =
            List.filter (mayUseEnpassant occupied enpassant) possibleAttacks
    in
    canMoveToSingle game moveTo occupiedSquares occupied team (possibleAttacks ++ possibleEnPassant)


pawnCanMoveTo : Game -> Position -> Dict ( Int, Int ) Piece -> ( Int, Int ) -> Team -> Bool
pawnCanMoveTo ({ enpassant } as game) moveTo occupiedSquares occupied team =
    let
        possibleMovements =
            case ( team, occupied ) of
                -- Pawns in starting locations may move forward two squares
                ( Black, ( _, 7 ) ) ->
                    [ ( 0, -1 ), ( 0, -2 ) ]

                ( White, ( _, 2 ) ) ->
                    [ ( 0, 1 ), ( 0, 2 ) ]

                ( Black, _ ) ->
                    [ ( 0, -1 ) ]

                ( White, _ ) ->
                    [ ( 0, 1 ) ]

        possibleAttacks =
            case team of
                Black ->
                    -- Pawns attack diagonally.
                    [ ( 1, -1 ), ( -1, -1 ) ]

                White ->
                    [ ( 1, 1 ), ( -1, 1 ) ]

        possibleMovementsForPawn =
            List.filter (possibleMoveForPawn occupiedSquares occupied) possibleMovements

        possibleAttacksForPawn =
            List.filter (possibleAttackForPawn occupiedSquares occupied) possibleAttacks

        possibleEnPassant =
            List.filter (mayUseEnpassant occupied enpassant) possibleAttacks
    in
    canMoveToSingle game moveTo occupiedSquares occupied team (possibleMovementsForPawn ++ possibleAttacksForPawn ++ possibleEnPassant)


mayUseEnpassant ( squareFromColumn, squareFromRow ) enpassant delta =
    Maybe.map2 (==) enpassant (Position.applyDelta (Position squareFromColumn squareFromRow) delta)
        |> Maybe.withDefault False


possibleAttackForPawn : Dict ( Int, Int ) Piece -> ( Int, Int ) -> ( Int, Int ) -> Bool
possibleAttackForPawn occupiedSquares ( squareFromColumn, squareFromRow ) delta =
    Position.applyDelta (Position squareFromColumn squareFromRow) delta
        |> Maybe.andThen (\(Position col row) -> Dict.get ( col, row ) occupiedSquares)
        |> isJust



-- Pawns may not attack forwards.


possibleMoveForPawn : Dict ( Int, Int ) Piece -> ( Int, Int ) -> ( Int, Int ) -> Bool
possibleMoveForPawn occupiedSquares ( squareFromColumn, squareFromRow ) delta =
    Position.applyDelta (Position squareFromColumn squareFromRow) delta
        |> Maybe.andThen (\(Position col row) -> Dict.get ( col, row ) occupiedSquares)
        |> (not << isJust)


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


canMoveToRepeating : Game -> Position -> Dict ( Int, Int ) Piece -> ( Int, Int ) -> Team -> List ( Int, Int ) -> Bool
canMoveToRepeating ({ turn } as game) ((Position squareToColumn squareToRow) as moveTo) occupiedSquares occupied team vectors =
    turn
        == team
        && List.any (\vector -> checkMoveInDirectionRepeating vector moveTo occupiedSquares occupied team) vectors
        && doesNotLeadToCheck occupied ( squareToColumn, squareToRow ) game


findPath : Position -> Dict ( Int, Int ) Piece -> ( Int, Int ) -> Team -> List ( Int, Int ) -> List ( Int, Int ) -> List ( Int, Int )
findPath moveTo occupiedSquares occupied team currentPath vectors =
    let
        final =
            List.map (\vector -> findPathRepeating vector moveTo occupiedSquares occupied team currentPath) vectors

        removeEmpties =
            List.filter (\l -> not (List.isEmpty l)) final
    in
    case removeEmpties of
        [] ->
            []

        p :: [] ->
            p

        _ ->
            -- You should never be able to have two paths to the same square.
            []


findPathRepeating : ( Int, Int ) -> Position -> Dict ( Int, Int ) Piece -> ( Int, Int ) -> Team -> List ( Int, Int ) -> List ( Int, Int )
findPathRepeating ( columnDelta, rowDeltay ) (Position column row) occupiedSquares ( currentColumn, currentRow ) team currentPath =
    let
        nextColumn =
            columnDelta + currentColumn

        nextRow =
            rowDeltay + currentRow

        piece =
            Dict.get ( nextColumn, nextRow ) occupiedSquares

        nextPath =
            ( nextColumn, nextRow ) :: currentPath
    in
    if nextColumn == 0 || nextRow == 0 || nextColumn == 9 || nextRow == 9 then
        []

    else if ( nextColumn, nextRow ) == ( column, row ) && not (occupiedByFriendly piece team) then
        nextPath

    else if occupiedSquare piece then
        []

    else
        findPathRepeating ( columnDelta, rowDeltay ) (Position column row) occupiedSquares ( nextColumn, nextRow ) team nextPath


horizontalMovement =
    [ west
    , east
    , north
    , south
    ]


diagonalMovement =
    [ northWest
    , northEast
    , southWest
    , southEast
    ]


knightMovement =
    [ ( -2, -1 )
    , ( -1, -2 )
    , ( 1, -2 )
    , ( 2, -1 )
    , ( 2, 1 )
    , ( 1, 2 )
    , ( -1, 2 )
    , ( -2, 1 )
    ]


west =
    ( -1, 0 )


east =
    ( 1, 0 )


north =
    ( 0, 1 )


south =
    ( 0, -1 )


northEast =
    ( 1, 1 )


southWest =
    ( -1, -1 )


southEast =
    ( -1, 1 )


northWest =
    ( 1, -1 )


checkMoveInDirection : ( Int, Int ) -> Position -> Dict ( Int, Int ) Piece -> ( Int, Int ) -> Team -> Bool
checkMoveInDirection ( columnDelta, rowDelta ) (Position column row) occupiedSquares ( currentColumn, currentRow ) team =
    let
        appliedColumn =
            currentColumn + columnDelta

        appliedRow =
            currentRow + rowDelta

        piece =
            Dict.get ( appliedColumn, appliedRow ) occupiedSquares
    in
    ( appliedColumn, appliedRow ) == ( column, row ) && not (occupiedByFriendly piece team)


checkMoveInDirectionRepeating : ( Int, Int ) -> Position -> Dict ( Int, Int ) Piece -> ( Int, Int ) -> Team -> Bool
checkMoveInDirectionRepeating ( columnDelta, rowDeltay ) (Position column row) occupiedSquares ( currentColumn, currentRow ) team =
    let
        nextColumn =
            columnDelta + currentColumn

        nextRow =
            rowDeltay + currentRow

        piece =
            Dict.get ( nextColumn, nextRow ) occupiedSquares
    in
    if nextColumn <= 0 || nextRow <= 0 || nextColumn >= 9 || nextRow >= 9 then
        False

    else if ( nextColumn, nextRow ) == ( column, row ) && not (occupiedByFriendly piece team) then
        True

    else if occupiedSquare piece then
        False

    else
        checkMoveInDirectionRepeating ( columnDelta, rowDeltay ) (Position column row) occupiedSquares ( nextColumn, nextRow ) team


occupiedSquare : Maybe Piece -> Bool
occupiedSquare piece =
    case piece of
        Nothing ->
            False

        Just (Piece _ t) ->
            True


occupiedByFriendly : Maybe Piece -> Team -> Bool
occupiedByFriendly piece team =
    case piece of
        Nothing ->
            False

        Just (Piece t _) ->
            t == team



-- SERIALIZATION


fromFen : List Move -> String -> Result String Game
fromFen moves fen =
    case String.split " " fen of
        [ rawBoard, rawTurn, rawCastlingRights, rawEnpassant, e, f ] ->
            Result.map4
                (\board turn enpassant castlingRights ->
                    Game (Dict.fromList (List.map (\( p, lol ) -> ( Position.toRaw p, lol )) board)) turn enpassant castlingRights (movesToDict moves) (movesDict moves) fen
                )
                (D.decodeValue boardDecoder (E.string rawBoard))
                (D.decodeValue turnDecoder (E.string rawTurn))
                (D.decodeValue enpassantDecoder (E.string rawEnpassant))
                (D.decodeValue castlingRightsDecoder (E.string rawCastlingRights))
                |> Result.mapError (\_ -> "Parsing failed")

        _ ->
            Err <| fen ++ " doesn't look right. FEN needs to have 6 pieces of info"



-- TODO: List into custom codec


movesToDict : List Move -> Dict MoveKey Move
movesToDict moves =
    List.map (\move -> ( moveKey move, move )) moves
        |> Dict.fromList


moveKey : Move -> MoveKey
moveKey { squareFrom, squareTo } =
    squareFrom ++ squareTo


type alias MoveKey =
    String


{-| squareTo => [movesToIt]
-}
movesDict : List Move -> Dict PositionKey (List Move)
movesDict moves =
    List.map (\pos -> ( Position.toAlgebraic pos, List.filter (\m -> m.squareTo == Position.toAlgebraic pos) moves )) Position.all
        |> Dict.fromList


type alias PositionKey =
    String



-- TODO: Turn instead of Team


turnDecoder : D.Decoder Team
turnDecoder =
    D.andThen parseTurn D.string


parseTurn : String -> D.Decoder Team
parseTurn t =
    case t of
        "b" ->
            D.succeed <| Black

        "w" ->
            D.succeed <| White

        notTurn ->
            D.fail <| notTurn ++ " is not a valid turn value"


boardDecoder : D.Decoder (List ( Position, Piece ))
boardDecoder =
    D.andThen parseFenBoard D.string


parseFenBoard : String -> D.Decoder (List ( Position, Piece ))
parseFenBoard board =
    String.split "/" board
        |> List.foldr parseFenBoardHelp (D.succeed <| Builder 1 [])
        |> D.map .pieces


parseFenBoardHelp : String -> D.Decoder Builder -> D.Decoder Builder
parseFenBoardHelp currentRow rowAccumulator =
    D.andThen (parseFenRow currentRow) rowAccumulator


parseFenRow : String -> Builder -> D.Decoder Builder
parseFenRow fenRow { acc, pieces } =
    String.toList fenRow
        |> List.reverse
        |> List.foldr (parseFenRowHelp acc) (D.succeed <| Builder 0 [])
        |> D.map (\b -> Builder (acc + 1) (b.pieces ++ pieces))


parseFenRowHelp : Int -> Char -> D.Decoder Builder -> D.Decoder Builder
parseFenRowHelp row nextColumn accumulator =
    D.andThen (parseCharacter row nextColumn) accumulator


type alias Builder =
    { acc : Int
    , pieces : List ( Position, Piece )
    }


parseCharacter : Int -> Char -> Builder -> D.Decoder Builder
parseCharacter row c { acc, pieces } =
    if Char.isDigit c then
        case String.toInt (String.fromChar c) of
            Nothing ->
                D.fail "not a digit"

            Just cc ->
                D.succeed (Builder (acc + cc) pieces)

    else
        case parsePieceType <| Char.toLower c of
            Err err ->
                D.fail err

            Ok pieceType ->
                D.succeed (Builder (acc + 1) (pieces ++ [ ( Position (acc + 1) row, Piece (parseTeam c) pieceType ) ]))


parseTeam : Char -> Team
parseTeam c =
    if Char.isLower c then
        Black

    else
        White


parsePieceType : Char -> Result String PieceType
parsePieceType c =
    case c of
        'k' ->
            Ok Monarch

        'q' ->
            Ok Advisor

        'r' ->
            Ok Rook

        'b' ->
            Ok Bishop

        'n' ->
            Ok Knight

        'p' ->
            Ok Pawn

        cc ->
            Err <| String.fromChar cc ++ " is not a valid pieceType"


enpassantDecoder : D.Decoder (Maybe Position)
enpassantDecoder =
    D.andThen parseEnpassant D.string


parseEnpassant : String -> D.Decoder (Maybe Position)
parseEnpassant t =
    case t of
        "-" ->
            D.succeed <| Nothing

        algebraic ->
            D.map Just (Position.decode algebraic)


castlingRightsDecoder : D.Decoder (List CastlingRight)
castlingRightsDecoder =
    D.andThen parseCastlingRights D.string



-- TODO. . . what is even happening in here?


parseCastlingRights : String -> D.Decoder (List CastlingRight)
parseCastlingRights t =
    case t of
        "-" ->
            D.succeed []

        castlingRights ->
            case
                List.foldl
                    (\a b ->
                        case b of
                            Ok going ->
                                case parseCastlingRight a of
                                    Ok nextOne ->
                                        Ok (nextOne :: going)

                                    Err err ->
                                        Err err

                            Err err ->
                                Err err
                    )
                    (Ok [])
                    (String.split "" castlingRights)
            of
                Ok rights ->
                    D.succeed rights

                Err bad ->
                    D.fail ""


parseCastlingRight : String -> Result String CastlingRight
parseCastlingRight t =
    case t of
        "q" ->
            Ok <| AdvisorSide Black

        "k" ->
            Ok <| MonarchSide Black

        "Q" ->
            Ok <| AdvisorSide White

        "K" ->
            Ok <| MonarchSide White

        bad ->
            Err <| bad ++ " is not a valid castlingRight"



--
-- VIEW
--


view : Responsive -> Model -> Html msg
view responsive (Model game) =
    --, movesHistorical, reinforcing, opponentReinforcing, playerTeam, dragStuff, browserBoard, mainNav, previousMove } =
    --    let
    --        sideBySide board =
    --            div [ class "flex flex-col sm:flex-col md:flex-col lg:flex-row xl:flex-row 2xl:flex-row" ]
    --                [ div [ class "flex-grow flex-shrink-0" ] [ board ]
    --                , div [ class "flex-shrink-0" ]
    --                    [ div [] [ h3 [ Attr.class "text-xl h-16" ] [ text "Details" ] ]
    --                    , viewLegend
    --                        (Settings
    --                            { recentMove = True
    --                            , reinforcing = List.isEmpty reinforcing |> not
    --                            , opponentReinforcing = List.isEmpty opponentReinforcing |> not
    --                            }
    --                        )
    --                    ]
    --                ]
    --    in
    case game of
        Animating { before, after } ->
            --sideBySide <|
            div [ class "container mx-auto flex-grow" ]
                [ --lazy2 viewTurn before playerTeam
                  --, Maybe.map2 (lazy4 viewPreviousMove before playerTeam) previousMove browserBoard |> Maybe.withDefault (span [] [])
                  --lazy3 viewBoard (SquareStuff before movesHistorical reinforcing opponentReinforcing playerTeam) dragStuff browserBoard
                  lazy3 viewBoard responsive (SquareStuff before (Historical []) [] [] Black) NoPieceInHand
                ]

        DoneAnimating g ->
            --sideBySide <|
            div [ class "container mx-auto flex-grow" ]
                [ --lazy2 viewTurn g playerTeam
                  -- Maybe.map (lazy4 viewGhostSquare g playerTeam dragStuff) browserBoard |> Maybe.withDefault (span [] [])
                  lazy3 viewBoard responsive (SquareStuff g (Historical []) [] [] Black) NoPieceInHand

                --, lazy viewSettings playerTeam
                ]



--viewSettings : Team -> Html Msg
--viewSettings color =
--    div []
--        [ fieldset []
--            [ radio "Black" (color == Logic.Black) (\_ -> ChangeTeam Logic.Black)
--            , radio "White" (color == Logic.White) (\_ -> ChangeTeam Logic.White)
--            ]
--        ]
--radio : String -> Bool -> (String -> Msg) -> Html Msg
--radio value isChecked msg =
--    label
--        []
--        [ input [ type_ "radio", name "Team", onInput msg, checked isChecked ] []
--        , text value
--        ]
--
--viewTurn : Logic.Game -> Team -> Html Msg
--viewTurn game playerColor =
--    div [ class "container mx-auto text-2xl" ]
--        [ if game.turn == playerColor then
--            text "You are up."
--
--          else
--            --text "Opponents turn"
--            text "Waiting on them!"
--        ]


type alias SquareStuff =
    { game : Game
    , historical : Historical
    , reinforcing : List Position
    , opponentReinforcing : List Position
    , playerColor : Team
    }


viewBoard : Responsive -> SquareStuff -> DragStuff -> Html msg
viewBoard responsive ({ game, playerColor, historical } as cellStuff) dragStuff =
    div []
        [ div
            [ id "main-board"
            , classList
                [ ( "grid grid-cols-8 grid-rows-8 border-2 border-gray-500 gap-0 shadow-2xl", True )

                --[ ( "h-96 w-96 md:h-constrained-1/2 md:w-constrained-1/2 lg:h-constrained-40% lg:w-constrained-40% 2xl:h-constrained-40% 2xl:w-constrained-40% grid grid-cols-8 grid-rows-8 border-2 border-gray-500 gap-0 shadow-2xl", True )
                , ( "rotated", playerColor == White )
                ]
            , Responsive.square responsive
            ]
            (List.concatMap (viewRow cellStuff) (List.range 1 8))
        ]


viewRow : SquareStuff -> Int -> List (Html msg)
viewRow cellStuff row =
    List.map (lazy3 viewCell cellStuff row) (List.reverse (List.range 1 8))


viewCell : SquareStuff -> Int -> Int -> Html msg
viewCell ({ playerColor } as cellStuff) row column =
    div
        [ classList
            [ ( "column-" ++ String.fromInt column, True )
            , ( "row-" ++ String.fromInt row, True )
            , ( shading column row, True )
            , ( "rotated", playerColor == White )
            ]
        ]
        [ lazy2 viewSquare cellStuff (Position column row) ]



-- BRING BACK
--viewGhostSquare : Logic.Game -> Team -> DragStuff -> Browser.Dom.Element -> Html Msg
--viewGhostSquare game playerColor dragStuff browserBoard =
--    let
--        squareSize =
--            round <| browserBoard.element.width / 8
--    in
--    case dragStuff of
--        NoPieceInHand ->
--            span [] []
--
--        PieceInHand ({ piece } as dragStuffInner) ->
--            let
--                ( x, y ) =
--                    getDragCoordinates dragStuffInner
--            in
--            div
--                [ classList
--                    [ ( "bg-transparent", True )
--                    , ( "grid-cols-none", True )
--                    , ( "fixed", True )
--                    , ( "z-50", True )
--                    , ( "h-12 w-12 md:h-16 md:w-16 lg:h-20 lg:w-20 2xl:h-28 2xl:w-28", True )
--                    ]
--                , styleList
--                    [ "left: " ++ String.fromInt (x - (squareSize // 2)) ++ "px"
--                    , "top: " ++ String.fromInt (y - (squareSize // 2)) ++ "px"
--                    , "cursor: grabbing"
--                    ]
--                , on "mouseup" (D.map2 StopDragging pageX pageY)
--                ]
--                [ findSvg piece [] ]
--
--
--viewPreviousMove : Logic.Game -> Team -> BasicMoveWithPieces -> Browser.Dom.Element -> Html Msg
--viewPreviousMove game playerColor previousMove browserBoard =
--    let
--        squareSize =
--            browserBoard.element.width / 8
--    in
--    case previousMove of
--        BasicMoveWithPieces { from, to, pieceMoved, pieceCaptured } ->
--            div []
--                [ Animated.div (runFull { starting = Position.toRaw from, diff = Position.toRaw to } playerColor squareSize)
--                    [ classList
--                        [ ( "grid-cols-none", True )
--                        , ( "absolute", True )
--                        , ( "z-50", True )
--
--                        --, ( "border-2", True )
--                        ]
--                    , styleList
--                        [ "width: " ++ String.fromInt (round squareSize) ++ "px"
--                        , "height: " ++ String.fromInt (round squareSize) ++ "px"
--                        ]
--                    ]
--                    [ findSvg pieceMoved [] ]
--                , case pieceCaptured of
--                    Nothing ->
--                        div [] []
--
--                    Just pC ->
--                        Animated.div (runHide { starting = Position.toRaw to } playerColor squareSize)
--                            [ classList
--                                [ ( "grid-cols-none", True )
--                                , ( "absolute", True )
--                                , ( "z-50", True )
--                                ]
--                            , styleList
--                                [ "width: " ++ String.fromInt (round squareSize) ++ "px"
--                                , "height: " ++ String.fromInt (round squareSize) ++ "px"
--                                ]
--                            ]
--                            [ findSvg pC [] ]
--                ]
--
--        CastlingMove { monarchFrom, monarchTo, rookFrom, rookTo, team } ->
--            div []
--                [ Animated.div (runFull { starting = Position.toRaw monarchFrom, diff = Position.toRaw monarchTo } playerColor squareSize)
--                    [ classList
--                        [ ( "grid-cols-none", True )
--                        , ( "absolute", True )
--                        , ( "z-50", True )
--                        ]
--                    , styleList
--                        [ "width: " ++ String.fromInt (round squareSize) ++ "px"
--                        , "height: " ++ String.fromInt (round squareSize) ++ "px"
--                        ]
--                    ]
--                    [ findSvg (Logic.Piece team Logic.Monarch) [] ]
--                , Animated.div (runFull { starting = Position.toRaw rookFrom, diff = Position.toRaw rookTo } playerColor squareSize)
--                    [ classList
--                        [ ( "grid-cols-none", True )
--                        , ( "absolute", True )
--                        , ( "z-50", True )
--                        ]
--                    , styleList
--                        [ "width: " ++ String.fromInt (round squareSize) ++ "px"
--                        , "height: " ++ String.fromInt (round squareSize) ++ "px"
--                        ]
--                    ]
--                    [ findSvg (Logic.Piece team Logic.Rook) [] ]
--                ]
--
--
--runHide : { starting : ( Int, Int ) } -> Team -> Float -> Animation
--runHide { starting } playerColor squareSize =
--    let
--        borderSize =
--            -2
--    in
--    Animation.steps { startAt = [ squareLocation starting squareSize borderSize playerColor |> (\( x, y ) -> P.xy x y) ], options = [ Animation.delay 500 ] }
--        [ Animation.step 400 [ P.opacity 0, squareLocation starting squareSize borderSize playerColor |> (\( x, y ) -> P.xy x y) ]
--        ]
--
--
--runFull : { starting : ( Int, Int ), diff : ( Int, Int ) } -> Team -> Float -> Animation
--runFull { starting, diff } playerColor squareSize =
--    let
--        borderSize =
--            case playerColor of
--                Logic.Black ->
--                    0
--
--                Logic.White ->
--                    0
--    in
--    Animation.steps
--        { startAt =
--            [ squareLocation starting squareSize borderSize playerColor |> (\( x, y ) -> P.xy x y)
--            ]
--        , options = [ Animation.easeInOut ]
--        }
--        [ Animation.wait 500
--        , Animation.step 400 [ squareLocation diff squareSize borderSize playerColor |> (\( x, y ) -> P.xy x y) ]
--        , Animation.wait 300
--        , Animation.step 500
--            [ squareLocation diff squareSize borderSize playerColor |> (\( x, y ) -> P.xy x y)
--            , P.backgroundColor "rgba(156, 163, 175, 0.7)"
--            ]
--        ]
--squareLocation : ( Int, Int ) -> Float -> Float -> Team -> ( Float, Float )
--squareLocation ( squareDiffX, squareDiffY ) squareSize borderSize playerColor =
--    let
--        xPerspectiveCorrected =
--            case playerColor of
--                Logic.Black ->
--                    toFloat (8 - squareDiffX)
--
--                Logic.White ->
--                    toFloat (squareDiffX - 1)
--
--        yPerspectiveCorrected =
--            case playerColor of
--                Logic.Black ->
--                    toFloat (squareDiffY - 1)
--
--                Logic.White ->
--                    toFloat (8 - squareDiffY)
--    in
--    ( xPerspectiveCorrected * squareSize + borderSize, yPerspectiveCorrected * squareSize + borderSize )
--styleList : List String -> Attribute Msg
--styleList styles =
--    String.join "; " styles
--        |> style
--getDragCoordinates : DragStuffInner -> ( Int, Int )
--getDragCoordinates { x, y, dragState } =
--    let
--        (Moving startX startY endX endY) =
--            dragState
--    in
--    ( x + endX - startX, y + endY - startY )
-- BRING BACK


viewSquare : SquareStuff -> Position -> Html msg
viewSquare { game, historical, reinforcing, opponentReinforcing, playerColor } position =
    case ( game.turn == playerColor, Dict.get (Position.toRaw position) game.occupiedSquares ) of
        ( True, Just piece ) ->
            let
                additionalSquareClasses =
                    [ ( "bg-green-500", reinforces position reinforcing )
                    , ( "bg-yellow-500", reinforces position opponentReinforcing )
                    , ( Position.toAlgebraic position, True )
                    ]

                additionalSquareAttrs =
                    [ style "cursor: grab"

                    --, on "mousedown" (D.map4 StartDragging pageX pageY (D.succeed position) (D.succeed piece))
                    ]
            in
            basicSquare position historical additionalSquareClasses additionalSquareAttrs [ findSvg piece [] ]

        ( False, Just piece ) ->
            basicSquare position historical [] [] [ findSvg piece [] ]

        ( _, Nothing ) ->
            basicSquare position historical [] [] []


basicSquare position historical additionalSquareClasses additionalSquareAttrs additionalSquareChildren =
    div
        ([ classList
            ([ ( "w-full h-full m-0", True )
             , ( "bg-gray-400 bg-opacity-70", isRecentMove position historical )
             , ( "transition", True )
             ]
                ++ additionalSquareClasses
            )
         ]
            ++ additionalSquareAttrs
        )
        additionalSquareChildren


isRecentMove : Position -> Historical -> Bool
isRecentMove position (Historical directional) =
    case directional of
        [] ->
            False

        ( BasicMove { from, to }, _ ) :: _ ->
            from == position || to == position


reinforces : Position -> List Position -> Bool
reinforces position reinforcing =
    List.any ((==) position) reinforcing


shading : Int -> Int -> String
shading column row =
    if modBy 2 (column + row) == 0 then
        "bg-gray-200 bg-opacity-50"

    else
        "bg-indigo-400 bg-opacity-50"



-- SQUARE PROPERTIES


friendlyMovesToPosition : Team -> Position -> Position -> List Move -> List Move
friendlyMovesToPosition turn squareTo squareFrom moves =
    movesToPosition squareTo squareFrom moves
        |> List.filter (\move -> move.color == turnToColor turn)



-- TODO: clean this up with a custom codec.


turnToColor : Team -> String
turnToColor color =
    case color of
        Black ->
            "b"

        White ->
            "w"


movesToPosition : Position -> Position -> List Move -> List Move
movesToPosition squareTo squareFrom =
    List.filter (\move -> move.squareFrom == Position.toAlgebraic squareFrom && move.squareTo == Position.toAlgebraic squareTo)



-- SVG


{-| -}
findSvg : Piece -> List (Html.Attribute msg) -> Html msg
findSvg (Piece color pieceType) =
    case ( color, pieceType ) of
        ( White, Pawn ) ->
            whitePawn

        ( Black, Pawn ) ->
            blackPawn

        ( White, Bishop ) ->
            whiteBishop

        ( Black, Bishop ) ->
            blackBishop

        ( White, Knight ) ->
            whiteKnight

        ( Black, Knight ) ->
            blackKnight

        ( White, Rook ) ->
            whiteRook

        ( Black, Rook ) ->
            blackRook

        ( White, Advisor ) ->
            whiteAdvisor

        ( Black, Advisor ) ->
            blackAdvisor

        ( White, Monarch ) ->
            whiteMonarch

        ( Black, Monarch ) ->
            blackMonarch


blackPawn : List (Html.Attribute msg) -> Html msg
blackPawn extraArguments =
    svgWithViewPort [ Svg.path [ d "m 22.5,9 c -2.21,0 -4,1.79 -4,4 0,0.89 0.29,1.71 0.78,2.38 C 17.33,16.5 16,18.59 16,21 c 0,2.03 0.94,3.84 2.41,5.03 C 15.41,27.09 11,31.58 11,39.5 H 34 C 34,31.58 29.59,27.09 26.59,26.03 28.06,24.84 29,23.03 29,21 29,18.59 27.67,16.5 25.72,15.38 26.21,14.71 26.5,13.89 26.5,13 c 0,-2.21 -1.79,-4 -4,-4 z", style "opacity:1; fill:#000000; fill-opacity:1; fill-rule:nonzero; stroke:#000000; stroke-width:1.5; stroke-linecap:round; stroke-linejoin:miter; stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [] ]


whitePawn : List (Html.Attribute msg) -> Html msg
whitePawn extraAttributes =
    svgWithViewPort [ Svg.path [ d "m 22.5,9 c -2.21,0 -4,1.79 -4,4 0,0.89 0.29,1.71 0.78,2.38 C 17.33,16.5 16,18.59 16,21 c 0,2.03 0.94,3.84 2.41,5.03 C 15.41,27.09 11,31.58 11,39.5 H 34 C 34,31.58 29.59,27.09 26.59,26.03 28.06,24.84 29,23.03 29,21 29,18.59 27.67,16.5 25.72,15.38 26.21,14.71 26.5,13.89 26.5,13 c 0,-2.21 -1.79,-4 -4,-4 z", style "opacity:1; fill:#ffffff; fill-opacity:1; fill-rule:nonzero; stroke:#000000; stroke-width:1.5; stroke-linecap:round; stroke-linejoin:miter; stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [] ]


blackBishop : List (Html.Attribute msg) -> Html msg
blackBishop extraAttributes =
    svgWithViewPort [ g [ style "opacity:1; fill:none; fill-rule:evenodd; fill-opacity:1; stroke:#000000; stroke-width:1.5; stroke-linecap:round; stroke-linejoin:round; stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ g [ style "fill:#000000; stroke:#000000; stroke-linecap:butt;" ] [ Svg.path [ d "M 9,36 C 12.39,35.03 19.11,36.43 22.5,34 C 25.89,36.43 32.61,35.03 36,36 C 36,36 37.65,36.54 39,38 C 38.32,38.97 37.35,38.99 36,38.5 C 32.61,37.53 25.89,38.96 22.5,37.5 C 19.11,38.96 12.39,37.53 9,38.5 C 7.65,38.99 6.68,38.97 6,38 C 7.35,36.54 9,36 9,36 z" ] [], Svg.path [ d "M 15,32 C 17.5,34.5 27.5,34.5 30,32 C 30.5,30.5 30,30 30,30 C 30,27.5 27.5,26 27.5,26 C 33,24.5 33.5,14.5 22.5,10.5 C 11.5,14.5 12,24.5 17.5,26 C 17.5,26 15,27.5 15,30 C 15,30 14.5,30.5 15,32 z" ] [], Svg.path [ d "M 25 8 A 2.5 2.5 0 1 1 20,8 A 2.5 2.5 0 1 1 25 8 z" ] [] ], Svg.path [ d "M 17.5,26 L 27.5,26 M 15,30 L 30,30 M 22.5,15.5 L 22.5,20.5 M 20,18 L 25,18", style "fill:none; stroke:#ffffff; stroke-linejoin:miter;" ] [] ] ]


whiteBishop : List (Html.Attribute msg) -> Html msg
whiteBishop extraAttributes =
    svgWithViewPort [ g [ style "opacity:1; fill:none; fill-rule:evenodd; fill-opacity:1; stroke:#000000; stroke-width:1.5; stroke-linecap:round; stroke-linejoin:round; stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ g [ style "fill:#ffffff; stroke:#000000; stroke-linecap:butt;" ] [ Svg.path [ d "M 9,36 C 12.39,35.03 19.11,36.43 22.5,34 C 25.89,36.43 32.61,35.03 36,36 C 36,36 37.65,36.54 39,38 C 38.32,38.97 37.35,38.99 36,38.5 C 32.61,37.53 25.89,38.96 22.5,37.5 C 19.11,38.96 12.39,37.53 9,38.5 C 7.65,38.99 6.68,38.97 6,38 C 7.35,36.54 9,36 9,36 z" ] [], Svg.path [ d "M 15,32 C 17.5,34.5 27.5,34.5 30,32 C 30.5,30.5 30,30 30,30 C 30,27.5 27.5,26 27.5,26 C 33,24.5 33.5,14.5 22.5,10.5 C 11.5,14.5 12,24.5 17.5,26 C 17.5,26 15,27.5 15,30 C 15,30 14.5,30.5 15,32 z" ] [], Svg.path [ d "M 25 8 A 2.5 2.5 0 1 1 20,8 A 2.5 2.5 0 1 1 25 8 z" ] [] ], Svg.path [ d "M 17.5,26 L 27.5,26 M 15,30 L 30,30 M 22.5,15.5 L 22.5,20.5 M 20,18 L 25,18", style "fill:none; stroke:#000000; stroke-linejoin:miter;" ] [] ] ]


blackKnight : List (Html.Attribute msg) -> Html msg
blackKnight extraAttributes =
    svgWithViewPort [ g [ style "opacity:1; fill:none; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ Svg.path [ d "M 22,10 C 32.5,11 38.5,18 38,39 L 15,39 C 15,30 25,32.5 23,18", style "fill:#000000; stroke:#000000;" ] [], Svg.path [ d "M 24,18 C 24.38,20.91 18.45,25.37 16,27 C 13,29 13.18,31.34 11,31 C 9.958,30.06 12.41,27.96 11,28 C 10,28 11.19,29.23 10,30 C 9,30 5.997,31 6,26 C 6,24 12,14 12,14 C 12,14 13.89,12.1 14,10.5 C 13.27,9.506 13.5,8.5 13.5,7.5 C 14.5,6.5 16.5,10 16.5,10 L 18.5,10 C 18.5,10 19.28,8.008 21,7 C 22,7 22,10 22,10", style "fill:#000000; stroke:#000000;" ] [], Svg.path [ d "M 9.5 25.5 A 0.5 0.5 0 1 1 8.5,25.5 A 0.5 0.5 0 1 1 9.5 25.5 z", style "fill:#ffffff; stroke:#ffffff;" ] [], Svg.path [ d "M 15 15.5 A 0.5 1.5 0 1 1 14,15.5 A 0.5 1.5 0 1 1 15 15.5 z", transform "matrix(0.866,0.5,-0.5,0.866,9.693,-5.173)", style "fill:#ffffff; stroke:#ffffff;" ] [], Svg.path [ d "M 24.55,10.4 L 24.1,11.85 L 24.6,12 C 27.75,13 30.25,14.49 32.5,18.75 C 34.75,23.01 35.75,29.06 35.25,39 L 35.2,39.5 L 37.45,39.5 L 37.5,39 C 38,28.94 36.62,22.15 34.25,17.66 C 31.88,13.17 28.46,11.02 25.06,10.5 L 24.55,10.4 z ", style "fill:#ffffff; stroke:none;" ] [] ] ]


whiteKnight : List (Html.Attribute msg) -> Html msg
whiteKnight extraAttributes =
    svgWithViewPort [ g [ style "opacity:1; fill:none; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ Svg.path [ d "M 22,10 C 32.5,11 38.5,18 38,39 L 15,39 C 15,30 25,32.5 23,18", style "fill:#ffffff; stroke:#000000;" ] [], Svg.path [ d "M 24,18 C 24.38,20.91 18.45,25.37 16,27 C 13,29 13.18,31.34 11,31 C 9.958,30.06 12.41,27.96 11,28 C 10,28 11.19,29.23 10,30 C 9,30 5.997,31 6,26 C 6,24 12,14 12,14 C 12,14 13.89,12.1 14,10.5 C 13.27,9.506 13.5,8.5 13.5,7.5 C 14.5,6.5 16.5,10 16.5,10 L 18.5,10 C 18.5,10 19.28,8.008 21,7 C 22,7 22,10 22,10", style "fill:#ffffff; stroke:#000000;" ] [], Svg.path [ d "M 9.5 25.5 A 0.5 0.5 0 1 1 8.5,25.5 A 0.5 0.5 0 1 1 9.5 25.5 z", style "fill:#000000; stroke:#000000;" ] [], Svg.path [ d "M 15 15.5 A 0.5 1.5 0 1 1 14,15.5 A 0.5 1.5 0 1 1 15 15.5 z", transform "matrix(0.866,0.5,-0.5,0.866,9.693,-5.173)", style "fill:#000000; stroke:#000000;" ] [] ] ]


blackRook : List (Html.Attribute msg) -> Html msg
blackRook extraAttributes =
    svgWithViewPort [ g [ style "opacity:1; fill:000000; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ Svg.path [ x "0", y "0", width "100%", height "100%", d "M 9,39 L 36,39 L 36,36 L 9,36 L 9,39 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 12.5,32 L 14,29.5 L 31,29.5 L 32.5,32 L 12.5,32 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 12,36 L 12,32 L 33,32 L 33,36 L 12,36 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 14,29.5 L 14,16.5 L 31,16.5 L 31,29.5 L 14,29.5 z ", style "stroke-linecap:butt;stroke-linejoin:miter;" ] [], Svg.path [ d "M 14,16.5 L 11,14 L 34,14 L 31,16.5 L 14,16.5 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 11,14 L 11,9 L 15,9 L 15,11 L 20,11 L 20,9 L 25,9 L 25,11 L 30,11 L 30,9 L 34,9 L 34,14 L 11,14 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 12,35.5 L 33,35.5 L 33,35.5", style "fill:none; stroke:#ffffff; stroke-width:1; stroke-linejoin:miter;" ] [], Svg.path [ d "M 13,31.5 L 32,31.5", style "fill:none; stroke:#ffffff; stroke-width:1; stroke-linejoin:miter;" ] [], Svg.path [ d "M 14,29.5 L 31,29.5", style "fill:none; stroke:#ffffff; stroke-width:1; stroke-linejoin:miter;" ] [], Svg.path [ d "M 14,16.5 L 31,16.5", style "fill:none; stroke:#ffffff; stroke-width:1; stroke-linejoin:miter;" ] [], Svg.path [ d "M 11,14 L 34,14", style "fill:none; stroke:#ffffff; stroke-width:1; stroke-linejoin:miter;" ] [] ] ]


whiteRook : List (Html.Attribute msg) -> Html msg
whiteRook extraAttributes =
    svgWithViewPort [ g [ style "opacity:1; fill:#ffffff; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ Svg.path [ d "M 9,39 L 36,39 L 36,36 L 9,36 L 9,39 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 12,36 L 12,32 L 33,32 L 33,36 L 12,36 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 11,14 L 11,9 L 15,9 L 15,11 L 20,11 L 20,9 L 25,9 L 25,11 L 30,11 L 30,9 L 34,9 L 34,14", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 34,14 L 31,17 L 14,17 L 11,14" ] [], Svg.path [ d "M 31,17 L 31,29.5 L 14,29.5 L 14,17", style "stroke-linecap:butt; stroke-linejoin:miter;" ] [], Svg.path [ d "M 31,29.5 L 32.5,32 L 12.5,32 L 14,29.5" ] [], Svg.path [ d "M 11,14 L 34,14", style "fill:none; stroke:#000000; stroke-linejoin:miter;" ] [] ] ]


blackAdvisor : List (Html.Attribute msg) -> Html msg
blackAdvisor extraAttributes =
    svgWithViewPort [ g [ style "opacity:1; fill:000000; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ g [ style "fill:#000000; stroke:none;" ] [ circle [ cx "6", cy "12", r "2.75" ] [], circle [ cx "14", cy "9", r "2.75" ] [], circle [ cx "22.5", cy "8", r "2.75" ] [], circle [ cx "31", cy "9", r "2.75" ] [], circle [ cx "39", cy "12", r "2.75" ] [] ], Svg.path [ d "M 9,26 C 17.5,24.5 30,24.5 36,26 L 38.5,13.5 L 31,25 L 30.7,10.9 L 25.5,24.5 L 22.5,10 L 19.5,24.5 L 14.3,10.9 L 14,25 L 6.5,13.5 L 9,26 z", style "stroke-linecap:butt; stroke:#000000;" ] [], Svg.path [ d "M 9,26 C 9,28 10.5,28 11.5,30 C 12.5,31.5 12.5,31 12,33.5 C 10.5,34.5 11,36 11,36 C 9.5,37.5 11,38.5 11,38.5 C 17.5,39.5 27.5,39.5 34,38.5 C 34,38.5 35.5,37.5 34,36 C 34,36 34.5,34.5 33,33.5 C 32.5,31 32.5,31.5 33.5,30 C 34.5,28 36,28 36,26 C 27.5,24.5 17.5,24.5 9,26 z", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 11,38.5 A 35,35 1 0 0 34,38.5", style "fill:none; stroke:#000000; stroke-linecap:butt;" ] [], Svg.path [ d "M 11,29 A 35,35 1 0 1 34,29", style "fill:none; stroke:#ffffff;" ] [], Svg.path [ d "M 12.5,31.5 L 32.5,31.5", style "fill:none; stroke:#ffffff;" ] [], Svg.path [ d "M 11.5,34.5 A 35,35 1 0 0 33.5,34.5", style "fill:none; stroke:#ffffff;" ] [], Svg.path [ d "M 10.5,37.5 A 35,35 1 0 0 34.5,37.5", style "fill:none; stroke:#ffffff;" ] [] ] ]


whiteAdvisor : List (Html.Attribute msg) -> Html msg
whiteAdvisor extraAttributes =
    svgWithViewPort [ g [ style "opacity:1; fill:#ffffff; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ Svg.path [ d "M 9 13 A 2 2 0 1 1 5,13 A 2 2 0 1 1 9 13 z", transform "translate(-1,-1)" ] [], Svg.path [ d "M 9 13 A 2 2 0 1 1 5,13 A 2 2 0 1 1 9 13 z", transform "translate(15.5,-5.5)" ] [], Svg.path [ d "M 9 13 A 2 2 0 1 1 5,13 A 2 2 0 1 1 9 13 z", transform "translate(32,-1)" ] [], Svg.path [ d "M 9 13 A 2 2 0 1 1 5,13 A 2 2 0 1 1 9 13 z", transform "translate(7,-4)" ] [], Svg.path [ d "M 9 13 A 2 2 0 1 1 5,13 A 2 2 0 1 1 9 13 z", transform "translate(24,-4)" ] [], Svg.path [ d "M 9,26 C 17.5,24.5 27.5,24.5 36,26 L 38,14 L 31,25 L 31,11 L 25.5,24.5 L 22.5,9.5 L 19.5,24.5 L 14,11 L 14,25 L 7,14 L 9,26 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 9,26 C 9,28 10.5,28 11.5,30 C 12.5,31.5 12.5,31 12,33.5 C 10.5,34.5 11,36 11,36 C 9.5,37.5 11,38.5 11,38.5 C 17.5,39.5 27.5,39.5 34,38.5 C 34,38.5 35.5,37.5 34,36 C 34,36 34.5,34.5 33,33.5 C 32.5,31 32.5,31.5 33.5,30 C 34.5,28 36,28 36,26 C 27.5,24.5 17.5,24.5 9,26 z", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 11.5,30 C 15,29 30,29 33.5,30", style "fill:none;" ] [], Svg.path [ d "M 12,33.5 C 18,32.5 27,32.5 33,33.5", style "fill:none;" ] [] ] ]


blackMonarch : List (Html.Attribute msg) -> Html msg
blackMonarch extraAttributes =
    svgWithViewPort [ g [ style "fill:none; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ Svg.path [ d "M 22.5,11.63 L 22.5,6", style "fill:none; stroke:#000000; stroke-linejoin:miter;", id "path6570" ] [], Svg.path [ d "M 22.5,25 C 22.5,25 27,17.5 25.5,14.5 C 25.5,14.5 24.5,12 22.5,12 C 20.5,12 19.5,14.5 19.5,14.5 C 18,17.5 22.5,25 22.5,25", style "fill:#000000;fill-opacity:1; stroke-linecap:butt; stroke-linejoin:miter;" ] [], Svg.path [ d "M 12.5,37 C 18,40.5 27,40.5 32.5,37 L 32.5,30 C 32.5,30 41.5,25.5 38.5,19.5 C 34.5,13 25,16 22.5,23.5 L 22.5,27 L 22.5,23.5 C 20,16 10.5,13 6.5,19.5 C 3.5,25.5 12.5,30 12.5,30 L 12.5,37", style "fill:#000000; stroke:#000000;" ] [], Svg.path [ d "M 20,8 L 25,8", style "fill:none; stroke:#000000; stroke-linejoin:miter;" ] [], Svg.path [ d "M 32,29.5 C 32,29.5 40.5,25.5 38.03,19.85 C 34.15,14 25,18 22.5,24.5 L 22.5,26.6 L 22.5,24.5 C 20,18 10.85,14 6.97,19.85 C 4.5,25.5 13,29.5 13,29.5", style "fill:none; stroke:#ffffff;" ] [], Svg.path [ d "M 12.5,30 C 18,27 27,27 32.5,30 M 12.5,33.5 C 18,30.5 27,30.5 32.5,33.5 M 12.5,37 C 18,34 27,34 32.5,37", style "fill:none; stroke:#ffffff;" ] [] ] ]


whiteMonarch : List (Html.Attribute msg) -> Html msg
whiteMonarch extraAttributes =
    svgWithViewPort [ g [ style "fill:none; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ Svg.path [ d "M 22.5,11.63 L 22.5,6", style "fill:none; stroke:#000000; stroke-linejoin:miter;" ] [], Svg.path [ d "M 20,8 L 25,8", style "fill:none; stroke:#000000; stroke-linejoin:miter;" ] [], Svg.path [ d "M 22.5,25 C 22.5,25 27,17.5 25.5,14.5 C 25.5,14.5 24.5,12 22.5,12 C 20.5,12 19.5,14.5 19.5,14.5 C 18,17.5 22.5,25 22.5,25", style "fill:#ffffff; stroke:#000000; stroke-linecap:butt; stroke-linejoin:miter;" ] [], Svg.path [ d "M 12.5,37 C 18,40.5 27,40.5 32.5,37 L 32.5,30 C 32.5,30 41.5,25.5 38.5,19.5 C 34.5,13 25,16 22.5,23.5 L 22.5,27 L 22.5,23.5 C 20,16 10.5,13 6.5,19.5 C 3.5,25.5 12.5,30 12.5,30 L 12.5,37", style "fill:#ffffff; stroke:#000000;" ] [], Svg.path [ d "M 12.5,30 C 18,27 27,27 32.5,30", style "fill:none; stroke:#000000;" ] [], Svg.path [ d "M 12.5,33.5 C 18,30.5 27,30.5 32.5,33.5", style "fill:none; stroke:#000000;" ] [], Svg.path [ d "M 12.5,37 C 18,34 27,34 32.5,37", style "fill:none; stroke:#000000;" ] [] ] ]


svgWithViewPort : List (Svg msg) -> Html msg
svgWithViewPort =
    svg [ viewBox "-6 -6 60 60", version "1.1", width "100%", height "100%" ]
