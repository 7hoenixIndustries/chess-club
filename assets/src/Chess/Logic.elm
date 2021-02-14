module Chess.Logic exposing
    ( Piece(..)
    , PieceType(..)
    , Square(..)
    , Team(..)
    , canMoveTo
    , findChecks
    , init
    , isCheckmate
    , makeMove
    )

import Chess.Position as Position exposing (Position(..))
import Dict exposing (Dict)
import Prelude
import Set exposing (Set)


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
    = Piece PieceType Team


type Square
    = Occupied Position Piece


type alias Game =
    -- TODO: Extract OccupiedSquares into new location (so that data structure used is opaque).
    -- Provide appropriate accessors / map functions.
    { occupiedSquares : Dict ( Int, Int ) Piece
    , turn : Team
    , enpassant : Maybe Position
    }


makeMove : ( Int, Int ) -> ( Int, Int ) -> Game -> Game
makeMove squareFrom squareTo ({ occupiedSquares, turn, enpassant } as game) =
    case Dict.get squareFrom occupiedSquares of
        Nothing ->
            game

        Just ((Piece Pawn team) as piece) ->
            case enpassant of
                Nothing ->
                    Dict.remove squareFrom occupiedSquares
                        |> Dict.insert squareTo piece
                        |> (\updatedPieces -> { game | occupiedSquares = updatedPieces, turn = opponentTurn turn })

                Just (Position col row) ->
                    if ( col, row ) == squareTo then
                        case findPawnThatMadeEnpassantMove game of
                            Nothing ->
                                -- This should be impossible.
                                Dict.remove squareFrom occupiedSquares
                                    |> Dict.insert squareTo piece
                                    |> (\updatedPieces -> { game | occupiedSquares = updatedPieces, turn = opponentTurn turn })

                            Just (Position enpassantColumn enpassantRow) ->
                                Dict.remove squareFrom occupiedSquares
                                    |> Dict.remove ( enpassantColumn, enpassantRow )
                                    |> Dict.insert squareTo piece
                                    |> (\updatedPieces -> { game | occupiedSquares = updatedPieces, turn = opponentTurn turn })

                    else
                        Dict.remove squareFrom occupiedSquares
                            |> Dict.insert squareTo piece
                            |> (\updatedPieces -> { game | occupiedSquares = updatedPieces, turn = opponentTurn turn })

        Just piece ->
            Dict.remove squareFrom occupiedSquares
                |> Dict.insert squareTo piece
                |> (\updatedPieces -> { game | occupiedSquares = updatedPieces, turn = opponentTurn turn })


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


init : List Square -> Team -> Maybe Position -> Game
init squares turn enpassant =
    Game (List.foldl asht Dict.empty squares) turn enpassant



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
                        |> Maybe.withDefault ( ( 1, 1 ), Piece Monarch turn )

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
                |> Maybe.withDefault ( ( 1, 1 ), Piece Monarch turn )

        enemyTeam =
            List.filter (opponentTeam turn) occupiedAsList

        fromTuple ( a, b ) fn =
            fn a b
    in
    List.filter (\( pieceLocation, p ) -> pieceCanMoveTo { game | turn = opponentTurn turn } (fromTuple monarchLocation Position) occupiedSquares (opponentTurn turn) pieceLocation) enemyTeam
        |> List.map (\( pos, piece ) -> Occupied (fromTuple pos Position) piece)


opponentTurn : Team -> Team
opponentTurn turn =
    case turn of
        Black ->
            White

        White ->
            Black


isMonarch : ( a, Piece ) -> Bool
isMonarch ( _, Piece pieceType _ ) =
    case pieceType of
        Monarch ->
            True

        _ ->
            False


sameTeam : Team -> ( a, Piece ) -> Bool
sameTeam turn ( _, Piece _ pieceColor ) =
    pieceColor == turn


opponentTeam : Team -> ( a, Piece ) -> Bool
opponentTeam turn ( _, Piece _ pieceColor ) =
    pieceColor /= turn



-- MOVEMENT


pieceMovementToPath : Position -> Dict ( Int, Int ) Piece -> ( Int, Int ) -> List ( Int, Int )
pieceMovementToPath moveTo occupiedSquares occupied =
    case Dict.get occupied occupiedSquares of
        Nothing ->
            []

        Just (Piece Monarch _) ->
            -- TODO: What should happen here?
            []

        Just (Piece Advisor team) ->
            findPath moveTo occupiedSquares occupied team [] (horizontalMovement ++ diagonalMovement)

        Just (Piece Bishop team) ->
            findPath moveTo occupiedSquares occupied team [] diagonalMovement

        Just (Piece Rook team) ->
            findPath moveTo occupiedSquares occupied team [] horizontalMovement

        Just (Piece Knight team) ->
            -- TODO: Is this correct?
            []

        Just (Piece Pawn team) ->
            -- TODO: Is this correct?
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
                Piece Monarch team ->
                    canMoveToSingle game moveTo occupiedSquares occupied team (horizontalMovement ++ diagonalMovement)

                Piece Advisor team ->
                    canMoveToRepeating game moveTo occupiedSquares occupied team (horizontalMovement ++ diagonalMovement)

                Piece Rook team ->
                    canMoveToRepeating game moveTo occupiedSquares occupied team horizontalMovement

                Piece Bishop team ->
                    canMoveToRepeating game moveTo occupiedSquares occupied team diagonalMovement

                Piece Knight team ->
                    canMoveToSingle game moveTo occupiedSquares occupied team knightMovement

                Piece Pawn team ->
                    pawnCanMoveTo game moveTo occupiedSquares occupied team


doesNotLeadToCheck : ( Int, Int ) -> ( Int, Int ) -> Game -> Bool
doesNotLeadToCheck squareFrom squareTo game =
    makeMove squareFrom squareTo game
        |> (\virtualGame ->
                { virtualGame | turn = game.turn }
                    |> findChecks
                    |> List.isEmpty
           )


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
    if nextColumn == 0 || nextRow == 0 || nextColumn == 9 || nextRow == 9 then
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

        Just (Piece _ t) ->
            t == team
