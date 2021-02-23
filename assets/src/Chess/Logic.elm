module Chess.Logic exposing
    ( BasicMove
    , CastlingRight(..)
    , Game
    , MoveKey
    , Piece(..)
    , PieceType(..)
    , Square(..)
    , Team(..)
    , blankBoard
    , canAttack
    , canMoveTo
    , findChecks
    , fromFen
    , init
    , isCheckmate
    , makeMove
    , nextTurn
    , removePiece
    )

import Chess.Position as Position exposing (Position(..))
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Page.Learn.Scenario exposing (Move)
import Set exposing (Set)


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


type alias Game =
    -- TODO: Extract OccupiedSquares into new location (so that data structure used is opaque).
    -- Provide appropriate accessors / map functions.
    { occupiedSquares : Dict ( Int, Int ) Piece
    , turn : Team
    , enpassant : Maybe Position
    , castlingRights : List CastlingRight
    , recentMove : Maybe BasicMove
    , moves : Dict MoveKey Move
    }


type alias BasicMove =
    { from : Position, to : Position }


type CastlingRight
    = MonarchSide Team
    | AdvisorSide Team



{-

   Make a move on the board.

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
                    case ( squareFrom, squareTo ) of
                        -- NOTE: Elm doesn't appear to allow us to pattern match on our nice Position.e8 style types :(
                        -- Monarch starting square to castling square
                        -- e8       g8
                        ( ( 5, 8 ), ( 7, 8 ) ) ->
                            makeCastlingMove squareFrom squareTo occupiedSquares piece team turn game { rookStarting = ( 8, 8 ), rookEnding = ( 6, 8 ) }

                        -- e8       c8
                        ( ( 5, 8 ), ( 3, 8 ) ) ->
                            makeCastlingMove squareFrom squareTo occupiedSquares piece team turn game { rookStarting = ( 1, 8 ), rookEnding = ( 4, 8 ) }

                        -- e1       c1
                        ( ( 5, 1 ), ( 3, 1 ) ) ->
                            makeCastlingMove squareFrom squareTo occupiedSquares piece team turn game { rookStarting = ( 1, 1 ), rookEnding = ( 4, 1 ) }

                        -- e1       g1
                        ( ( 5, 1 ), ( 7, 1 ) ) ->
                            makeCastlingMove squareFrom squareTo occupiedSquares piece team turn game { rookStarting = ( 8, 1 ), rookEnding = ( 6, 1 ) }

                        ( _, _ ) ->
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


removePiece square ({ occupiedSquares } as game) =
    Dict.remove (Position.toRaw square) occupiedSquares
        |> (\updatedSquares -> { game | occupiedSquares = updatedSquares })


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
        |> Dict.remove rookStarting
        |> Dict.insert rookEnding (Piece team Rook)


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



-- NOTE: MetaGame isn't using this (instead it is using the fromFen function below).


init : List Square -> Team -> Maybe Position -> List CastlingRight -> Game
init squares turn enpassant castlingRights =
    Game (List.foldl asht Dict.empty squares) turn enpassant castlingRights Nothing Dict.empty


blankBoard : Game
blankBoard =
    Game Dict.empty Black Nothing [] Nothing Dict.empty



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


fromFen : List Move -> String -> Maybe BasicMove -> Result String Game
fromFen moves fen recentMove =
    case String.split " " fen of
        [ rawBoard, rawTurn, rawCastlingRights, rawEnpassant, e, f ] ->
            Result.map4
                (\board turn enpassant castlingRights ->
                    Game (Dict.fromList (List.map (\( p, lol ) -> ( Position.toRaw p, lol )) board)) turn enpassant castlingRights recentMove (movesToDict moves)
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
