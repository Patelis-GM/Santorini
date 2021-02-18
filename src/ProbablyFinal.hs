module StudentCode where
import Debug.Trace

type RowPos = Int
type ColumnPos = Int
type Position = (RowPos, ColumnPos)
type PlayerPositions = (Position, Position)
type BluePlayerPositions = PlayerPositions
type RedPlayerPositions = PlayerPositions
type Height = Int
type Building = (Height, Position)
type BuildingsList = [Building]
type Turn = Char
type Depth = Int
type PlayerId = Int
type Block = (PlayerId,TotalFloors)
type TotalFloors = Int
type Board = [[Block]]
type Stack = [State]


data State = State {  gameTurn::Int,
                      gameBoard::Board,
                      winnerFound::Bool,
                      bluePlayerPositions::BluePlayerPositions,
                      redPlayerPositions::RedPlayerPositions,
                      currentPlayer::Turn,
                      buildingsList::BuildingsList}

data Game = Game {
  gameState::State,
  undoStack::Stack,
  redoStack::Stack
}


-- #############
-- ### Utils ###
-- #############


belongs :: (Eq a) => a -> [a] -> Bool
belongs _ [] = False
belongs y (x:xs)
  | x == y = True
  | otherwise = belongs y xs

getAdjacent :: Position -> [Position]
getAdjacent (0,0) = [(a,b) | a <- [0..1] , b <-[0..1] , (a,b) /= (0,0)]
getAdjacent (0,4) = [(a,b) | a <- [0..1] , b <-[3..4] , (a,b) /= (0,4)]
getAdjacent (4,0) = [(a,b) | a <- [3..4] , b <-[0..1] , (a,b) /= (4,0)]
getAdjacent (4,4) = [(a,b) | a <- [3..4] , b <-[3..4] , (a,b) /= (4,4)]
getAdjacent (x,y)
  | x >= 1 && x <= 3 && y == 0 = [ (a,b) | a <- [(x-1)..(x+1)] , b <-[0..1] , (a,b) /= (x,0)]
  | x >= 1 && x <= 3 && y == 4 = [ (a,b) | a <- [(x-1)..(x+1)] , b <-[3..4] , (a,b) /= (x,4)]
  | x == 0 && y >= 1 && y <= 3 = [ (a,b) | a <- [0..1] , b <- [(y-1)..(y+1)] , (a,b) /= (0,y)]
  | x == 4 && y >= 1 && y <= 3 = [ (a,b) | a <- [3..4] , b <- [(y-1)..(y+1)] , (a,b) /= (4,y)]
  | otherwise = [(a,b) | a <- [(x-1)..(x+1)] , b <- [(y-1)..(y+1)], (a,b) /= (x,y)]

extendedAdjacent :: Position -> [Position]
extendedAdjacent (0,0) = [(a,b) | a <- [0..2] , b <-[0..2]]
extendedAdjacent (0,4) = [(a,b) | a <- [0..2] , b <-[2..4]]
extendedAdjacent (4,0) = [(a,b) | a <- [2..4] , b <-[0..2]]
extendedAdjacent (4,4) = [(a,b) | a <- [2..4] , b <-[2..4]]
extendedAdjacent (x,y)
  | x >= 1 && x <= 3 && y == 0 = [ (a,b) | a <- [(x-2)..(x+2)] , b <-[0..2] , a >= 0 , a <= 4]
  | x >= 1 && x <= 3 && y == 4 = [ (a,b) | a <- [(x-2)..(x+2)] , b <-[2..4] , a >= 0 , a <= 4]
  | x == 0 && y >= 1 && y <= 3 = [ (a,b) | a <- [0..2] , b <- [(y-2)..(y+2)] , b >= 0 , b <= 4]
  | x == 4 && y >= 1 && y <= 3 = [ (a,b) | a <- [2..4] , b <- [(y-2)..(y+2)] , b >= 0 , b <= 4]
  | otherwise = [(a,b) | a <- [(x-2)..(x+2)] , b <- [(y-2)..(y+2)], a >= 0 , a <= 4 , b >= 0 , b <= 4]


-- ###########################
-- ### Board Functionality ###
-- ###########################


createBoardHelper :: Int -> [Block]
createBoardHelper 0 = []
createBoardHelper x  = (0,0) : createBoardHelper (x - 1)

createBoard :: Int -> Int -> Board
createBoard 0 _ = []
createBoard rows cols = newRow : createBoard (rows - 1) cols
  where newRow = createBoardHelper cols

getBlock :: Board -> Position -> Block
getBlock board (rowIdx,colIdx) = value
  where row = board !! rowIdx
        value = row !! colIdx

getNeighborsHelper :: Board -> [Position] -> [Block]
getNeighborsHelper board [] = []
getNeighborsHelper board (p:ps) = pb : getNeighborsHelper board ps
  where pb = getBlock board p

getNeighbors :: Board -> Position -> [Block]
getNeighbors board pos = getNeighborsHelper board neighbors
  where neighbors = getAdjacent pos

setBlockHelper :: [Block] -> Int -> Block -> [Block]
setBlockHelper (b:bs) colIdx block
  | colIdx == 0 = block : bs
  | otherwise = b : setBlockHelper bs (colIdx - 1) block

setBlock :: Board -> Position -> Block -> Board
setBlock (r:rs) (rowIdx,colIdx) block
  | rowIdx == 0 = newRow : rs
  | otherwise = r : setBlock rs (rowIdx - 1,colIdx) block
    where newRow = setBlockHelper r colIdx block


-- ####################################
-- ### initializeGame Functionality ###
-- ####################################


defaultInitialPositions :: [Position] -> [Position]
defaultInitialPositions (bp1:bp2:rp1:rp2:[])
  | check1 == False && check2 == False && check3 == False && check4 == False =  (bp1:bp2:rp1:rp2:[])
  | otherwise = ((0,0):(0,4):(4,0):(4,4):[])
    where check1 = belongs bp1 (bp2:rp1:rp2:[])
          check2 = belongs bp2 (bp1:rp1:rp2:[])
          check3 = belongs rp1 (bp1:bp2:rp2:[])
          check4 = belongs rp2 (bp1:bp2:rp1:[])

initializeGame :: BluePlayerPositions -> RedPlayerPositions -> Game
initializeGame (bp1,bp2) (rp1,rp2) = Game initialState (initialState:[]) []
  where initialState = State 1 finalBoard False (rbp1,rbp2) (rrp1,rrp2) 'B' []
        gameBoard = createBoard 5 5
        blueBlock1 = (1,0)
        blueBlock2 = (3,0)
        redBlock1  = (2,0)
        redBlock2  = (4,0)
        boardOne  = setBlock gameBoard rbp1 blueBlock1
        boardTwo  = setBlock boardOne rbp2 blueBlock2
        boardThree = setBlock boardTwo rrp1 redBlock1
        finalBoard = setBlock boardThree rrp2 redBlock2
        (rbp1:rbp2:rrp1:rrp2:[]) = defaultInitialPositions (bp1:bp2:rp1:rp2:[])


-- ####################################
-- ### screenshotGame Functionality ###
-- ####################################


declareWinner :: Bool -> Turn -> Turn
declareWinner winnerFound player
  | winnerFound == True && player == 'R' = 'B'
  | winnerFound == True && player == 'B' = 'R'
  | otherwise = player

screenshotGame :: Game -> (Bool, Turn, BluePlayerPositions, RedPlayerPositions, BuildingsList)
screenshotGame (Game ((State _ _ winnerFound bluePlayerPositions redPlayerPositions currentPlayer buildingsList)) _ _) = (mWinnerFound,player,mBluePlayerPositions,mRedPlayerPositions,mBuildingsList)
  where player = declareWinner winnerFound currentPlayer
        mWinnerFound = winnerFound
        mBluePlayerPositions = bluePlayerPositions
        mRedPlayerPositions = redPlayerPositions
        mBuildingsList = buildingsList


-- #############################
-- ### tryMove Functionality ###
-- #############################


turnToId :: Turn -> (Int,Int)
turnToId 'B' = (1,3)
turnToId 'R' = (2,4)

positionsToBlocks :: Board -> (Position,Position,Position) -> (Block,Block,Block)
positionsToBlocks board (p1,p2,p3) = (b1,b2,b3)
  where b1 = getBlock board p1
        b2 = getBlock board p2
        b3 = getBlock board p3

possibleMove :: Turn -> (Position, Position, Position) -> (Block,Block,Block) -> Bool
possibleMove playerID (cp,np,bp) ((cbp,cbtf),(nbp,nbtf),(bbp,bbtf))
  | (a,b) == (1,3) && (cbp == 2 || cbp  == 4) =  False
  | (a,b) == (2,4) && (cbp == 1 || cbp  == 3) =  False
  | cbp == 0 = False
  | cp == np = False
  | nbtf == 4 = False
  | bbtf == 4 = False
  | (nbtf - cbtf) > 1 = False
  | currentBlockNextBlock == False = False
  | nextBlockBuildingBlock == False =  False
  | nbp /= 0 = False
  | cbp == 1 && (bbp == 2 || bbp == 3 || bbp == 4) =  False
  | cbp == 2 && (bbp == 1 || bbp == 3 || bbp == 4) =  False
  | cbp == 3 && (bbp == 1 || bbp == 2 || bbp == 4) =  False
  | cbp == 4 && (bbp == 1 || bbp == 2 || bbp == 3) =  False
  | otherwise = True
    where currentNeighbors = getAdjacent cp
          nextNeighbors = getAdjacent np
          currentBlockNextBlock = belongs np currentNeighbors
          nextBlockBuildingBlock = belongs bp nextNeighbors
          (a,b) = turnToId playerID

newPositions :: (Position,Position) -> (Position,Position) -> (Position,Position)
newPositions (p1,p2) (cp,np)
  | p1 == cp = (np,p2)
  | p2 == cp = (p1,np)
  | otherwise = (p1,p2)

newBuildings :: [Building] -> Position -> Int -> [Building]
newBuildings [] bp _ = ((1,bp):[])
newBuildings ((height,position):bs) bp bbtf
  | bp == position = (newHeight,position):bs
  | otherwise = (height,position) : newBuildings bs bp bbtf
    where newHeight = bbtf + 1

nextPlayer :: Turn -> Turn
nextPlayer player
  | player == 'R' = 'B'
  | otherwise = 'R'

hasAtLeastOneMove :: Block -> [Block] -> Bool
hasAtLeastOneMove _ [] = False
hasAtLeastOneMove (cbp,cbtf) ((nbp,nbtf):bs)
  | verdict == True = True
  | otherwise = hasAtLeastOneMove (cbp,cbtf) bs
    where verdict = ((nbp == 0) && ((nbtf-cbtf) <= 1))

hasValidMoves :: Board -> (Position,Position) -> Bool
hasValidMoves board (p1,p2) = (r1 || r2)
  where r1 = hasAtLeastOneMove p1Block p1AdjacentBlocks
        r2 = hasAtLeastOneMove p2Block p2AdjacentBlocks
        p1Block = getBlock board p1
        p2Block = getBlock board p2
        p1AdjacentBlocks = getNeighbors board p1
        p2AdjacentBlocks = getNeighbors board p2

winnerCheck :: Board -> Turn -> (Position,Position) -> (Position,Position) -> Bool
winnerCheck board currentPlayer (bp1,bp2) (rp1,rp2)
  | currentPlayer == 'R' = not(hasValidMoves board (bp1,bp2))
  | otherwise = not(hasValidMoves board (rp1,rp2))

floorCheck :: Int -> Bool
floorCheck floors
  | floors == 3 = True
  | otherwise = False


tryMove :: Game -> (Position, Position, Position) -> Game
tryMove (Game ((State turn board winner bluePositions redPositions currentPlayer buildingsList)) undoStack redoStack) (cp,np,bp)
  | validMove == False || winner == True = (Game ((State turn board winner bluePositions redPositions currentPlayer buildingsList)) undoStack redoStack)
  | otherwise = (Game newState newUndoStack [])
    where validMove = possibleMove currentPlayer (cp,np,bp) ((cbp,cbtf),(nbp,nbtf),(bbp,bbtf))
          ((cbp,cbtf),(nbp,nbtf),(bbp,bbtf)) = positionsToBlocks board (cp,np,bp)
          newCurrentBlock = (0,cbtf)
          newNextBlock =  (cbp,nbtf)
          newBuildingBlock = (0,(bbtf + 1))
          boardOne = setBlock board cp newCurrentBlock
          boardTwo = setBlock boardOne np newNextBlock
          newBoard = setBlock boardTwo bp newBuildingBlock
          newTurn = turn + 1
          newCurrentPlayer = nextPlayer currentPlayer
          newBluePos = newPositions bluePositions (cp,np)
          newRedPos = newPositions redPositions (cp,np)
          newBuildingsList = newBuildings buildingsList bp bbtf
          winnerFound = (floorCheck nbtf) || (winnerCheck newBoard currentPlayer newBluePos newRedPos)
          newState = (State newTurn newBoard winnerFound newBluePos newRedPos newCurrentPlayer newBuildingsList)
          newUndoStack = (newState:undoStack)


-- #######################################
-- ### undoMove/redoMove Functionality ###
-- #######################################


undoMove :: Game -> Game
undoMove (Game currentState (uds:[]) redoStack) = (Game currentState (uds:[]) redoStack)
undoMove (Game currentState (uds:udss) redoStack) = (Game olderState udss newRedoStack)
    where olderState = head udss
          newRedoStack = (uds:redoStack)


redoMove :: Game -> Game
redoMove (Game currentState undoStack []) = (Game currentState undoStack [])
redoMove (Game currentState undoStack (rds:rdss)) = (Game newerState newUndoStack rdss)
  where newerState = rds
        newUndoStack = (rds:undoStack)


-- ###################################
-- ### possibleMoves Functionality ###
-- ###################################


availableNextPosition :: Board -> Position -> Position -> Bool
availableNextPosition board cp np
  | ap == 0 && apf < 4 && (apf - pf) <= 1 = True
  | otherwise = False
    where (_,pf) = getBlock board cp
          (ap,apf) = getBlock board np

availableBuildPosition :: Board -> Position -> Int -> Bool
availableBuildPosition board bp pId
  | (p == 0 || p == pId) && tf < 4 = True
  | otherwise = False
    where (p,tf) = getBlock board bp

getMoves :: Board -> Position -> Int -> [(Position, Position, Position)]
getMoves board cp pId = [(cp,np,bp) | np <- (getAdjacent cp), bp <- (getAdjacent np) , let npAvailable = availableNextPosition board cp np , let bpAvailable = availableBuildPosition board bp pId , npAvailable, bpAvailable]

possibleMoves :: Game -> [(Position, Position, Position)]
possibleMoves  (Game ((State _ board _ (bp1,bp2) (rp1,rp2) currentPlayer _)) undoStack redoStack)
  | currentPlayer == 'B' = bc1 ++ bc2
  | otherwise = rc1 ++ rc2
    where bc1 = getMoves board bp1 p1id
          bc2 = getMoves board bp2 p2id
          rc1 = getMoves board rp1 p3id
          rc2 = getMoves board rp2 p4id
          (p1id,_) = getBlock board bp1
          (p2id,_) = getBlock board bp2
          (p3id,_) = getBlock board rp1
          (p4id,_) = getBlock board rp2


-- ###################################
-- ### evaluateState Functionality ###
-- ###################################


isNearCenter :: Position -> Bool
isNearCenter p = isAroundCenter
  where isAroundCenter = belongs p (getAdjacent (2,2))

getBoardPoints :: (Position,Position) -> Int
getBoardPoints (p1,p2)
  | p1 == (2,2) && p2NearCenter = 30
  | p2 == (2,2) && p1NearCenter = 30
  | p1 == (2,2) && not(p2NearCenter) = 10
  | p2 == (2,2) && not(p1NearCenter) = 10
  | p1NearCenter && p2NearCenter = 15
  | p1NearCenter || p2NearCenter = 5
  | otherwise = 0
     where p1NearCenter = isNearCenter p1
           p2NearCenter = isNearCenter p2

floorPoints :: Int -> Int -> Int
floorPoints x oa
  | x == 1 && oa == 0 = 40
  | x == 1 && oa == 1 = 10
  | x == 1 && oa == 2 = 5
  | x == 2 && oa == 0 = 60
  | x == 2 && oa == 1 = 20
  | x == 2 && oa == 2 = 10
  | x == 3 = 10000
  | otherwise = 0

getFloorPoints :: (Block,Block) -> (Int,Int) -> Int
getFloorPoints ((_,p1f),(_,p2f)) (oap1,oap2) = firstPoints + secondPoints
  where firstPoints = floorPoints p1f oap1
        secondPoints = floorPoints p2f oap2

imminentWinPoints :: Int -> [Block] -> Int -> Int -> Int
imminentWinPoints _ [] oa total
  | total == 2 = 500
  | total == 1 && oa == 0 = 150
  | total == 1 && oa == 1 = 100 -- 50
  | total == 1 && oa == 2 = 75 -- 25
  | otherwise = 0
imminentWinPoints _ _ _ 2 = 500
imminentWinPoints pf ((ap,af):bs) oa total
  | pf /= 2 = 0
  | pf == 2 && ap == 0 && af == 3 = imminentWinPoints pf bs oa (total + 1)
  | otherwise = imminentWinPoints pf bs oa total

getImminentWinPoints :: (Block,Block) -> ([Block],[Block]) -> (Int,Int) -> Int
getImminentWinPoints ((_,tf1),(_,tf2)) (n1,n2) (oap1,oap2)
  | firstPoints == 500 || secondPoints == 500 = 500
  | otherwise = firstPoints + secondPoints
    where firstPoints = imminentWinPoints tf1 n1 oap1 0
          secondPoints = imminentWinPoints tf2 n2 oap2 0

{-

flexibleBuildPosition :: Board -> Position -> Int -> Int
flexibleBuildPosition board bp pId
  | ((p == 0 || p == pId) && (tf == 3 || tf  == 2)) = 1
  | otherwise = 0
    where (p,tf) = getBlock board bp

flexibleNextPosition :: Board -> Position -> Position -> Int
flexibleNextPosition board cp np
  | ap /= 0 || (apf - pf) > 1 || apf == 4 || (apf - pf) < -1 = 0
  | otherwise = 1
    where (_,pf) = getBlock board cp
          (ap,apf) = getBlock board np

getFlexibilityMoves :: Board -> Position -> Int -> [(Position, Position,Position)]
getFlexibilityMoves board cp pId = [(cp,np,bp) | np <- (getAdjacent cp), bp <- (getAdjacent np),let npFlexible = flexibleNextPosition board cp np , let bpFlexible = flexibleBuildPosition board bp pId, bpFlexible > 0 ,npFlexible > 0]

evaluateFlexibility :: Int -> Int -> Int
evaluateFlexibility p1f p2f
  | p1f > 0 && p2f > 0 = 10
  | otherwise = 0

-}

allyNearbyPoints :: (Position,Position) -> Int
allyNearbyPoints (p1,p2)
  | (belongs p1 p2Neighborhood) || (belongs p2 p1Neighborhood) = 10
  | otherwise = 0
     where p1Neighborhood = extendedAdjacent p1
           p2Neighborhood = extendedAdjacent p2


opponentsAround :: Position -> (Position,Position) -> Int
opponentsAround p (o1,o2)
  | o1NearPlayer && o2NearPlayer = 2
  | o1NearPlayer && not(o2NearPlayer) = 1
  | not(o1NearPlayer) && o2NearPlayer = 1
  | otherwise = 0
    where pNeighborhood = extendedAdjacent p
          o1NearPlayer = belongs o1 pNeighborhood
          o2NearPlayer = belongs o2 pNeighborhood

isPlayerAlly :: Int -> Int -> Bool
isPlayerAlly p ap
  | (p == 1 || p == 3) && (ap == 1 || ap == 3) = True
  | (p == 2 || p == 4) && (ap == 2 || ap == 4) = True
  | otherwise = False

surroundingsPoints :: Block -> [Block] -> Int
surroundingsPoints _  [] = 0
surroundingsPoints (p,pf) ((ap,apf):bs)
  | fd == 0 && apAlly && pf == 0 = -5 + surroundingsPoints (p,pf) bs
  | fd == 0 = surroundingsPoints (p,pf) bs
  | fd == 1 && pf == 0 = surroundingsPoints (p,pf) bs
  | fd == 1 = 5 + surroundingsPoints (p,pf) bs
  | fd == 2 = -10 + surroundingsPoints (p,pf) bs
  | fd > 2 = -15 + surroundingsPoints (p,pf) bs
  | otherwise = -3 + surroundingsPoints (p,pf) bs
    where fd = apf - pf
          apAlly = isPlayerAlly p ap

getSurroundingsPoints :: (Block,Block) -> ([Block],[Block]) -> Int
getSurroundingsPoints ((p1,pf1),(p2,pf2)) (n1,n2) = firstPoints + secondPoints
  where firstPoints = surroundingsPoints (p1,pf1) n1
        secondPoints = surroundingsPoints (p2,pf2) n2

nearOpponentPoints :: Int -> Int
nearOpponentPoints oa
  | oa == 0 = 0
  | oa == 1 = 40
  | oa == 2 = 60

evaluateState :: Turn -> Game -> Int
evaluateState player (Game ((State gameTurn board _ (bp1,bp2) (rp1,rp2) _ _)) _ _)
  | player == 'B' = blueState
  | otherwise = redState
    where blueState =  (bpfp - rpfp) + bpnop + (bpiwp - rpiwp) + bpap + bpbp + bpsp
          redState =  (rpfp - bpfp) + rpnop + (rpiwp - bpiwp) + rpap + rpbp + rpsp
          bp1Neighbors = getNeighbors board bp1
          bp2Neighbors = getNeighbors board bp2
          rp1Neighbors = getNeighbors board rp1
          rp2Neighbors = getNeighbors board rp2
          (p1id,p1f) = getBlock board bp1
          (p2id,p2f) = getBlock board bp2
          (p3id,p3f) = getBlock board rp1
          (p4id,p4f) = getBlock board rp2
          bpfp = getFloorPoints ((p1id,p1f),(p2id,p2f)) (bp1oa,bp2oa)
          rpfp = getFloorPoints ((p3id,p3f),(p4id,p4f)) (rp1oa,rp2oa)
          bpbp = getBoardPoints (bp1,bp2)
          rpbp = getBoardPoints (rp1,rp2)
          bpiwp = getImminentWinPoints ((p1id,p1f),(p2id,p2f)) (bp1Neighbors,bp2Neighbors) (bp1oa,bp2oa)
          rpiwp = getImminentWinPoints ((p3id,p3f),(p4id,p4f)) (rp1Neighbors,rp2Neighbors) (rp1oa,rp2oa)
          {-bpf = evaluateFlexibility (length bp1f) (length bp2f)
          rpf = evaluateFlexibility (length rp1f) (length rp2f)
          bp1f = getFlexibilityMoves board bp1 p1id
          bp2f = getFlexibilityMoves board bp2 p2id
          rp1f = getFlexibilityMoves board rp1 p3id
          rp2f = getFlexibilityMoves board rp2 p4id-}
          bpap = allyNearbyPoints (bp1,bp2)
          rpap = allyNearbyPoints (rp1,rp2)
          bp1oa = opponentsAround bp1 (rp1,rp2)
          bp2oa = opponentsAround bp2 (rp1,rp2)
          rp1oa = opponentsAround rp1 (bp1,bp2)
          rp2oa = opponentsAround rp2 (bp1,bp2)
          bpsp = getSurroundingsPoints ((p1id,p1f),(p2id,p2f)) (bp1Neighbors,bp2Neighbors)
          rpsp = getSurroundingsPoints ((p3id,p3f),(p4id,p4f)) (rp1Neighbors,rp2Neighbors)
          bpnop = (nearOpponentPoints bp1oa) + (nearOpponentPoints bp2oa)
          rpnop = (nearOpponentPoints rp1oa) + (nearOpponentPoints rp2oa)