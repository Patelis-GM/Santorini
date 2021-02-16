module StudentCode where

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


data State = State {  gameBoard::Board,
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
  where initialState = State finalBoard False (rbp1,rbp2) (rrp1,rrp2) 'B' []
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
screenshotGame (Game ((State _ winnerFound bluePlayerPositions redPlayerPositions currentPlayer buildingsList)) _ _) = (mWinnerFound,player,mBluePlayerPositions,mRedPlayerPositions,mBuildingsList)
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
tryMove (Game ((State board winner bluePositions redPositions currentPlayer buildingsList)) undoStack redoStack) (cp,np,bp)
  | validMove == False || winner == True = (Game ((State board winner bluePositions redPositions currentPlayer buildingsList)) undoStack redoStack)
  | otherwise = (Game newState newUndoStack [])
    where validMove = possibleMove currentPlayer (cp,np,bp) ((cbp,cbtf),(nbp,nbtf),(bbp,bbtf))
          ((cbp,cbtf),(nbp,nbtf),(bbp,bbtf)) = positionsToBlocks board (cp,np,bp)
          newCurrentBlock = (0,cbtf)
          newNextBlock =  (cbp,nbtf)
          newBuildingBlock = (0,(bbtf + 1))
          boardOne = setBlock board cp newCurrentBlock
          boardTwo = setBlock boardOne np newNextBlock
          newBoard = setBlock boardTwo bp newBuildingBlock
          newCurrentPlayer = nextPlayer currentPlayer
          newBluePos = newPositions bluePositions (cp,np)
          newRedPos = newPositions redPositions (cp,np)
          newBuildingsList = newBuildings buildingsList bp bbtf
          winnerFound = (floorCheck nbtf) || (winnerCheck newBoard currentPlayer newBluePos newRedPos)
          newState = (State newBoard winnerFound newBluePos newRedPos newCurrentPlayer newBuildingsList)
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
availableBuildPosition board np pId
  | (p == 0 || p == pId) && tf < 4 = True
  | otherwise = False
    where (p,tf) = getBlock board np

getMoves :: Board -> Position -> Int -> [(Position, Position, Position)]
getMoves board cp pId = [(cp,np,bp) | np <- (getAdjacent cp), bp <- (getAdjacent np) , let npAvailable = availableNextPosition board cp np , let bpAvailable = availableBuildPosition board bp pId , npAvailable == True, bpAvailable == True]

possibleMoves :: Game -> [(Position, Position, Position)]
possibleMoves  (Game ((State board _ (bp1,bp2) (rp1,rp2) currentPlayer _)) undoStack redoStack)
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


boardPoints :: Position -> Int
boardPoints (x,y)
  | (x,y) == (2,2) = 20
  | (x >=1 && x <= 3) && (y >= 1 && y <=3) = 15
  | otherwise = 0

getBoardPoints :: (Position,Position) -> Int
getBoardPoints (p1,p2) = firstPoints + secondPoints
  where firstPoints = boardPoints p1
        secondPoints = boardPoints p2

floorPoints :: Int -> Int -> Int
floorPoints x oa
  | x == 1 && oa == 0 = 5
  | x == 1 && oa == 1 = 2
  | x == 1 && oa == 2 = 1
  | x == 2 && oa == 0 = 40
  | x == 2 && oa == 1 = 20
  | x == 2 && oa == 2 = 5
  | x == 3 = 10000
  | otherwise = 0

getFloorPoints :: (Block,Block) -> (Int,Int) -> Int
getFloorPoints ((_,tf1),(_,tf2)) (oab1,oab2) = firstPoints + secondPoints
  where firstPoints = floorPoints tf1 oab1
        secondPoints = floorPoints tf2 oab2

imminentWinPoints :: Int -> Int -> [Block] -> Int -> Int
imminentWinPoints _ oa [] 1
  | oa == 0 = 200
  | oa == 1 = 50
  | oa == 2 = 10
imminentWinPoints _ _ [] 2 = 1000
imminentWinPoints _ _ [] _ = 0
imminentWinPoints _ _ _ 2 = 1000
imminentWinPoints pf oa ((ap,af):bs) total
  | pf /= 2 = 0
  | pf == 2 && ap == 0 && af == 3 = imminentWinPoints pf oa bs (total + 1)
  | otherwise = imminentWinPoints pf oa bs total


getImminentWinPoints :: (Block,Block) -> ([Block],[Block]) -> (Int,Int) -> Int
getImminentWinPoints ((_,tf1),(_,tf2)) (n1,n2) (oab1,oab2)
  | firstPoints == 500 || secondPoints == 500 = 500
  | otherwise = firstPoints + secondPoints
  where firstPoints = imminentWinPoints tf1 oab1 n1 0
        secondPoints = imminentWinPoints tf2 oab2 n2 0

opponentsAround :: Bool -> Int -> [Block] -> Int
opponentsAround _ total [] = total
opponentsAround _ 2 _ = 2
opponentsAround bluePlayer total ((p,_):bs)
  | bluePlayer == True && (p == 2 || p == 4) = opponentsAround bluePlayer (total + 1) bs
  | bluePlayer == True && (p == 1 || p == 3 || p == 0) = opponentsAround bluePlayer total bs
  | bluePlayer == False && (p == 1 || p == 3) = opponentsAround bluePlayer (total + 1) bs
  | bluePlayer == False && (p == 2 || p == 4 || p == 0) = opponentsAround bluePlayer total bs


surroundingsPoints :: Int -> Int -> [Block] -> Int
surroundingsPoints _ _ [] = 0
surroundingsPoints pf oa ((_,af):bs)
  | fd == 1 && pf == 0 && oa == 0 = 5 + surroundingsPoints pf oa bs
  | fd == 1 && pf == 0 && oa == 1 = 3 + surroundingsPoints pf oa bs
  | fd == 1 && pf == 0 && oa == 2 = 1 + surroundingsPoints pf oa bs
  | fd == 1 && pf == 1 && oa == 0 = 15 + surroundingsPoints pf oa bs
  | fd == 1 && pf == 1 && oa == 1 = 13 + surroundingsPoints pf oa bs
  | fd == 1 && pf == 1 && oa == 2 = 11 + surroundingsPoints pf oa bs
  | fd == 1 && pf == 2 && oa == 0 = 20 + surroundingsPoints pf oa bs
  | fd == 1 && pf == 2 && oa == 1 = 18 + surroundingsPoints pf oa bs
  | fd == 1 && pf == 2 && oa == 2 = 16 + surroundingsPoints pf oa bs
  | fd == 2 && oa == 0 = (-5) + surroundingsPoints pf oa bs
  | fd == 2 && oa == 1 = (-10) + surroundingsPoints pf oa bs
  | fd == 2 && oa == 2 = (-15) + surroundingsPoints pf oa bs
  | fd == 3 && oa == 0 = (-20) + surroundingsPoints pf oa bs
  | fd == 3 && oa == 1 = (-25) + surroundingsPoints pf oa bs
  | fd == 3 && oa == 2 = (-30) + surroundingsPoints pf oa bs
  | fd == 0 = surroundingsPoints pf oa bs
  | otherwise = (-60) + surroundingsPoints pf oa bs
    where fd = af - pf

getSurroundingsPoints :: (Block,Block) -> ([Block],[Block]) -> (Int,Int) -> Int
getSurroundingsPoints ((_,tf1),(_,tf2)) (n1,n2) (oab1,oab2) = firstPoints + secondPoints
  where firstPoints = surroundingsPoints tf1 oab1 n1
        secondPoints = surroundingsPoints tf2 oab2 n2


evaluateState :: Turn -> Game -> Int
evaluateState player (Game ((State board _ (bp1,bp2) (rp1,rp2) _ _)) _ _)
  | player == 'B' = (bpfp - rpfp) + (bpbp - rpbp) + (bpiwp - rpiwp) + (bpsp - arpsp)
  | otherwise = (rpfp - bpfp) + (rpbp - bpbp) + (rpiwp - bpiwp) + (rpsp - abpsp)
    where bp1Neighbors = getNeighbors board bp1
          bp2Neighbors = getNeighbors board bp2
          rp1Neighbors = getNeighbors board rp1
          rp2Neighbors = getNeighbors board rp2
          bp1Block = getBlock board bp1
          bp2Block = getBlock board bp2
          rp1Block = getBlock board rp1
          rp2Block = getBlock board rp2
          bpfp = getFloorPoints (bp1Block,bp2Block) (bp1oa,bp2oa)
          rpfp = getFloorPoints (rp1Block,rp2Block) (rp1oa,rp2oa)
          bpbp = getBoardPoints (bp1Block,bp2Block)
          rpbp = getBoardPoints (rp1Block,rp2Block)
          bpiwp = getImminentWinPoints (bp1Block,bp2Block) (bp1Neighbors,bp2Neighbors) (bp1oa,bp2oa)
          rpiwp = getImminentWinPoints (rp1Block,rp2Block) (rp1Neighbors,rp2Neighbors) (rp1oa,rp2oa)
          bp1oa = opponentsAround True 0 bp1Neighbors
          bp2oa = opponentsAround True 0 bp2Neighbors
          rp1oa = opponentsAround False 0 rp1Neighbors
          rp2oa = opponentsAround False 0 rp2Neighbors
          bpsp = getSurroundingsPoints (bp1Block,bp2Block) (bp1Neighbors,bp2Neighbors) (bp1oa,bp2oa)
          rpsp = getSurroundingsPoints (rp1Block,bp2Block) (rp1Neighbors,rp2Neighbors) (rp1oa,rp2oa)
          arpsp = abs rpsp
          abpsp = abs bpsp