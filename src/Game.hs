import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color


import Data.Maybe
import Data.List

import qualified StudentCode as Stud

-- represents where the user has clicked with his mouse
type ClickPosition = Maybe (Int, Int)

-- data types that specify a pawn's place on the board
type RowPos = Int
type ColumnPos = Int
type Position = (RowPos, ColumnPos)

-- positions for players' pawns
type PlayerPositions = (Position, Position)
type BluePlayerPositions = PlayerPositions
type RedPlayerPositions = PlayerPositions

-- types that specify a building
type Height = Int
type Building = (Height, Position)

-- a list where we keep all the active buildings of the board
type BuildingsList = [Building]

type Turn = Char
type Depth = Int
type Score = Int

-- specifies if we are playing vs AI or not
data PlayMode = AI |
                Human 
                deriving (Eq)

-- some states the app can find itself in 
data ControlState = WaitForPlacementClick | -- used for getting the first four positions where the pawns will be placed
                    WaitForChoiceClick |    -- used to get the position of the pawn that shall move
                    WaitForMoveClick |      -- used to get the landing position of the chosen pawn
                    WaitForBuildClick       -- used to get the positition where the pawn shall build
                    deriving (Eq)

-- wrapper object for student's game that allows visualization
data MyGame = MyGame {playMode :: PlayMode,                 -- human vs Ai or human vs human
                      depth :: Int,                         -- depth the AI checks
                      controlState :: ControlState,         -- sort of like a FSM, described above
                      placements :: [Position],             -- this keeps the initial placements by user before sending them to the student game
                      targetCoordinates :: ClickPosition,   -- this tells us where is the pawn that moves
                      moveCoordinates :: ClickPosition,     -- this tells us where the pawn moves to
                      studentGame :: Maybe Stud.Game}       -- this is the student's record, before we get the starting positions, we don't have it





-- boring numbers
imageWidth :: Int
imageHeight :: Int
imageWidth = 415
imageHeight = 368

imageScale :: Float
imageScale = 2.5

screenWidth :: Int
screenHeight :: Int
screenWidth = floor $ imageScale * (fromIntegral imageWidth)
screenHeight = floor $ imageScale * (fromIntegral imageHeight)

boardScale :: Float
boardScale = 0.7

boardWidth = (fromIntegral screenWidth) * boardScale
boardHeight = (fromIntegral screenHeight) * boardScale

-- numbers for squares in the grid
-- scale is 0.2 since we have 5 squares in each dimension
cellScale :: Float
cellScale = 0.2

cellHeight :: Float
cellWidth :: Float
cellHeight = boardHeight / 5.0
cellWidth = boardWidth / 5.0

cellWidthScale :: Float
cellHeightScale :: Float
cellWidthScale = cellWidth / 60.0
cellHeightScale = cellHeight / 60.0


playerWidthScale :: Float
playerHeightScale :: Float
playerWidthScale = 20.0 / 200.0
playerHeightScale = 20.0 / 200.0 


-- make a window for the GUI
-- this assumes a 1080p monitor but will fit in smaller screens also, just maybe not so well
window = InWindow "Santorini" (screenWidth, screenHeight) ((1920 - screenWidth) `div` 2, (1080 - screenHeight) `div` 2)

-- make a nice (not really) background color to fit the game
backgroundColor = makeColor 0.121 0.329 0.741 1

-- now we need to specify the initial game state, which builds on top of student's
-- start by waiting for the first 4 clicks which will place the pawns on the board
-- so, we don't have a studentGame yet, it will be created in transformGame
initialMyGame = MyGame{playMode = Human,
                       controlState = WaitForPlacementClick,
                       placements = [], 
                       targetCoordinates = Nothing, 
                       moveCoordinates = Nothing, 
                       studentGame = Nothing,
                       depth = 3}

main :: IO ()
main = do
  -- load all the images we will use
  gameBoard <- loadBMP "./images/gameBoard.bmp"
  firstFloorImage <- loadBMP "./images/firstFloor.bmp"
  secondFloorImage <- loadBMP "./images/secondFloor.bmp"
  thirdFloorImage <- loadBMP "./images/thirdFloor.bmp"
  domeImage <- loadBMP "./images/dome.bmp"
  bluePlayerImage <- loadBMP "./images/bluePlayer.bmp"
  redPlayerImage <- loadBMP "./images/redPlayer.bmp"
  -- give them to the play function and let the game begin
  play window backgroundColor 30 initialMyGame (gameAsPicture [gameBoard,
                                                               firstFloorImage,
                                                               secondFloorImage,
                                                               thirdFloorImage,
                                                               domeImage,
                                                               scale playerWidthScale playerHeightScale bluePlayerImage,
                                                               scale playerWidthScale playerHeightScale redPlayerImage]) transformGame (const id)  


-- we must define a gameToPicture function
-- we ask of the student to provide a representation of the game as such:
-- (Bool, Turn, BluePositions, RedPositions, BuildingsList)
-- Bool is a value that indicates if the game is over
-- Turn indicates which player is about to play. 'B' for blue, 'R' for red
-- BluePositions = (BluePawn1Position, BluePawn2Position)
-- same for red
-- BuildingsList is a list of where the buildings (if any) are as such:
-- [(Height, Position)]

gameAsPicture :: [Picture] -> MyGame -> Picture
gameAsPicture images myGame = pictures $ concat [boardPicture,
                                                 placementPictures,
                                                 buildingPictures,
                                                 playerPictures,
                                                 choicesPictures,
                                                 currPlayerPic,
                                                 playModePic]
 where boardPicture = [scale imageScale imageScale $ head images] -- this is the board pic
       -- these are all different images for every height
       firstFloorImage = images !! 1
       secondFloorImage = images !! 2
       thirdFloorImage = images !! 3
       domeImage = images !! 4
       -- now for the players
       bluePlayerImage = images !! 5 -- take the image for the blue player
       redPlayerImage = images !! 6 -- take the image for the red player
       -- now start actually making pictures
       choicesPictures = gameChoicesToPictures myGame  -- this is the highlights to let the user know the choice state
       
        -- these only apply when the game has started, so guard accordingly
       (playerPictures, buildingPictures, currPlayerPic)
          | controlState myGame /= WaitForPlacementClick = (playerPositionsToPictures (bluePositions, redPositions) (bluePlayerImage, redPlayerImage),
                                                            buildingsToPictures [firstFloorImage, secondFloorImage, thirdFloorImage, domeImage] buildingsAsList,
                                                            currentPlayerToPictures (bluePlayerImage, redPlayerImage) gameEnded currentPlayer)
          | otherwise = ([], [], [])

       -- grab screenshot of the game state given by student in order to draw the pics
       (gameEnded, currentPlayer, bluePositions, redPositions, buildingsAsList) = Stud.screenshotGame $ fromJust $ studentGame myGame
       -- highlights where the user has chosen to lay the pawns
       placementPictures = map (choiceToPic . Just) $ placements myGame
       -- inform about the play mode via text, also inform about depth when AI mode is on
       playModePic = [translate (- cellWidth) (0.7 * cellHeight) $ cellAdjustment 0 4 $ scale 0.1 0.1 $ Text $ "AI " ++ optionText]
          where optionText = if playMode myGame == AI then "On (depth=" ++ show (depth myGame) ++ ")" else "Off"

-- draws a pawn in the top left corner as well as a text indicating which player is playing or has won.
currentPlayerToPictures :: (Picture, Picture) -> Bool -> Turn -> [Picture]
currentPlayerToPictures (bluePic, redPic) ended player = [playerPic, endedPic]
  where playerPic
          | player == 'B' = translate (-0.6 * cellWidth) (0.7 * cellHeight) $ cellAdjustment 0 0 $ bluePic
          | otherwise = translate (-0.6 * cellWidth) (0.7 * cellHeight) $ cellAdjustment 0 0 $ redPic
        endedPic
          | ended = translate (-0.4 * cellWidth) (0.7 * cellHeight) $ cellAdjustment 0 0 $ scale 0.1 0.1 $ Text "Won"
          | otherwise = translate (-0.4 * cellWidth) (0.7 * cellHeight) $ cellAdjustment 0 0 $ scale 0.1 0.1 $ Text "Playing"


-- makes a picture for every building
buildingsToPictures :: [Picture] -> BuildingsList -> [Picture]
buildingsToPictures buildingPics = map (buildingToPicture buildingPics)

-- actually translates one building into an appropriate picture
buildingToPicture :: [Picture] -> Building -> Picture
buildingToPicture buildingPics (height, (row, col)) = cellAdjustment row col $ buildingPics !! (height - 1)

-- draws the pawns
playerPositionsToPictures :: (BluePlayerPositions, RedPlayerPositions) -> (Picture, Picture) -> [Picture]
playerPositionsToPictures (bluePlayerPos, redPlayerPos) (bluePic, redPic) = bluePics ++ redPics
  where bluePics = map (\(row, col) -> cellAdjustment row col bluePic) bluePosList
        redPics =  map (\(row, col) -> cellAdjustment row col redPic) redPosList
        bluePosList = (\(x, y) -> [x, y]) bluePlayerPos
        redPosList = (\(x, y) -> [x, y]) redPlayerPos

-- resizes and shifts a picture in order to fit specified cell
cellAdjustment :: Int -> Int -> Picture -> Picture
cellAdjustment row col pic = translate (columnPosToOffset col) (rowPosToOffset row) $ scale cellWidthScale cellHeightScale $ pic

-- draws the highlights when picking a move to make
gameChoicesToPictures :: MyGame -> [Picture]
gameChoicesToPictures myGame = [targetChoicePic, moveChoicePic]
  where targetChoicePic = choiceToPic $ targetCoordinates myGame
        moveChoicePic = choiceToPic $ moveCoordinates myGame

-- pick a nice transparent color for the choice
choiceColor = makeColor 0.325 0.792 0.933 0.7

-- drows a highlight in specified cell
choiceToPic :: Maybe (Int, Int) -> Picture
choiceToPic Nothing = Blank
choiceToPic (Just (choiceRow, choiceColumn)) = translate (columnPosToOffset choiceColumn) (rowPosToOffset choiceRow) $ color choiceColor $ rectangleSolid cellWidth cellHeight


rowPosToOffset :: Int -> Float
rowPosToOffset rowPos = (2 - floatPos) * boardHeight / 4.0
  where floatPos = fromIntegral rowPos

columnPosToOffset :: Int -> Float
columnPosToOffset columnPos = (floatPos - 2) * boardWidth / 4.0
  where floatPos = fromIntegral columnPos



transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) myGame =
    case controlState myGame of
      -- if we are still placing pawns
      WaitForPlacementClick -> 
          if length currentPlacements == 3 -- this means last placement
              -- so start playing game
              then myGame {controlState = WaitForChoiceClick, placements = [], studentGame = Just $ Stud.initializeGame bluePositions redPositions}
              -- wait for next placement
              else myGame {placements = currentPlacements ++ [clickedCoordinates]}

                where currentPlacements = placements myGame
                      clickedCoordinates = mousePosAsCellCoord mousePos

                      -- get input to initializeGame based on placements
                      bluePositions = (currentPlacements !! 0, currentPlacements !! 1)
                      redPositions = (currentPlacements !! 2, clickedCoordinates)

      -- we now know which pawn to move, look for where it lands
      WaitForChoiceClick -> myGame {controlState = WaitForMoveClick, targetCoordinates = Just $ mousePosAsCellCoord mousePos}
      -- we now know now where it lands, look for where it builds
      WaitForMoveClick -> myGame {controlState = WaitForBuildClick, moveCoordinates = Just $ mousePosAsCellCoord mousePos}
      -- since we have a full move, apply it and wait for next move
      WaitForBuildClick -> myGame {controlState = WaitForChoiceClick, targetCoordinates = Nothing, moveCoordinates = Nothing, studentGame = Just newGame}

        where -- this is the game after specified move is tried
              modifiedStudentGame = Stud.tryMove currentStudentGame moveToMake 
              -- convert info in state to move that should be tried
              moveToMake = (fromJust $ targetCoordinates myGame,
                            fromJust $ moveCoordinates myGame,
                            clickedCoordinates)
              -- the pawns that have been placed up until now
              currentPlacements = placements myGame
              -- the cell where the user clicked
              clickedCoordinates = mousePosAsCellCoord mousePos
              -- after the specified move is tried, let AI make a move on the resulting game
              gameAfterAIMove = Stud.tryMove modifiedStudentGame $ chooseMoveAB modifiedStudentGame $ depth myGame
              -- AI should play only if move was actually applied and we are playing versus it
              newGame 
                | currentPlayMode == AI && (Stud.screenshotGame modifiedStudentGame) /= (Stud.screenshotGame currentStudentGame) && not studentGameWon = gameAfterAIMove
                | otherwise = modifiedStudentGame
              -- play mode (vs AI or Human)
              currentPlayMode = playMode myGame
              -- student's game right before the move
              currentStudentGame = fromJust $ studentGame myGame
              -- check if the game was won with the move, in which case AI should not play
              (studentGameWon, _, _, _, _) = Stud.screenshotGame modifiedStudentGame

-- undo move with z
transformGame (EventKey (Char 'z') Up _ _) myGame = 
    case controlState myGame of
      WaitForPlacementClick -> myGame
      otherwise -> myGame{studentGame = fmap Stud.undoMove $ studentGame myGame}

-- redo move with y
transformGame (EventKey (Char 'y') Up _ _) myGame = 
    case controlState myGame of
      WaitForPlacementClick -> myGame
      otherwise -> myGame{studentGame = fmap Stud.redoMove $ studentGame myGame}

-- toggle play mode with a
transformGame (EventKey (Char 'a') Up _ _) myGame = 
    case playMode myGame of
      AI -> myGame {playMode = Human}
      Human -> myGame {playMode = AI}

-- increase AI search depth with +
transformGame (EventKey (Char '+') Up _ _) myGame = 
    myGame {depth = depth myGame + 1}

-- decrease AI search depth with -
transformGame (EventKey (Char '-') Up _ _) myGame = 
    myGame {depth = max 1 $ depth myGame - 1}

-- if none of the above has happened, do nothing
transformGame _ myGame = myGame


-- translates raw mouse input to clicked cell on the board
mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (rawX, rawY) = ( 4 - (floor $ (y + boardHeight * 0.5) / cellHeight),
                                     floor $ (x + boardWidth * 0.5) / cellWidth
                             )
                             where x = boardScale * rawX
                                   y = boardScale * rawY



-- uses the Minimax algorithm with alpha beta pruning to return the best move
chooseMoveAB :: Stud.Game -> Depth -> (Position, Position, Position)
chooseMoveAB game depth = bestMove
    where -- get best move by the compFunc
          bestMove = maximumBy compFunc availableMoves
          -- get all possible moves from the student
          availableMoves = Stud.possibleMoves game
          -- check which player is playing
          (_, player, _, _, _) = Stud.screenshotGame game
          -- starting values for alpha and beta
          negativeInfinity = minBound :: Int
          positiveInfinity = maxBound :: Int
          -- given two moves, compare them by the minimax values of the games to which they lead
          compFunc = (\move1 move2 -> compare (minimaxAB player (depth - 1) (negativeInfinity, positiveInfinity) False $ Stud.tryMove game move1)
                                              (minimaxAB player (depth - 1) (negativeInfinity, positiveInfinity) False $ Stud.tryMove game move2))

-- actually calculates minimax value for a given game and player
minimaxAB :: Turn -> Depth -> (Int, Int) -> Bool -> Stud.Game -> Score
-- if leaf node, evaluate it
minimaxAB player depth (_, _) maximize game 
  | (depth == 0) || (Stud.possibleMoves game == []) = Stud.evaluateState player game

-- if not leaf node, look at nodes below and calculate based on maximize (Bool)
minimaxAB player depth (alpha, beta) maximize game = calculationResult
    where -- actually apply all the possible moves to current game
          nextGames = map (Stud.tryMove game) $ Stud.possibleMoves game

          negativeInfinity = minBound :: Int
          positiveInfinity = maxBound :: Int
          startingValue 
            | maximize = negativeInfinity
            | otherwise = positiveInfinity
          -- calculate value based on games generated
          calculationResult = scanStatesAB player depth startingValue (alpha, beta) maximize nextGames


-- "scans" a level of the search tree, using alpha beta pruning
scanStatesAB :: Turn -> Depth -> Score -> (Score, Score) -> Bool -> [Stud.Game] -> Score
-- maximizing player
scanStatesAB player depth value (alpha, beta) True (gameToScan:nextGames) = if (newAlpha >= beta) then newValue else scanStatesAB player depth newValue (newAlpha, beta) True nextGames
    where newValue = max value $ minimaxAB player (depth - 1) (alpha, beta) False gameToScan
          newAlpha = max alpha newValue

-- minimizing player 
scanStatesAB player depth value (alpha, beta) False (gameToScan:nextGames) = if (newBeta <= alpha) then newValue else scanStatesAB player depth newValue (alpha, newBeta) False nextGames
    where newValue = min value $ minimaxAB player (depth - 1) (alpha, beta) True gameToScan
          newBeta = min beta newValue

-- when level is over, return most up to date value
scanStatesAB _ _ value (_, _) _ [] = value

