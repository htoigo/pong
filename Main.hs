module Main(main, PongGame(..), render, initialState, moveBall) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.Pure.Game
import System.Random

-- Global pong ball characteristics
ballRadius, ballSpeed :: Float
ballRadius = 10
ballSpeed  = 300

-- Global paddle characteristics
paddleWidth, paddleHeight, paddleBorder, lPdlX, rPdlX :: Float
paddleWidth  = 26
paddleHeight = 86
paddleBorder = 3
lPdlX        = -fromIntegral windowWidth / 2 + 30
rPdlX        = fromIntegral windowWidth / 2 - 30
pdlMaxSpeed  = 2200
pdlMinSpeed  = 200
pdlAccel     = 3000

-- Global wall characteristics
wallLength, wallThickness :: Float
wallLength = fromIntegral windowWidth - 30
wallThickness = 5

-- | Data describing the state of the pong game.
data PongGame = Game
  { ballLoc :: (Float, Float)  -- ^ Pong ball (x,y) location.
  , ballVel :: (Float, Float)  -- ^ Pong ball (x,y) velocity.
  , playerL :: Float           -- ^ Left player paddle position.
                               -- Zero is the middle of the screen.
  , playerR :: Float           -- ^ Right player paddle position.
  , lPdlState :: PaddleState   -- ^ Left paddle state of motion.
  , lPdlVel :: Float           -- ^ Left paddle speed.
  , rPdlState :: PaddleState   -- ^ Right paddle state of motion.
  , rPdlVel :: Float           -- ^ Right paddle speed.
  , stateOfPlay :: PlayState   -- ^ Is the game in play, paused, ended, etc.
  , winner :: Player           -- ^ Indicates which player has won.
  , mode :: PlayMode           -- ^ Whether the game is in 1- or 2-player mode.
  , rndGen :: StdGen           -- ^ Random number generator used for initial
                               --   ball direction.
  } deriving Show

-- | A data type indicating the play mode, either 1- or 2-player.
data PlayMode = OnePlayer
              | TwoPlayer
              deriving Show

-- | A data type to denote one of the players, or Neither if no player has won.
data Player = Neither
            | LeftPlayer
            | RightPlayer
            deriving Show

-- | Data describing the state of game play.  Ensures that certain states
-- are mutually exclusive, such as gamed paused and game ended.
data PlayState = NotBegun
               | InPlay
               | Paused
               | Ended
               deriving Show

-- | Data describing whether a paddle is moving or not.
data PaddleState = Still
                 | GoingUp
                 | GoingDown
                 deriving Show

-- | The starting state for the game of Pong.
initialState :: StdGen -> PongGame
initialState rg = Game
  { ballLoc     = (-20, 30)
  , ballVel     = (ballSpeed * cos dir, ballSpeed * sin dir)
  , playerL     = 0
  , playerR     = 0
  , lPdlState   = Still
  , lPdlVel     = pdlMinSpeed
  , rPdlState   = Still
  , rPdlVel     = pdlMinSpeed
  , stateOfPlay = NotBegun
  , winner      = Neither
  , mode        = TwoPlayer
  , rndGen      = rg'
  }
  where
    -- 'dir' is the initial direction of the ball, as an angle in radians
    -- measured counter-clockwise from straight right.  So zero is to the right,
    -- pi is to the left, etc.  To avoid the directions near straight up or
    -- down, we restrict the possible directions to the intervals
    --     [3*pi/4, 5*pi/4] and [7*pi/4, 9*pi/4].
    dir
      | d <= 0.5  = (3 + 4*d) * pi/4    -- [3pi/4, 5pi/4]
      | otherwise = (5 + 4*d) * pi/4    -- [7pi/4, 9pi/4]
    (d, rg') = randomR (0, 1) rg

-- | Convert a game state into a picture.
render :: PongGame    -- ^ The game state to render.
       -> Picture     -- ^ A picture of this game state.
render game =
  pictures [ ball (stateOfPlay game), walls
           , mkPaddle lBorderClr lPdlX (playerL game)
           , mkPaddle rBorderClr rPdlX (playerR game)
           , statusDisplay (stateOfPlay game) (winner game) (mode game)
           ]
  where
    -- The pong ball
    ball :: PlayState -> Picture
    ball NotBegun = blank
    ball _        = uncurry translate (ballLoc game) $ color ballColor
                    $ circleSolid ballRadius
    ballColor     = dark red

    -- The top and bottom walls
    walls = pictures [ wall (fromIntegral windowHeight/2 - wallThickness/2)
                     , wall (-fromIntegral windowHeight/2 + wallThickness/2)
                     ]
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid wallLength wallThickness
    wallColor = greyN 0.5

    -- Make a paddle of a given border and vertical offset
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid paddleWidth paddleHeight
      , translate x y $ color paddleColor
          $ rectangleSolid (paddleWidth - 2 * paddleBorder)
                           (paddleHeight - 2 * paddleBorder)
      ]
    paddleColor = light (light blue)
    lBorderClr  = mixColors 0.75 0.25 rose orange
    rBorderClr  = mixColors 0.25 0.75 red blue

    -- The status display overlay
    statusDisplay :: PlayState -> Player -> PlayMode -> Picture
    statusDisplay NotBegun _ _ = pictures [onePlyrMsg, twoPlyrMsg]
      where
        onePlyrMsg = translate (-137) 100 $ scale 0.12 0.12
                     $ color txtColor $ text "Press 1 for a single player game."
        twoPlyrMsg = translate (-124) 60 $ scale 0.12 0.12
                     $ color txtColor $ text "Press 2 for a 2-player game."
    statusDisplay Paused   _ _ = translate (-50) 0 $ scale 0.12 0.12
                                 $ color txtColor $ text "Game Paused"
    statusDisplay Ended    p m = pictures [overMsg, whoWon p m]
        where
          overMsg = translate (-40) 20 $ scale 0.12 0.12
                        $ color txtColor $ text "Game Over"
          whoWon LeftPlayer TwoPlayer = translate (-36) (-20)
                                        $ scale 0.12 0.12
                                        $ color txtColor
                                        $ text "Rose won!"
          whoWon LeftPlayer OnePlayer = translate (-52) (-20)
                                        $ scale 0.12 0.12
                                        $ color txtColor
                                        $ text "Sorry, you lost!"
          whoWon RightPlayer TwoPlayer = translate (-38) (-20)
                                         $ scale 0.12 0.12
                                         $ color txtColor
                                         $ text "Indigo won!"
          whoWon RightPlayer OnePlayer = translate (-38) (-20)
                                         $ scale 0.12 0.12
                                         $ color txtColor
                                         $ text "You won!"
    statusDisplay _ _ _ = blank
    txtColor = white

-- | Update the ball position using its current velocity.
moveBall :: Float       -- ^ The number of seconds since last update
         -> PongGame    -- ^ The initial game state
         -> PongGame    -- ^ A new game state with an updated ball position
moveBall secs game = game { ballLoc = (x', y') }
  where
    -- Old location and velocity
    (x, y) = ballLoc game
    (vx, vy) = ballVel game
    -- New location
    x' = x + vx * secs
    y' = y + vy * secs

-- | Update the paddle positions depending on their states.
movePaddles :: Float       -- ^ The number of seconds since last update.
            -> PongGame    -- ^ The last game state.
            -> PongGame    -- ^ A new game state with updated paddle positions.
movePaddles secs game@Game{lPdlState=Still, rPdlState=Still} = game
movePaddles secs game = moveRightPdl secs (moveLeftPdl secs game)

moveLeftPdl :: Float -> PongGame -> PongGame
moveLeftPdl secs game@Game{lPdlState=GoingUp}
  = game { playerL = min upperBound (playerL game + v*secs), lPdlVel = newV }
    where v          = lPdlVel game
          newV       = min pdlMaxSpeed (v + pdlAccel*secs)
          upperBound = fromIntegral windowHeight/2 - wallThickness
                       - paddleHeight/2
moveLeftPdl secs game@Game{lPdlState=GoingDown}
  = game { playerL = max lowerBound (playerL game - v*secs), lPdlVel = newV }
    where v          = lPdlVel game
          newV       = min pdlMaxSpeed (v + pdlAccel*secs)
          lowerBound = -fromIntegral windowHeight/2 + wallThickness
                       + paddleHeight/2
moveLeftPdl _ game@Game{lPdlState=Still} = game      

moveRightPdl :: Float -> PongGame -> PongGame
moveRightPdl secs game@Game{rPdlState=GoingUp}
  = game { playerR = min upperBound (playerR game + v*secs), rPdlVel = newV }
    where v          = rPdlVel game
          newV       = min pdlMaxSpeed (v + pdlAccel*secs)
          upperBound = fromIntegral windowHeight/2 - wallThickness
                       - paddleHeight/2
moveRightPdl secs game@Game{rPdlState=GoingDown}
  = game { playerR = max lowerBound (playerR game - v*secs), rPdlVel = newV }
    where v          = rPdlVel game
          newV       = min pdlMaxSpeed (v + pdlAccel*secs)
          lowerBound = -fromIntegral windowHeight/2 + wallThickness
                       + paddleHeight/2
moveRightPdl _ game@Game{rPdlState=Still} = game      

-- | Update the game by moving the ball, bouncing off walls and paddles,
-- and moving the paddles.
-- Ignore the ViewPort argument.  The first Float parameter is the number of
-- seconds since the last update.
update :: Float -> PongGame -> PongGame
update _ game@Game{stateOfPlay=NotBegun}   = game          -- Do nothing
update _ game@Game{stateOfPlay=Ended}      = game          -- Do nothing
update _ game@Game{stateOfPlay=Paused}     = game          -- Do nothing
update secs game
  | ballX < -fromIntegral windowWidth / 2 - ballRadius
        = game {stateOfPlay = Ended, winner = RightPlayer}
  | ballX >  fromIntegral windowWidth / 2 + ballRadius
        = game {stateOfPlay = Ended, winner = LeftPlayer}
  | otherwise
        = paddleBounce $ wallBounce $ movePaddles secs $ aiResponds
            $ moveBall secs game
  where
    ballX = fst (ballLoc game)

aiResponds :: PongGame -> PongGame
-- In 2-player mode, AI does nothing.
aiResponds game@Game{mode=TwoPlayer} = game
-- In 1-player mode, AI controls the left paddle.
aiResponds game
  | ballY > lPdlY + 20 = game { lPdlState = GoingUp }
  | ballY < lPdlY - 20 = game { lPdlState = GoingDown }
  | otherwise          = game { lPdlState = Still, lPdlVel = pdlMinSpeed }
  where
    (_, ballY) = ballLoc game
    lPdlY      = playerL game

type Radius = Float
type Position = (Float, Float)

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool
wallCollision (_,y) radius = topCollision || bottomCollision
  where
    topCollision    = y + radius >=  fromIntegral windowHeight/2 - wallThickness
    bottomCollision = y - radius <= -fromIntegral windowHeight/2 + wallThickness

-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    -- The old velocity
    (vx, vy) = ballVel game
    vy' = if wallCollision (ballLoc game) ballRadius
          then
            -- Update the velocity.
            -vy
          else
            -- Do nothing. Return the old velocity.
            vy

-- | We only need to differentiate between collisions with the front of a paddle
-- or with a paddle's top/bottom.
data Collision = None
               | LFront
               | RFront
               | Top
               | Bottom

-- | Detect a collision with a paddle.  Given position and radius of the ball,
-- and y positions of the paddles, return whether a paddle collision occurred,
-- and if so what type.
paddleCollision :: Position -> Radius -> Float -> Float -> Collision
paddleCollision (x,y) r lPdlY rPdlY
  | lFrontCollision                = LFront
  | rFrontCollision                = RFront
  | lTopCollision || rTopCollision = Top
  | lBtmCollision || rBtmCollision = Bottom
  | otherwise                      = None
  where
    lFrontCollision = y >= lPdlBtm && y <= lPdlTop
                      && x - r <= lPdlRight && x - r > lPdlRight - 20
    lTopCollision   = x >= lPdlLeft && x <= lPdlRight
                      && y - r <= lPdlTop && y - r > lPdlTop - 30
    lBtmCollision   = x >= lPdlLeft && x <= lPdlRight
                      && y + r >= lPdlBtm && y + r < lPdlBtm + 30
    rFrontCollision = y >= rPdlBtm && y <= rPdlTop
                      && x + r >= rPdlLeft && x + r < rPdlLeft + 20
    rTopCollision   = x >= rPdlLeft && x <= rPdlRight
                      && y - r <= rPdlTop && y - r > rPdlTop - 30
    rBtmCollision   = x >= rPdlLeft && x <= rPdlRight
                      && y + r >= rPdlBtm && y + r < rPdlBtm + 30
    lPdlTop         = lPdlY + paddleHeight / 2
    lPdlBtm         = lPdlY - paddleHeight / 2
    lPdlLeft        = lPdlX - paddleWidth / 2
    lPdlRight       = lPdlX + paddleWidth / 2
    rPdlTop         = rPdlY + paddleHeight / 2
    rPdlBtm         = rPdlY - paddleHeight / 2
    rPdlLeft        = rPdlX - paddleWidth / 2
    rPdlRight       = rPdlX + paddleWidth / 2

-- | Upon collisions, change the velocity of the ball to bounce it off the
-- paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy') }
  where
    (vx, vy)    = ballVel game    -- The old velocity
    (vx', vy')  = case paddleCollision (ballLoc game) ballRadius
                                      (playerL game) (playerR game) of
                   None   -> (vx, vy)
                   LFront -> case lPdlState game of
                               Still     -> (reflectRU vx, vy)
                               GoingUp   -> (reflectRU vx + 0.03*lpV,
                                              vy + 0.3*lpV)
                               GoingDown -> (reflectRU vx + 0.03*lpV,
                                              vy - 0.3*lpV)
                   RFront -> case rPdlState game of
                               Still     -> (reflectLD vx, vy)
                               GoingUp   -> (reflectLD vx + 0.03*rpV,
                                              vy + 0.3*rpV)
                               GoingDown -> (reflectLD vx + 0.03*rpV,
                                              vy - 0.3*rpV)
                   Top    -> (vx, reflectRU vy)
                   Bottom -> (vx, reflectLD vy)
    reflectRU v = if v < 0 then -v else v
    reflectLD v = if v > 0 then -v else v
    rpV         = rPdlVel game
    lpV         = lPdlVel game

-- | Respond to key events.
--
--   '2'   begins a 2-player game.
--   '1'   begins a 1-player (AI) game.
--   SPC   pauses/unpauses the game.
--   'r'   resets the game after it has ended.
--   'w'   moves the left paddle up.
--   's'   moves the left paddle down.
--   UP    moves the right paddle up.
--   DWN   moves the right paddle down.
handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char '1') Down _ _) game@Game{stateOfPlay=NotBegun}
  = game {stateOfPlay = InPlay, mode = OnePlayer}
handleKeys (EventKey (Char '2') Down _ _) game@Game{stateOfPlay=NotBegun}
  = game {stateOfPlay = InPlay}
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game@Game{stateOfPlay=InPlay}
  = game {stateOfPlay = Paused}
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game@Game{stateOfPlay=Paused}
  = game {stateOfPlay = InPlay}
-- If the game is paused, don't respond to keypresses other than SPC.
handleKeys _ game@Game{stateOfPlay=Paused} = game
-- After the game has ended, allow the game to be reset.
handleKeys (EventKey (Char 'r') Down _ _) game@Game{stateOfPlay=Ended}
  = initialState (rndGen game)
-- Controlling the paddles
handleKeys (EventKey (Char 'w') Down _ _) game@Game{mode=TwoPlayer}
  = game { lPdlState = GoingUp }
handleKeys (EventKey (Char 'w') Up _ _) game@Game{mode=TwoPlayer}
  = game { lPdlState = Still, lPdlVel = pdlMinSpeed }
handleKeys (EventKey (Char 's') Down _ _) game@Game{mode=TwoPlayer}
  = game { lPdlState = GoingDown }
handleKeys (EventKey (Char 's') Up _ _) game@Game{mode=TwoPlayer}
  = game { lPdlState = Still, lPdlVel = pdlMinSpeed }
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game
  = game { rPdlState = GoingUp }
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game
  = game { rPdlState = Still, rPdlVel = pdlMinSpeed }
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game
  = game { rPdlState = GoingDown }
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game
  = game { rPdlState = Still, rPdlVel = pdlMinSpeed }
-- Do nothing for all other events.
handleKeys _ game = game

screenWidth, screenHeight :: Int
windowWidth, windowHeight, windowPosX, windowPosY :: Int
screenWidth  = 1920
screenHeight = 1080
windowWidth  = 800
windowHeight = 800
windowPosX   = (screenWidth - windowWidth) `div` 2
windowPosY   = (screenHeight - windowHeight) `div` 2

window :: Display
window = InWindow "Pong" (windowWidth, windowHeight) (windowPosX, windowPosY)

background :: Color
background = black

-- | Number of frames to show per second.
fps :: Int
fps = 60

main :: IO ()
main = do
  rg <- newStdGen
  play window background fps (initialState rg) render handleKeys update
