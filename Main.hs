--module Main(main, PongGame(..), renderIO, initialState, moveBall) where
module Main(main) where

import System.Exit     (exitSuccess)
import System.Random

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.IO.Game
import qualified SDL
import qualified SDL.Mixer as Mix

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
  , sounds :: Sounds           -- ^ A record of all game sounds.
  } deriving Show

-- | A data type indicating the play mode, either 1- or 2-player.
data PlayMode = OnePlayer
              | TwoPlayer
              deriving (Show, Eq)

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

data Sounds = Sounds
  { frPdlBounce :: Mix.Chunk    -- ^ Sound of ball hitting paddle front.
  , sdPdlBounce :: Mix.Chunk    -- ^ Sound of ball hitting paddle top/bottom.
  , topWallBounce :: Mix.Chunk  -- ^ Sound of ball hitting top wall.
  , btmWallBounce :: Mix.Chunk  -- ^ Sound of ball hitting bottom wall.
  , begin :: Mix.Chunk          -- ^ Sound when play begins.
  , bkgndMusic :: Mix.Chunk     -- ^ Background music.
  , defeat :: Mix.Chunk         -- ^ Game over (defeat) sound.
  , victory :: Mix.Chunk        -- ^ Game over (victory) sound.
  } deriving (Show, Eq)

-- | The starting state for the game of Pong.
initialState :: StdGen -> Sounds -> PongGame
initialState rg snds = Game
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
  , sounds      = snds
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
renderIO :: PongGame    -- ^ The game state to render.
         -> IO Picture  -- ^ A picture of this game state.
renderIO game =
  return $ pictures [ ball (stateOfPlay game)
                    , walls
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
-- moving the paddles, and playing sounds.
-- Ignore the ViewPort argument.  The first Float parameter is the number of
-- seconds since the last update.
updateIO :: Float -> PongGame -> IO PongGame
updateIO _ game@Game{stateOfPlay=NotBegun}   = return game   -- Do nothing
updateIO _ game@Game{stateOfPlay=Ended}      = return game   -- Do nothing
updateIO _ game@Game{stateOfPlay=Paused}     = return game   -- Do nothing
updateIO secs game
  | ballX < -fromIntegral windowWidth / 2 - ballRadius
        = do
          Mix.halt Mix.AllChannels
          Mix.play victorySnd
          return game {stateOfPlay = Ended, winner = RightPlayer}
  | ballX >  fromIntegral windowWidth / 2 + ballRadius
        = do
          Mix.halt Mix.AllChannels
          Mix.play gameOverSnd
          return game {stateOfPlay = Ended, winner = LeftPlayer}
  | otherwise
        = paddleBounce =<< wallBounce
            (movePaddles secs $ aiResponds $ moveBall secs game)
  where
    ballX       = fst (ballLoc game)
    victorySnd  = victory (sounds game)
    defeatSnd   = defeat (sounds game)
    gameOverSnd = case (mode game) of
                    OnePlayer -> defeatSnd
                    TwoPlayer -> victorySnd

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

-- | Indicates whether or not a collision with a wall has happened, and if so
-- with which wall.
data WallCollision = NoneW
                   | TopWall
                   | BtmWall

-- | Detect a collision with one of the side walls. Given position and radius of
-- the ball, return whether a collision occurred, and if so with which wall.
wallCollision :: Position -> Radius -> WallCollision
wallCollision (_,y) radius
  | y + radius >=  fromIntegral windowHeight/2 - wallThickness = TopWall
  | y - radius <= -fromIntegral windowHeight/2 + wallThickness = BtmWall
  | otherwise                                                  = NoneW

-- | Upon a wall collision, update the velocity of the ball to bounce it off the
-- wall, and if necessary play the appropriate sound.
wallBounce :: PongGame -> IO PongGame
wallBounce game = case wallCollision (ballLoc game) ballRadius of
                    NoneW   -> return game
                    TopWall -> do
                      Mix.play topWallSnd
                      return game { ballVel = reflect oldVel }
                    BtmWall -> do
                      Mix.play btmWallSnd
                      return game { ballVel = reflect oldVel }
  where
    oldVel = ballVel game
    topWallSnd = topWallBounce (sounds game)
    btmWallSnd = btmWallBounce (sounds game)
    reflect (vx,vy) = (vx, -vy)

-- | We only need to differentiate between collisions with the front of a paddle
-- or with a paddle's top/bottom.
data PdlCollision = NoneP
                  | LFront
                  | RFront
                  | TopP
                  | BtmP

-- | Detect a collision with a paddle.  Given position and radius of the ball,
-- and y positions of the paddles, return whether a paddle collision occurred,
-- and if so what type.
paddleCollision :: Position -> Radius -> Float -> Float -> PdlCollision
paddleCollision (x,y) r lPdlY rPdlY
  | lFrontCollision                = LFront
  | rFrontCollision                = RFront
  | lTopCollision || rTopCollision = TopP
  | lBtmCollision || rBtmCollision = BtmP
  | otherwise                      = NoneP
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
-- paddle, and if necessary play the appropriate sound.
paddleBounce :: PongGame -> IO PongGame
paddleBounce game = case paddleCollision (ballLoc game) ballRadius
                                         (playerL game) (playerR game) of
                      NoneP  -> return game
                      LFront -> do
                        Mix.play frPdlSnd
                        return game { ballVel = reflectR oldVel lPdlSt }
                      RFront -> do
                        Mix.play frPdlSnd
                        return game { ballVel = reflectL oldVel rPdlSt }
                      TopP   -> do
                        Mix.play sdPdlSnd
                        return game { ballVel = reflectU oldVel }
                      BtmP   -> do
                        Mix.play sdPdlSnd
                        return game { ballVel = reflectD oldVel }
  where
    oldVel   = ballVel game
    lPdlSt   = lPdlState game
    rPdlSt   = rPdlState game
    rpV      = rPdlVel game
    lpV      = lPdlVel game
    frPdlSnd = frPdlBounce (sounds game)
    sdPdlSnd = sdPdlBounce (sounds game)
    reflectR :: (Float, Float) -> PaddleState -> (Float, Float)
    reflectR (vx,vy) pdlSt = case pdlSt of
                               Still -> (abs vx, vy)
                               GoingUp -> (abs vx + 0.03*lpV, vy + 0.3*lpV)
                               GoingDown -> (abs vx + 0.03*lpV, vy - 0.3*lpV)
    reflectL (vx,vy) pdlSt = case pdlSt of
                               Still -> (-abs vx, vy)
                               GoingUp -> (-abs vx - 0.03*rpV, vy + 0.3*rpV)
                               GoingDown -> (-abs vx - 0.03*rpV, vy - 0.3*rpV)
    reflectU (vx,vy)       = (vx,  abs vy)
    reflectD (vx,vy)       = (vx, -abs vy)

-- | Respond to key events.
--
--   '2'        begins a 2-player game.
--   '1'        begins a 1-player (AI) game.
--   SPC        pauses/unpauses the game.
--   'r'        resets the game after it has ended.
--   'w'        moves the left paddle up.
--   's'        moves the left paddle down.
--   UP         moves the right paddle up.
--   DWN        moves the right paddle down.
--   ESC        aborts the game, closing the window.
handleKeysIO :: Event -> PongGame -> IO PongGame
handleKeysIO (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
handleKeysIO (EventKey (Char '1') Down _ _) game@Game{stateOfPlay=NotBegun}
  = do
    Mix.play $ begin (sounds game)
    Mix.play $ bkgndMusic (sounds game)
    return game {stateOfPlay = InPlay, mode = OnePlayer}
handleKeysIO (EventKey (Char '2') Down _ _) game@Game{stateOfPlay=NotBegun}
  = do
    Mix.play $ begin (sounds game)
    Mix.play $ bkgndMusic (sounds game)
    return game {stateOfPlay = InPlay}
handleKeysIO (EventKey (SpecialKey KeySpace) Down _ _) game@Game{stateOfPlay=InPlay}
  = do
    Mix.pause Mix.AllChannels
    return game {stateOfPlay = Paused}
handleKeysIO (EventKey (SpecialKey KeySpace) Down _ _) game@Game{stateOfPlay=Paused}
  = do
    Mix.resume Mix.AllChannels
    return game {stateOfPlay = InPlay}
-- If the game is paused, don't respond to keypresses other than SPC.
handleKeysIO _ game@Game{stateOfPlay=Paused} = return game
-- After the game has ended, allow the game to be reset.
handleKeysIO (EventKey (Char 'r') Down _ _) game@Game{stateOfPlay=Ended}
  = do
    Mix.halt Mix.AllChannels
    return $ initialState (rndGen game) (sounds game)
-- Controlling the paddles
handleKeysIO (EventKey (Char 'w') Down _ _) game@Game{mode=TwoPlayer}
  = return game { lPdlState = GoingUp }
handleKeysIO (EventKey (Char 'w') Up _ _) game@Game{mode=TwoPlayer}
  = return game { lPdlState = Still, lPdlVel = pdlMinSpeed }
handleKeysIO (EventKey (Char 's') Down _ _) game@Game{mode=TwoPlayer}
  = return game { lPdlState = GoingDown }
handleKeysIO (EventKey (Char 's') Up _ _) game@Game{mode=TwoPlayer}
  = return game { lPdlState = Still, lPdlVel = pdlMinSpeed }
handleKeysIO (EventKey (SpecialKey KeyUp) Down _ _) game
  = return game { rPdlState = GoingUp }
handleKeysIO (EventKey (SpecialKey KeyUp) Up _ _) game
  = return game { rPdlState = Still, rPdlVel = pdlMinSpeed }
handleKeysIO (EventKey (SpecialKey KeyDown) Down _ _) game
  = return game { rPdlState = GoingDown }
handleKeysIO (EventKey (SpecialKey KeyDown) Up _ _) game
  = return game { rPdlState = Still, rPdlVel = pdlMinSpeed }
-- Do nothing for all other events.
handleKeysIO _ game = return game

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

-- | Load all the game sounds from files into a Sounds record.
loadSounds :: IO Sounds
loadSounds = do
  pdlFr <- Mix.load "audio/paddle-bounce-front.wav"
  pdlSd <- Mix.load "audio/paddle-bounce-side.wav"
  topWall <- Mix.load "audio/wall-bounce-top.wav"
  btmWall <- Mix.load "audio/wall-bounce-bottom.wav"
  begin <- Mix.load "audio/begin-play.ogg"
  music <- Mix.load "audio/background-music.ogg"
  defeat <- Mix.load "audio/defeat.wav"
  victory <- Mix.load "audio/victory.ogg"
  return $ Sounds pdlFr pdlSd topWall btmWall begin music defeat victory

main :: IO ()
main = do
  rg <- newStdGen                 -- Get a random number generator
  SDL.initialize [SDL.InitAudio]  -- Setup audio via SDL

  let chunkSz = 256
    in Mix.withAudio Mix.defaultAudio chunkSz $ do
         snds <- loadSounds
         playIO window background fps (initialState rg snds)
                renderIO handleKeysIO updateIO

  SDL.quit
