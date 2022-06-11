{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main(main) where

-- base
import           Data.Bool             (bool)
import           Data.List             (intercalate)
import           Prelude               hiding (Either (..))
import qualified Prelude               as P
import           System.Environment    (lookupEnv)
import           System.IO.Error       (tryIOError)

-- directory
import           System.Directory      (createDirectoryIfMissing)

-- filepath
import           System.FilePath.Posix ((</>))

-- optparse-applicative
import qualified Options.Applicative   as Opt

-- random
import           System.Random         (getStdGen)

-- vty
import           Graphics.Vty

-- infinisweep
import           Sweeper.Game
import           Sweeper.Grid

{-# ANN module ("HLint: ignore Use head") #-}
-- we often use "palette !! x" for some x

optionsParser :: Opt.Parser Options
optionsParser = Options
  <$> Opt.switch
    (Opt.short 'a' <> Opt.long "auto-open" <> Opt.help "Open cells automatically (as per flags/markers)")
  <*> Opt.option Opt.auto
    (Opt.short 'd' <> Opt.long "density" <> Opt.help "Density of the minefield, as a percentage" <> Opt.value 20 <> Opt.metavar "PERCENT")

showGrid :: GameState -> Panel -> Image
showGrid gamestate ~(Cartesian left top, Cartesian right bottom) =
    vertCat
        [ horizCat
            [ showCell gamestate (Cartesian x y)
            | x <- [left..right]
            ]
        | y <- [top..bottom]
        ]

showCell :: GameState -> Position -> Image
showCell GameState{grid, playState} pos = showCell' currentCell
    where
        currentCell :: Cell
        currentCell = getCell pos grid

        showCell' :: Cell -> Image
        showCell' (Empty True)
            | playState == Dead = drawMine
        showCell' (Mark _)      = char (markerColor playState currentCell) '#'
        showCell' (Visible t)   =
            char
                (defAttr `withForeColor` (numColor !! t))
                (if t == 0 then '-' else head $ show t)
        showCell' _             = char defAttr ' '

        numColor :: [Color]
        numColor =
          [ blue          -- 0
          , white         -- 1
          , yellow        -- 2
          , green         -- 3
          , cyan          -- 4
          , magenta       -- 5
          , brightBlue    -- 6
          , brightGreen   -- 7
          , brightMagenta -- 8
          ]

        drawMine :: Image
        drawMine = char (defAttr `withForeColor` red) 'X'

        markerColor :: PlayState -> Cell -> Attr
        markerColor Dead c | not (isMine c) = defAttr `withForeColor` yellow
        markerColor _    _ = defAttr `withForeColor` red

-- Highscore file path depends on the options
-- This uses the XDG spec to determine the location of the data directory
getHighscorePath :: Options -> IO FilePath
getHighscorePath Options{autoOpen, density} = do
  let fileName = concat
          [ "highscore_"
          , bool "" "auto_" autoOpen
          , show density
          ]

  dataDir <- lookupEnv "XDG_DATA_HOME" >>= \case
      Just dataHome -> return dataHome
      Nothing ->
          lookupEnv "HOME" >>= \case
              Just home -> return (home </> ".local" </> "share")
              Nothing -> error $ unlines
                  [ "Unable to set path for highscore file."
                  , "One of $XDG_DATA_HOME or $HOME needs to be set."
                  ]

  let infinisweepDataDir = dataDir </> "infinisweep"
  createDirectoryIfMissing True infinisweepDataDir

  return (infinisweepDataDir </> fileName)

readHighscore :: Options -> IO Score
readHighscore options = do
    highscorePath <- getHighscorePath options
    strOrExc <- tryIOError $ readFile highscorePath
    let
        getScore :: [String] -> Score
        getScore []    = 0
        getScore (x:_) = read $ last $ words x

        highscore = case strOrExc of
            P.Left  _        -> 0
            P.Right contents -> getScore $ lines contents

    return highscore

writeHighscore :: Options -> Score -> IO ()
writeHighscore options score = do
  highscorePath <- getHighscorePath options
  writeFile highscorePath (show score)

main :: IO ()
main = do
    gen  <- getStdGen
    options <- Opt.execParser $ Opt.info (Opt.helper <*> optionsParser) Opt.fullDesc
    !highscore <- readHighscore options -- get the saved highscore

    cfg <- standardIOConfig
    vty <- mkVty cfg

    -- Start the UI and the mainloop
    -- get the new highscore
    new_highscore <- doUpdate vty (createGameState gen options highscore)

    -- save the new highscore
    writeHighscore options new_highscore
    shutdown vty

-- Mainloop
-- Update the UI
doUpdate :: Vty -> GameState -> IO Score
doUpdate vty g@GameState{position = ~(Cartesian x y), score, highscore, playState, options} = do
    (displayWidth, displayHeight) <- displayBounds (outputIface vty)

    let sizeX = toInteger displayWidth
        sizeY = toInteger displayHeight

        topLeft@(Cartesian left top) = Cartesian (x - (sizeX `div` 2)) (y - (sizeY `div` 2))
        bottomRight = Cartesian (left + sizeX - 1) (top + sizeY - 3)
        panel = (topLeft, bottomRight)

        image = vertCat
            [ showGrid g panel
            , string (defAttr `withForeColor` yellow) (replicate displayWidth '─')
            , string (defAttr `withForeColor` blue) $ take displayWidth $
              intercalate " | " (
                  prettyShow options ++
                  case playState of
                      Alive -> ["Score: " ++ show score]
                      Dead  -> ["Game over! Your score is: " ++ show score, "Highscore is: " ++ show highscore]
                  )
              ++ repeat ' '
            ]

        picture = Picture
            { picCursor = AbsoluteCursor (displayWidth `div` 2) (displayHeight `div` 2)
            , picLayers = [image]
            , picBackground = ClearBackground
            }

    update vty picture
    inputUpdate vty g

-- Take keyboard inputs and update GameState
inputUpdate :: Vty -> GameState -> IO Score
inputUpdate vty g = do
    event <- nextEvent vty
    case stepGameWorld event g of
        Nothing -> pure (highscore g)
        Just g' -> doUpdate vty g'

-- Handle keyboard inputs on the current GameState and update the GameState accordingly
stepGameWorld :: Event -> GameUpdate
stepGameWorld (EvKey key _)
    | quit key          = const Nothing
    | restart key       = pure . newGame
    | moveUp key        = makeMove Up
    | moveDown key      = makeMove Down
    | moveLeft key      = makeMove Left
    | moveRight key     = makeMove Right
    | moveUpLeft key    = makeMove UpLeft
    | moveUpRight key   = makeMove UpRight
    | moveDownLeft key  = makeMove DownLeft
    | moveDownRight key = makeMove DownRight
    | placeMarkerK key  = placeMarker
    | clickCellK key    = clickCell
    | otherwise = pure
  where
    quit = \case
        KChar c | c `elem` "qQ" -> True
        _                       -> False

    restart = \case
        KChar c | c `elem` "rR" -> True
        _                       -> False

    moveUp = \case
        KChar c | c `elem` "wWkK8" -> True
        KUp                        -> True
        _                          -> False

    moveDown = \case
        KChar c | c `elem` "sSjJ2" -> True
        KDown                      -> True
        _                          -> False

    moveLeft = \case
        KChar c | c `elem` "aAhH4" -> True
        KLeft                      -> True
        _                          -> False

    moveRight = \case
        KChar c | c `elem` "dDlL6" -> True
        KRight                     -> True
        _                          -> False

    moveUpLeft = \case
        KChar c | c `elem` "yY7" -> True
        KUpLeft                  -> True
        _                        -> False

    moveUpRight = \case
        KChar c | c `elem` "uU9" -> True
        KUpRight                 -> True
        _                        -> False

    moveDownLeft = \case
        KChar c | c `elem` "bB1" -> True
        KDownLeft                -> True
        _                        -> False

    moveDownRight = \case
        KChar c | c `elem` "nN3" -> True
        KDownRight               -> True
        _                        -> False

    placeMarkerK = \case
        KChar c | c `elem` "mMeE5" -> True
        _                          -> False

    clickCellK = \case
        KChar c | c `elem` " 0" -> True
        _                       -> False
stepGameWorld _ = pure
