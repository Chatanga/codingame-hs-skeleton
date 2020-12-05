{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Codingame
    ( doSubmit
    , doList
    , doRetrieve
    , doPlay
    , doReplay
    ) where

import Control.Monad ( when )
import Data.Aeson ( encode, json, fromJSON, Result(Success, Error), ToJSON(toJSON) )
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString as BS
import Data.Function ( (&) )
import Data.IORef ( newIORef, readIORef, writeIORef )
import Data.List ( findIndex, intercalate, isPrefixOf )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( (</>) )
import System.IO ( stderr, hPutStrLn )
import System.IO.Error ( catchIOError, eofErrorType, isEOFError, mkIOError )

import Codingame.WebServices
import Codingame.SourcePackager

import BotRunner
import Player

--------------------------------------------------------------------------------

-- Credentials need to be a login and a password as opposed to a Google account
-- or some other means of authentication.
credentialsFilePath = "credentials.json"

-- In case you don’t know it, a call to doSubmit with print it.
userId = 123456

-- If you don't know the exact challenge title, just enter some gibberish.
-- The first time you will use one of the commands bellow, you will get an
-- error giving to you the list of available challenges.
-- Note that if you play the ongoing challenge, if any, you must use the
-- OngoingChallenge data constructor and not provide any explicit title.
challengeSelector = PastChallengeTitle "code-of-kutulu"

-- The root source file of your bot.
sourcePath = "src/Player.hs"

-- The agents used for a game play in the Codingame IDE. Always put your bot as
-- the first agent (index 0) to make replay possible (see replay function below).
-- Note that 'PlayerId userId' is your last submitted code in the arena whereas
-- 'IdeCode' is simply the code you send to the IDE.
agents = [IdeCode, DefaultAi, DefaultAi, DefaultAi]

-- The options used for a game play in the Codingame IDE.
-- Nothing: auto game / Just ... : manual game (eg. Just "seed=-1234").
options = Nothing

-- Probably the same as the one defined in "sourcePath".
botForReplay = bot

-- A folder where downloaded game results will be stored.
storePath = "LastBattles"

--------------------------------------------------------------------------------

-- Upload your bot into the contest arena.
doSubmit :: IO ()
doSubmit = do
    source <- createMonolithicSource sourcePath
    credentials <- readCredentials credentialsFilePath
    submitToArena credentials challengeSelector source >>= \case
        Left e -> hPutStrLn stderr e
        Right agentId -> putStrLn $ "Agent ID: " ++ show agentId

-- List the last battles waged in the arena by your bot.
doList :: IO ()
doList = do
    credentials <- readCredentials credentialsFilePath
    getLastBattles credentials challengeSelector >>= \case
        Left e -> hPutStrLn stderr e
        Right battles -> do
            let showBattle Battle{..} = show battle_gameId ++ " - " ++ intercalate ", " (map showPlayer battle_players)
                showPlayer Player{..} = show player_position ++ ": " ++ player_nickname ++ " (" ++ show player_playerAgentId ++ ")"
            putStrLn $ "Completed last battles:\n" ++ intercalate "\n" (battles & filter battle_done & map showBattle)

-- Download a specific game result and save it locally.
doRetrieve :: Int -> IO ()
doRetrieve gameId = do
    credentials <- readCredentials credentialsFilePath
    getGameResult credentials challengeSelector gameId >>= \case
        Left e -> hPutStrLn stderr e
        Right gameResult -> do
            save storePath gameResult
            putStrLn $ "Ranks: " ++ show (gameResult_ranks gameResult)
            putStrLn $ "Game options: " ++ show (gameResult_refereeInput gameResult)
            putStrLn $ "Saved game result: " ++ show (gameResult_gameId gameResult)

-- Upload your bot into the Codingame IDE to run a game and save the result locally.
doPlay :: IO ()
doPlay = do
    source <- createMonolithicSource sourcePath
    credentials <- readCredentials credentialsFilePath
    playInIDE credentials challengeSelector source agents options >>= \case
        Left e -> hPutStrLn stderr e
        Right gameResult -> do
            save storePath gameResult
            putStrLn $ "Ranks: " ++ show (gameResult_ranks gameResult)
            putStrLn $ "Game options: " ++ show (gameResult_refereeInput gameResult)
            putStrLn $ "Saved game result: " ++ show (gameResult_gameId gameResult)

-- Replay a game locally by extracting the input from a given game result.
doReplay :: Int -> IO ()
doReplay gameId = load storePath gameId >>= replay botForReplay

--------------------------------------------------------------------------------

replay :: Bot -> GameResult -> IO ()
replay aBot gameResult = do
    let agentsFound = gameResult_agents gameResult
        -- The frame_agentId is actually an index, not the agent ID and
        -- this agent_index seems redundant with the actual index.
        myAgentId = case agentsFound of
            Nothing -> 0 -- No agents are provided for games played in the IDE.
            Just agents -> case findIndex ((==) userId . maybe (-1) codingamer_userId . agent_codingamer) agents of
                Nothing -> error $ "User ID not found among " ++ show (map agent_agentId agents)
                Just index -> index

        myFrames = gameResult_frames gameResult & filter ((== myAgentId) . frame_agentId)
        extractEchoedInputLinesFromFrame f = maybe
            []
            (map (drop 1) . filter (isPrefixOf escapedInputInErrorPrefix) . lines)
            (frame_stderr f)
        allInputLines = concatMap extractEchoedInputLinesFromFrame myFrames

    when (null allInputLines) (error "No input lines!")

    frameSetRef <- newIORef allInputLines

    let readLine = do
            readIORef frameSetRef >>= \case
                [] -> ioError (mkIOError eofErrorType "No more input" Nothing Nothing)
                (l:ls) -> writeIORef frameSetRef ls >> return l
        writeLine = putStrLn

    catchIOError
        (aBot readLine writeLine)
        (\e -> if isEOFError e
            then putStrLn "Terminated"
            else ioError e)

--------------------------------------------------------------------------------

save :: FilePath -> GameResult -> IO FilePath
save storePath gameResult = do
    createDirectoryIfMissing True storePath
    let filePath = storePath </> (show (gameResult_gameId gameResult) ++ ".json")
        fileContent = LBS.toStrict (encode (toJSON gameResult'))
        gameResult' = gameResult{ gameResult_frames = map patch (gameResult_frames gameResult) }
        patch frame = frame{ frame_view = "removed by codingame-hs" }
    BS.writeFile filePath fileContent
    return filePath

load :: FilePath -> Int -> IO GameResult
load storePath gameId = do
    let filePath = storePath </> (show gameId ++ ".json")
    fileContent <- BS.readFile filePath
    let gameResult = case parseOnly json fileContent of
            Left identificationError -> error identificationError
            Right value -> case fromJSON value of
                Error conversionError -> error conversionError
                Success result -> result
    return gameResult
