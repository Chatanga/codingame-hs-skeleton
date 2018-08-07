module BotRunner
    ( Bot
    , escapedInputInErrorPrefix
    , runBot
    ) where

import Control.Monad
import System.IO

-- A Codingame bot where input and output have been abstracted out.
type Bot = IO String -> (String -> IO ()) -> IO ()

escapedInputInErrorPrefix :: String
escapedInputInErrorPrefix = "#"

-- Run a bot in the Codingame Arena (or IDE).
runBot
    :: Bool -- Shall the bot’s input be echoed on stderr to be able to replay any game result?
    -> Bot -- The bot.
    -> IO ()
runBot echoInput bot = do
    hSetBuffering stdout NoBuffering
    bot readLine writeLine
    where
        readLine = do
            line <- getLine
            when echoInput $
                hPutStrLn stderr (escapedInputInErrorPrefix ++ line)
            return line
        writeLine = putStrLn
