{-# LANGUAGE CPP #-}

module Player
    ( bot
    ) where

import System.IO
import Control.Monad

import BotRunner

runMain :: IO ()
runMain = runBot True bot

bot :: Bot
bot readLine writeLine = do
    -- Survive the wrath of Kutulu
    -- Coded fearlessly by JohnnyYuge & nmahoude (ok we might have been a bit scared by the old god...but don't say anything)

{-
The CG_ARENA identifier is automatically defined (with the value 1)
when packaging the source code with the SourcePackager module.
-}
#ifndef CG_ARENA
    hPutStrLn stderr "Two bots enter; one bot leaves!"
#endif

    input_line <- readLine
    let width = read input_line :: Int
    input_line <- readLine
    let height = read input_line :: Int
    
    replicateM height $ do
        line <- readLine
        return ()
    input_line <- readLine
    let input = words input_line
    let sanitylosslonely = read (input!!0) :: Int -- how much sanity you lose every turn when alone, always 3 until wood 1
    let sanitylossgroup = read (input!!1) :: Int -- how much sanity you lose every turn when near another player, always 1 until wood 1
    let wandererspawntime = read (input!!2) :: Int -- how many turns the wanderer take to spawn, always 3 until wood 1
    let wandererlifetime = read (input!!3) :: Int -- how many turns the wanderer is on map after spawning, always 40 until wood 1
    
    -- game loop
    forever $ do
        input_line <- readLine
        let entitycount = read input_line :: Int -- the first given entity corresponds to your explorer
        
        replicateM entitycount $ do
            input_line <- readLine
            let input = words input_line
            let entitytype = input!!0
            let id = read (input!!1) :: Int
            let x = read (input!!2) :: Int
            let y = read (input!!3) :: Int
            let param0 = read (input!!4) :: Int
            let param1 = read (input!!5) :: Int
            let param2 = read (input!!6) :: Int
            return ()
        
        -- hPutStrLn stderr "Debug messages..."
        
        -- MOVE <x> <y> | WAIT
        writeLine "WAIT"
