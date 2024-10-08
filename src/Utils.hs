{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Utils where
    
import Data.Char
    ( isSpace, isNumber )

import Data.List
    ( dropWhileEnd )

import ResultST 
    ( Error (FileDoesNotExist) )

import System.IO.Error
    ( isDoesNotExistError )

import System.Directory 
    ( getHomeDirectory )  

import Control.Monad
    ( guard )

import Control.Exception 
    ( tryJust )

import System.IO

debug :: String -> IO ()
debug message =
#ifdef DEBUG
    hPutStrLn stderr message
#else
    return ()
#endif

-- | Removes the whitespace from either end of a string.
trimWhitespace :: String -> String
trimWhitespace = (dropWhileEnd isSpace) . (dropWhile isSpace)

-- | Returns the path to the home directory.
getHomeDir :: IO (Either Error String)
getHomeDir = do 
    r <- tryJust (guard . isDoesNotExistError) getHomeDirectory
    return $ either (\_ -> Left FileDoesNotExist) Right r

-- | A recursor for maybe.
maybeCase :: Maybe a -> b -> (a -> b) -> b
maybeCase = flip $ \n -> flip (maybe n)

just :: Monad m => Maybe a -> (a -> m ()) -> m ()
just Nothing _ = return ()
just (Just x) f = f x

isDouble :: String -> Bool
isDouble s = isDouble' (break (=='.') s) 
    where
        isDouble' :: (String,String) -> Bool
        isDouble' ("", _) = False
        isDouble' (d1,"") = all isNumber d1
        isDouble' (d1,tail -> d2) = (d2 /= "") && (all isNumber d1) && (all isNumber d2)
