{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Utils where
    import Data.Char 
        ( isSpace )

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
