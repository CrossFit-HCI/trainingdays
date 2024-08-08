{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Redundant bracket" #-}

module Client.CommandLine where
    import System.Console.Haskeline
    import Control.Monad.IO.Class (liftIO)
    import Data.Char (isSpace)
    import Data.List (dropWhileEnd)
    import Control.Exception (tryJust, Exception, SomeException)
    import System.IO.Error (isDoesNotExistError)
    import System.Directory (getHomeDirectory)
    import Control.Monad.Trans (MonadIO)
    import Data.Functor ((<&>))
    import Control.Monad (guard, void)
    import Data.Bifunctor (second)
    import Database.MongoDB (Value (..))
    import Control.Monad.State.Lazy (StateT, evalStateT, MonadState (get, put))
    import Control.Monad.Trans.Class (lift)
    import Control.Monad.Catch (MonadMask(..), MonadCatch, MonadThrow (..), ExitCase (..))
    import GHC.Stack (HasCallStack)
    import Control.Monad.Catch.Pure (MonadCatch(..))

    trimWhitespace :: String -> String
    trimWhitespace = (dropWhileEnd isSpace) . (dropWhile isSpace)

    data Command =
          Quit
        | ShowConfig
        | SetFirstName String
        | SetLastName String
        | SetEmail String
        deriving (Show)

    parseCmd :: String -> Maybe Command
    parseCmd "quit" = Just Quit
    parseCmd ('s':'e':'t':' ':rest) = mkSetCmd . (second trimWhitespace) $ break (== ' ') $ trimWhitespace rest
        where
            mkSetCmd ("firstname", f) = Just $ SetFirstName f
            mkSetCmd ("lastname", l)  = Just $ SetLastName l
            mkSetCmd ("email", e)     = Just $ SetEmail e
            mkSetCmd _                = Nothing
    parseCmd ('s':'h':'o':'w':' ':rest) = mkShowCmd . trimWhitespace $ rest
        where
            mkShowCmd "config" = Just ShowConfig
            mkShowCmd _        = Nothing
    parseCmd _ = Nothing

    cmdValue :: Command -> String
    cmdValue (SetFirstName f) = f
    cmdValue (SetLastName l)  = l
    cmdValue (SetEmail e)     = e
    cmdValue _                = ""

    data Property =
          FirstName String
        | LastName String
        | Email String
        | AthleteID String
        deriving (Show)

    type Config = [Property]

    propKey :: Property -> String
    propKey (FirstName _) = "firstname"
    propKey (LastName _)  = "lastname"
    propKey (Email _)     = "email"
    propKey (AthleteID _) = "athleteid"

    mkProp :: String -> String -> Maybe Property
    mkProp "firstname" v = Just . FirstName $ v
    mkProp "lastname"  l = Just . LastName $ l
    mkProp "email"     e = Just . Email $ e
    mkProp "athleteid" i = Just . AthleteID $ i
    mkProp _           _ = Nothing

    lookupProp :: String -> [Property] -> Maybe String
    lookupProp _ [] = Nothing
    lookupProp key (p:_) | key == propKey p = Just . propValue $ p
    lookupProp key (_:ps) = lookupProp key ps

    propValue :: Property -> String
    propValue (FirstName f)  = f
    propValue (LastName l)   = l
    propValue (Email e)      = e
    propValue (AthleteID i)  = i

    setProp :: Property -> [Property] -> [Property]
    setProp (FirstName new) ((FirstName _):ps) = (FirstName new):ps
    setProp (LastName new) ((LastName _):ps)   = (LastName new):ps
    setProp (Email new) ((Email _):ps)         = (Email new):ps
    setProp (AthleteID new) ((AthleteID _):ps) = (AthleteID new):ps
    setProp prop (p:ps) = p:setProp prop ps
    setProp prop [] = [prop]

    propsToString :: [Property] -> String
    propsToString = foldr (\p r -> propKey p++": " ++ propValue p ++ "\n" ++ r) ""

    data Error = FileDoesNotExist | ParseError String | Exception SomeException | Aborted
        deriving Show

    type Store s = (FilePath, [Property], Value,s)

    emptyStore :: FilePath -> Store ()
    emptyStore fp = (fp, [], Null,())

    getConfigFilePath :: ResultST s FilePath
    getConfigFilePath = do (fp, _, _, _) <- get
                           return fp

    putConfigFilePath :: FilePath -> ResultST s ()
    putConfigFilePath fp = do (_, c, v, a) <- get
                              put (fp,c,v,a)

    getConfig :: ResultST s Config
    getConfig = do (_, conf, _, _) <- get
                   return conf

    putConfig :: Config -> ResultST s ()
    putConfig conf = do (f, _, v, a) <- get
                        put (f,conf,v,a)

    getValue :: ResultST s Value
    getValue = do (_, _, aid, _) <- get
                  return aid

    newtype ResultST s a = Result (StateT (Store s) IO (Either [Error] a))
    type Result a = ResultST () a

    runResult :: ResultST s a -> (Store s -> IO (Either [Error] a))
    runResult (Result a) = evalStateT a

    liftIOR :: IO a -> ResultST s a
    liftIOR am = Result $ liftIO am <&> Right

    result :: (a -> ResultST s b) -> ResultST s a -> ResultST s b
    result comp (Result x) = Result $ do
        r <- x
        either (return . Left) (toState . comp) r

    returnResult :: a -> ResultST s a
    returnResult = Result . return . Right

    returnError :: Error -> ResultST s a
    returnError = Result . return . Left . (:[])

    instance MonadIO (ResultST s) where
      liftIO = liftIOR

    instance Functor (ResultST s) where
        fmap f (Result am)= Result $ am <&> fmap f

    instance Applicative (ResultST s) where
        pure = returnResult
        (<*>) (Result fm) (Result am) = Result $ fm >>= (\fe -> am >>= (\ae -> return ((\f -> (Right . f) =<< ae) =<< fe)))

    instance Monad (ResultST s) where
        x >>= f = result f x

    instance MonadState (FilePath, Config, Value, s) (ResultST s) where
        get = Result $ do s <- get
                          return . Right $ s
        put s = Result $ do put s
                            return . Right $ ()

    instance MonadThrow (ResultST s) where
      throwM :: (HasCallStack, Exception e) => e -> ResultST s a
      throwM = Result . throwM

    toState :: ResultST s a -> StateT (Store s) IO (Either [Error] a)
    toState (Result r) = r

    instance MonadCatch (ResultST s) where
      catch :: (HasCallStack, Exception e) => ResultST s a -> (e -> ResultST s a) -> ResultST s a
      catch (Result r) f = Result $ catch r $ toState . f

    instance MonadMask (ResultST s) where
        mask :: HasCallStack => ((forall a. ResultST s a -> ResultST s a) -> ResultST s b) -> ResultST s b
        mask f = Result $ mask (\h -> toState $ f $ Result . h . toState)

        uninterruptibleMask :: HasCallStack => ((forall a. ResultST s a -> ResultST s a) -> ResultST s b) -> ResultST s b
        uninterruptibleMask f = Result $ uninterruptibleMask (\h -> toState $ f $ Result . h . toState)

        generalBracket :: HasCallStack => ResultST s a -> (a -> ExitCase b -> ResultST s c) -> (a -> ResultST s b) -> ResultST s (b, c)
        generalBracket r f g = Result $ do x <- generalBracket (toState r) f' g'
                                           return $ case x of
                                                        (Left e1, Left e2) -> Left $ e1 ++ e2
                                                        (Left e, _) -> Left e
                                                        (_, Left e) -> Left e
                                                        (Right r1, Right r2) -> Right (r1,r2)

            where
                f' (Right r') (ExitCaseSuccess (Right b)) = toState $ f r' $ ExitCaseSuccess b
                f' (Left e) _ = return . Left $ e
                f' (Right _) (ExitCaseSuccess (Left e)) = return . Left $ e
                f' (Right _) (ExitCaseException e) = return . Left $ [Exception e]
                f' (Right _) ExitCaseAbort = return . Left $ [Aborted]

                g' (Right r') = toState . g $ r'
                g' (Left e)  = return . Left $ e

    cmdLine :: IO ()
    cmdLine = do filePathsM <- mkConfigFilePath
                 case filePathsM of
                     Left _ -> putStrLn "CmdLine:Fatal: Failed to find the home directory."
                     Right (homedir,configFilePath) ->
                         void $ runResult (runInputT (settings homedir) mainLoop) (emptyStore configFilePath)
        where
            settings homedir = Settings {
                complete       = completeFilename,
                historyFile    = Just $ homedir ++ "/.tdr.trainingdays.history",
                autoAddHistory = True
            }

    mainLoop :: InputT (ResultST ()) ()
    mainLoop = (lift readConfFile) >> loop
        where
            loop :: InputT (ResultST ()) ()
            loop = do
                minput <- getInputLine "trd> "
                case minput of
                    Nothing -> loop
                    Just input -> do let i = trimWhitespace input
                                     if i == ""
                                     then loop
                                     else do let cmdM = parseCmd i
                                             case cmdM of
                                                 Just Quit -> return ()
                                                 Just cmd -> do _ <- lift $ handleCommand cmd
                                                                loop
                                                 Nothing -> do liftIO . putStrLn $ "Unrecognized command: " ++ i
                                                               loop

    handleCommand :: Command -> Result ()
    handleCommand Quit             = return ()
    handleCommand ShowConfig       = do filePath <- getConfigFilePath
                                        conf <- getConfig
                                        liftIO . putStrLn $ "Configuration File: " ++ filePath
                                        liftIO . putStr $ propsToString conf
                                        return ()
    handleCommand (SetFirstName f) = writePropToConfFile (FirstName f)
    handleCommand (SetLastName l)  = writePropToConfFile (LastName l)
    handleCommand (SetEmail e)     = writePropToConfFile (Email e)

    getHomeDir :: IO (Either Error  String)
    getHomeDir = do r <- liftIO $ tryJust (guard . isDoesNotExistError) getHomeDirectory
                    return $ case r of
                                Left _ -> Left FileDoesNotExist
                                Right h -> Right h

    mkConfigFilePath :: IO (Either Error (FilePath,FilePath))
    mkConfigFilePath = either Left (\hd -> Right (hd,hd ++ "/.trainingdays")) <$> getHomeDir

    writePropToConfFile :: Property -> Result ()
    writePropToConfFile prop = do conf <- getConfig
                                  let newConfig = setProp prop conf
                                  putConfig newConfig
                                  writeConfFile
                                  return ()

    writeConfFile ::  Result ()
    writeConfFile = do confFilePath <- getConfigFilePath
                       config <- getConfig
                       let contents = propsToString config
                       liftIO $ writeFile confFilePath contents
                       return ()

    readConfFile :: Result ()
    readConfFile = do confFilePath <- getConfigFilePath
                      contents <- liftIO $ readFile confFilePath
                      let contentsLines = lines contents
                      props <- parseLines contentsLines
                      putConfig props
                      return ()
        where
            parseLines :: [String] -> Result [Property]
            parseLines [] = return []
            parseLines (l:ls) = do let (key, rest) = break (== ':') l
                                   let value = drop 2 rest
                                   conf <- parseLines ls
                                   case mkProp key value of
                                        Nothing -> returnError . ParseError $ "incorrect property key "++key++"of value "++value
                                        Just prop -> return $ prop : conf

