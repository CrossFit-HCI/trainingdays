{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Client.CommandLine where
    import System.Console.Haskeline
    import Control.Monad.IO.Class (liftIO)
    import Data.Char (isSpace)
    import Data.List (dropWhileEnd)
    import System.Posix.Files (fileExist)
    import Control.Exception (tryJust)
    import System.IO.Error (isDoesNotExistError)
    import System.Directory (getHomeDirectory)
    import Control.Monad.Trans (MonadIO)
    import Data.Functor ((<&>))
    import Control.Monad (guard)

    trimWhitespace :: String -> String
    trimWhitespace = (dropWhileEnd isSpace) . (dropWhile isSpace)

    data Command =
          Quit
        | SetFirstName String
        | SetLastName String
        | SetEmail String
        deriving (Show)

    cmdToString :: Command -> String
    cmdToString Quit = "quit"
    cmdToString (SetFirstName _) = "set firstname"
    cmdToString (SetLastName _) = "set lastname"
    cmdToString (SetEmail _) = "set email"

    mkCmd :: String -> String -> Maybe Command
    mkCmd "quit" _ = Just Quit
    mkCmd "set firstname" f = Just $ SetFirstName f
    mkCmd "set lastname" l = Just $ SetLastName l
    mkCmd "set email" e = Just $ SetEmail e
    mkCmd _ _ = Nothing

    cmdValue :: Command -> String
    cmdValue (SetFirstName f) = f
    cmdValue (SetLastName l) = l
    cmdValue (SetEmail e) = e
    cmdValue _ = ""

    data Property =
          FirstName String
        | LastName String
        | Email String
        | AthleteID String
        deriving (Show)

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
    setProp prop (_:ps) = setProp prop ps
    setProp prop [] = [prop]

    propsToString :: [Property] -> String
    propsToString = foldr (\p r -> propKey p++": " ++ propValue p ++ "\n" ++ r) ""

    data Error = FileDoesNotExist | ParseError String
        deriving Show

    newtype Result a = Result (IO (Either Error a))

    runResult :: Result a -> IO (Either Error a)
    runResult (Result a) = a

    lift :: IO a -> Result a
    lift am = Result (am <&> Right)

    result :: (a -> (Result b)) -> (Result a) -> (Result b)
    result comp (Result x) = Result $ x >>= (either (return . Left) (runResult . comp))

    returnResult :: a -> Result a
    returnResult = Result . return . Right

    returnError ::Error -> (Result a)
    returnError = Result . return . Left
    instance MonadIO Result where
      liftIO = lift

    instance Functor Result where
        fmap f (Result am)= Result $ am >>= (either (return . Left) (return . Right . f))

    instance Applicative Result where
        pure = returnResult
        (<*>) (Result fm) (Result am) = Result $ fm >>= (\fe -> am >>= (\ae -> return ((\f -> (Right . f) =<< ae) =<< fe)))

    instance Monad Result where
        x >>= f = result f x

    main :: IO ()
    main = do
        confM <- runResult readConfFile
        either print (runInputT defaultSettings . loop) confM
        where
            loop :: [Property] -> InputT IO ()
            loop conf = do
                minput <- getInputLine "% "
                case minput of
                    Nothing -> loop conf
                    Just input -> do let i = trimWhitespace input
                                     if i == ""
                                     then loop conf
                                     else do let cmdM = parseCommand i
                                             outputStrLn $ "Parsed Input: " ++ show cmdM
                                             case cmdM of
                                                 Just cmd -> do r <- liftIO $ runResult (handleCommand conf cmd)
                                                                case r of
                                                                    Left e -> liftIO . putStrLn $ "Error: "++(show e)
                                                                    Right conf' -> loop conf'
                                                 Nothing -> do liftIO . putStrLn $ "Unrecognized command: " ++ i
                                                               loop conf

    handleCommand :: [Property] -> Command -> Result [Property]
    handleCommand conf Quit             = return conf
    handleCommand conf (SetFirstName f) = writePropToConfFile conf (FirstName f)
    handleCommand conf (SetLastName l)  = writePropToConfFile conf (LastName l)
    handleCommand conf (SetEmail e)     = writePropToConfFile conf (Email e)

    parseCommand :: String -> Maybe Command
    parseCommand (':':rest) = mkCmd cmd value
        where
            (cmd',value') = break (== ' ') rest
            (cmd,value) = (trimWhitespace cmd',trimWhitespace value')
    parseCommand _ = Nothing

    getHomeDir :: Result String
    getHomeDir = do r <- liftIO $ tryJust (guard . isDoesNotExistError) getHomeDirectory
                    case r of
                        Left _ -> returnError FileDoesNotExist
                        Right h -> returnResult h

    configFile :: Result FilePath
    configFile = do homedir <- getHomeDir
                    let filePath = homedir ++ "/.trainingdays"
                    e <- liftIO $ fileExist filePath
                    if e
                    then return filePath
                    else returnError FileDoesNotExist

    writePropToConfFile :: [Property] -> Property -> Result [Property]
    writePropToConfFile conf prop = do let newProps = setProp prop conf
                                       let newConfig = propsToString newProps
                                       writeConfFile newConfig
                                       return newProps

    writeConfFile :: String -> Result ()
    writeConfFile contents = do conf <- configFile
                                liftIO $ writeFile conf contents
                                return ()

    readConfFile :: Result [Property]
    readConfFile = do conf <- configFile
                      contents <- liftIO $ readFile conf
                      let contentsLines = lines contents
                      props <- parseLines contentsLines
                      returnResult props
        where
            parseLines :: [String] -> Result [Property]
            parseLines [] = return []
            parseLines (l:ls) = do let (key, rest) = break (== ':') l
                                   let value = drop 2 rest
                                   conf <- parseLines ls
                                   case mkProp key value of
                                        Nothing -> returnError . ParseError $ "incorrect property key "++key++"of value "++value
                                        Just prop -> return $ prop : conf

