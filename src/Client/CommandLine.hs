{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Client.CommandLine where
    import Utils
        ( trimWhitespace, getHomeDir, maybeCase )

    import ResultST
        ( returnError,
          outputStrLn,
          outputStr,
          Error(ParseError), runResultST, liftResult, ResultST )

    import System.Console.Haskeline
        ( Settings(Settings, autoAddHistory, complete, historyFile),
          InputT,
          runInputT,
          completeFilename,
          getInputLine )

    import Control.Monad
        ( void )

    import Data.Bifunctor
        ( second )

    import Database.MongoDB
        ( Value (..) )

    import Control.Monad.State
        ( lift, get, put, liftIO )

    import Database.MongoDB.Connection 
        (Pipe)
    import Database (extractMongoAtlasCredentials, connectAtlas, atlas_host, atlas_user, atlas_password, authAtlas)

    import qualified Data.Text as T
    
    -- | The type of the command lines global state. It contains the
    -- `FilePath` to the configuration file, the parsed contents of
    -- the configuration file, and the id of the athlete being
    -- inspected.
    newtype Store = Store (FilePath, [Property], Value, Maybe Pipe)

    -- | The type of our results; that is, the return type of all
    -- computations requiring the global state and error tracking.
    type Result a = ResultST Store a

    -- | The initial store. All values are set to their types
    -- respective "empty" value except for the file path which can be
    -- passed in.
    initStore :: FilePath -> Store
    initStore fp = Store (fp,[],Null,Nothing)

    -- | Returns the path to the configuration file from the global
    -- store.
    getConfigFilePath :: Result FilePath
    getConfigFilePath = do Store (fp,_,_,_) <- get
                           return fp

    -- | Replaces the path to the configuration file in the global
    -- store with the given file path.
    putConfigFilePath :: FilePath -> Result ()
    putConfigFilePath fp = do Store (_,c,v,p) <- get
                              put $ Store (fp,c,v,p)

    -- | Returns the parsed contents of the configuration file saved
    -- in the global store.
    getConfig :: Result Config
    getConfig = do Store (_,conf,_,_) <- get
                   return conf

    -- | Replaces the parsed contents of the configuration file in the
    -- global store with the given `Config`.
    putConfig :: Config -> Result ()
    putConfig conf = do Store (f,_,v,p) <- get
                        put $ Store (f,conf,v,p)

    -- | Returns the athlete id saved in the global store.
    getValue :: Result Value
    getValue = do Store (_,_,aid,_) <- get
                  return aid

    -- | Replaces the connection pipe in the global store with the
    -- given `Pipe`.
    putPipe :: Pipe -> Result ()
    putPipe pipe = do Store (f,c,v,_) <- get
                      put $ Store (f,c,v,Just pipe)

    -- | Returns the athlete id saved in the global store.
    getPipe :: Result (Maybe Pipe)
    getPipe = do Store (_,_,_,pipe) <- get
                 return pipe

    data Command =
          Quit                  -- ^ The quit command.
        | ShowConfig            -- ^ The command for printing the 
                                -- configuration file to the user.
        | SetFirstName String   -- ^ The command for setting the first 
                                -- name of the athlete being
                                -- inspected.
        | SetLastName String    -- ^ The command for setting the last
                                -- name of the athlete being
                                -- inspected.
        | SetEmail String       -- ^ The command for setting the email 
                                -- of the athlete being inspected.
        | SetConnection String  -- ^ The command for setting the 
                                -- connection string for MongoDB 
                                -- Atlas.
        deriving (Show)

    -- | Parses a `Command` from a `String`. 
    -- A parse error is indicated by `Nothing`.
    parseCmd :: String -> Maybe Command
    parseCmd "quit" = Just Quit
    parseCmd ('s':'e':'t':' ':rest) =
        mkSetCmd . (second trimWhitespace) $
            break (== ' ') $ trimWhitespace rest
        where
            mkSetCmd ("firstname", f)  = Just $ SetFirstName f
            mkSetCmd ("lastname", l)   = Just $ SetLastName l
            mkSetCmd ("email", e)      = Just $ SetEmail e
            mkSetCmd ("connection", s) = Just $ SetConnection s
            mkSetCmd _                 = Nothing
    parseCmd ('s':'h':'o':'w':' ':rest) =
        mkShowCmd . trimWhitespace $ rest
        where
            mkShowCmd "config" = Just ShowConfig
            mkShowCmd _        = Nothing
    parseCmd _ = Nothing

    -- | Returns the parameter of a `Command` that has one.
    cmdValue :: Command -> String
    cmdValue (SetFirstName f)  = f
    cmdValue (SetLastName l)   = l
    cmdValue (SetEmail e)      = e
    cmdValue (SetConnection s) = s
    cmdValue _                 = ""

    data Property =
          FirstName String         -- ^ The firstname property of the 
                                   -- athlete being inspected stored in the
                                   -- configuration file.
        | LastName String          -- ^ The lastname property of the 
                                   -- athlete being inspected stored in the
                                   -- configuration file.
        | Email String             -- ^ The email property of the 
                                   -- athlete being inspected stored in the
                                   -- configuration file.
        | AthleteID String         -- ^ The athlete-id property of the 
                                   -- athlete being inspected stored in the
                                   -- configuration file.
        | ConnectionString String  -- ^ The MongoDB Atlas connection string.        
        deriving (Show)

    -- | The type of a configuration.
    type Config = [Property]

    -- | Returns the property key of a `Property`.
    propKey :: Property -> String
    propKey (FirstName _) = "firstname"
    propKey (LastName _)  = "lastname"
    propKey (Email _)     = "email"
    propKey (AthleteID _) = "athleteid"
    propKey (ConnectionString _) = "connection"

    -- | Converts a key and a value into a `Property`.
    mkProp :: String -> String -> Maybe Property
    mkProp "firstname"  v = Just . FirstName $ v
    mkProp "lastname"   l = Just . LastName $ l
    mkProp "email"      e = Just . Email $ e
    mkProp "athleteid"  i = Just . AthleteID $ i
    mkProp "connection" s = Just . ConnectionString $ s
    mkProp _           _ = Nothing

    -- | Looks up the value of a `Property` in a configuration using
    -- the given key.
    lookupProp :: String -> Config -> Maybe String
    lookupProp _ [] = Nothing
    lookupProp key (p:_) | key == propKey p = Just . propValue $ p
    lookupProp key (_:ps) = lookupProp key ps

    -- | Returns the value of a property.
    propValue :: Property -> String
    propValue (FirstName f)        = f
    propValue (LastName l)         = l
    propValue (Email e)            = e
    propValue (AthleteID i)        = i
    propValue (ConnectionString s) = s

    -- | Replaces a property with the given ones value in a
    -- configuration if it is already set, otherwise adds it to the
    -- given configuration.
    setProp :: Property -> [Property] -> [Property]
    setProp (FirstName new) ((FirstName _):ps) 
        = (FirstName new):ps
    setProp (LastName new) ((LastName _):ps)   
        = (LastName new):ps
    setProp (Email new) ((Email _):ps)         
        = (Email new):ps
    setProp (AthleteID new) ((AthleteID _):ps) 
        = (AthleteID new):ps
    setProp (ConnectionString new) ((ConnectionString _):ps) 
        = (ConnectionString new):ps
    setProp prop (p:ps) 
        = p:setProp prop ps
    setProp prop [] 
        = [prop]

    -- | Converts a configuration into a string. Each property is
    -- converted into:
    --
    -- key: value
    --
    -- with each property of the configuration on its own line.
    propsToString :: Config -> String
    propsToString =
        foldr (\p r -> propKey p++": " ++ propValue p ++ "\n" ++ r) ""

    -- | The main entry point into the command line.
    --
    -- Initializes the main loop, but before it can be executed we
    -- must setup Haskline and initialize the global store.
    cmdLine :: IO ()
    cmdLine = do filePathsM <- mkConfigFilePath
                 case filePathsM of
                     Left _ ->
                        putStrLn $ "CmdLine:Fatal: Failed to find" ++
                                   " the home directory."
                     Right (homedir,configFilePath) -> do
                         let settings' = settings homedir
                         let ml = runInputT settings' mainLoop
                         let store = initStore configFilePath
                         void $ runResultST ml store
        where
            settings homedir =
                let histFile = "/.tdr.trainingdays.history"
                in Settings {
                       complete       = completeFilename,
                       historyFile    = Just $ homedir ++ histFile,
                       autoAddHistory = True
                    }

    -- | The command lines main loop.
    --
    -- Before executing the main loop we parse in the configuration
    -- file and save it in the global store.
    mainLoop :: InputT (ResultST Store) ()
    mainLoop = (lift readConfFile) >> do
           conf <- lift getConfig
           -- Authentication:
           let mConStr = lookupProp "connection" conf
           maybeCase mConStr
            (lift . outputStrLn $ "No connection string set in the configuration file.")
            (\conStr -> do creds <- lift . liftResult $ extractMongoAtlasCredentials (T.pack conStr)
                           pipe <- lift. liftResult $ connectAtlas (atlas_host creds)
                           lift . putPipe $ pipe
                           logged_in <- liftIO $ authAtlas (atlas_user creds) (atlas_password creds) pipe
                           if logged_in 
                           then do -- We are authenticated, show prompt:
                                  minput <- getInputLine "trd> "
                                  maybeCase minput mainLoop $ \input ->
                                      do let i = trimWhitespace input
                                         if i == ""
                                         then mainLoop
                                         else do 
                                             let cmdM = parseCmd i
                                             let errorMsg = "Unrecognized command: "
                                             maybeCase cmdM
                                                 ((lift . outputStrLn $ errorMsg ++ i) >> mainLoop)
                                                handleCommand                                   
                                  else lift . outputStrLn $ "Failed to authenticate with the database.")           
      where
        handleCommand :: Command -> InputT (ResultST Store) ()
        handleCommand Quit       = return ()
        handleCommand ShowConfig = do
            lift handleShowConfig
            mainLoop
        handleCommand (SetFirstName f) = do
            lift $ writePropToConfFile (FirstName f)
            mainLoop
        handleCommand (SetLastName l) = do
            lift $ writePropToConfFile (LastName l)
            mainLoop
        handleCommand (SetEmail e) = do
            lift $ writePropToConfFile (Email e)
            mainLoop
        handleCommand (SetConnection s) = do
            lift $ writePropToConfFile (ConnectionString s)
            mainLoop

        handleShowConfig :: Result ()
        handleShowConfig = 
            do filePath <- getConfigFilePath
               conf <- getConfig
               outputStrLn $ "Configuration File: " ++ filePath
               outputStr $ propsToString conf

    -- | Returns the path to the users home directory and
    -- configuration file.
    mkConfigFilePath :: IO (Either Error (FilePath,FilePath))
    mkConfigFilePath = either Left paths <$> getHomeDir
      where
        configFileName = "/.trainingdays"
        paths hd = Right (hd,hd ++ configFileName)

    -- | Writes the given property to the configuration file. If it is
    -- already set then the value is updated, otherwise the property
    -- is added.
    -- 
    -- This does write the change to the configuration file on disk.
    writePropToConfFile :: Property -> Result ()
    writePropToConfFile prop = do conf <- getConfig
                                  let newConfig = setProp prop conf
                                  putConfig newConfig
                                  writeConfFile
                                  return ()

    -- | Writes the configuration stored in the global store to the
    -- configuration file on disk.
    writeConfFile ::  Result ()
    writeConfFile = do confFilePath <- getConfigFilePath
                       config <- getConfig
                       let contents = propsToString config
                       liftIO $ writeFile confFilePath contents
                       return ()

    -- | Reads the configuration file on disk, parses its contents,
    -- and stores the configuration in the global store.
    readConfFile :: Result ()
    readConfFile = do confFilePath <- getConfigFilePath
                      contents <- liftIO $ readFile confFilePath
                      let contentsLines = lines contents
                      props <- parseLines contentsLines
                      putConfig props
        where
            parseLines :: [String] -> Result [Property]
            parseLines [] = return []
            parseLines (l:ls) = 
              do let (key, rest) = break (== ':') l
                 let value = drop 2 rest
                 conf <- parseLines ls
                 let propM = mkProp key value
                 maybeCase propM 
                   (returnError . ParseError $ 
                        "incorrect property key "++
                        key++"of value "++value)
                   (return . (:conf))

