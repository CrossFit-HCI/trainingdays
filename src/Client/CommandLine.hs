{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Client.CommandLine where
    import Utils
        ( trimWhitespace, getHomeDir, maybeCase, just )

    import ResultST
        ( returnError,
          outputStrLn,
          outputStr,
          Error(..), runResultST, liftResult, ResultST )

    import System.Console.Haskeline
        ( Settings(Settings, autoAddHistory, complete, historyFile),
          InputT,
          runInputT,
          completeFilename,
          getInputLine )

    import Control.Monad ( void, unless )

    import Data.Bifunctor
        ( second )
    import Database.MongoDB ( Value(..), close )

    import Control.Monad.State
        ( lift, get, put, liftIO )

    import Database.MongoDB.Connection
        (Pipe)
    import Database (extractMongoAtlasCredentials, connectAtlas, atlas_host, atlas_user, atlas_password, authAtlas, selsertAthleteId, runAction, maybeValue, trainingJournalToDoc, selectInsert)

    import qualified Data.Text as T

    import System.Directory (doesFileExist)

    import System.FilePath (isExtensionOf)

    import Client.TDMLParser (parse)

    import Nutrition.MacroCalculator (macroCalculator)
    
    -- | The type of the command lines global state. It contains the `FilePath`
    -- to the configuration file, the parsed contents of the configuration file,
    -- the id of the athlete being inspected, the database connection (pipe),
    -- and a flag to toggle debugging.
    newtype Store = Store (FilePath, [Property], Value, Maybe Pipe, Bool)

    -- | The type of our results; that is, the return type of all
    -- computations requiring the global state and error tracking.
    type CmdLineResultST a = ResultST Store a

    -- | The initial store. All values are set to their types
    -- respective "empty" value except for the file path which can be
    -- passed in.
    initStore :: FilePath -> Store
    initStore fp = Store (fp,[],Null,Nothing,False)

    -- | Returns the path to the configuration file from the global
    -- store.
    getConfigFilePath :: CmdLineResultST FilePath
    getConfigFilePath = do Store (fp,_,_,_,_) <- get
                           return fp

    -- | Replaces the path to the configuration file in the global
    -- store with the given file path.
    putConfigFilePath :: FilePath -> CmdLineResultST ()
    putConfigFilePath fp = do Store (_,c,v,p,d) <- get
                              put $ Store (fp,c,v,p,d)

    -- | Returns the parsed contents of the configuration file saved
    -- in the global store.
    getConfig :: CmdLineResultST Config
    getConfig = do Store (_,conf,_,_,_) <- get
                   return conf

    -- | Replaces the parsed contents of the configuration file in the
    -- global store with the given `Config`.
    putConfig :: Config -> CmdLineResultST ()
    putConfig conf = do Store (f,_,v,p,d) <- get
                        put $ Store (f,conf,v,p,d)

    -- | Replaces the connection pipe in the global store with the
    -- given `Pipe`.
    putPipe :: Pipe -> CmdLineResultST ()
    putPipe pipe = do Store (f,c,v,_,d) <- get
                      put $ Store (f,c,v,Just pipe,d)

    -- | Returns the pipe saved in the global store.
    getPipe :: CmdLineResultST (Maybe Pipe)
    getPipe = do Store (_,_,_,pipe,_) <- get
                 return pipe

    -- | Replaces the athlete id in the global store with the
    -- given `Value`.
    putAthleteId :: Value -> CmdLineResultST ()
    putAthleteId aid = do Store (f,c,_,pipe,d) <- get
                          put $ Store (f,c,aid,pipe,d)

    -- | Returns the athlete id saved in the global store.
    getAthleteId :: CmdLineResultST (Maybe Value)
    getAthleteId = do Store (_,_,v,_,_) <- get
                      return $ maybeValue v

    -- | Replaces the debugging-mode toggle in the global store with the given
    -- `Value`.
    putDebug :: Bool -> CmdLineResultST ()
    putDebug d = do Store (f,c,aid,pipe,_) <- get
                    put $ Store (f,c,aid,pipe,d)

    -- | Returns the debugging-mode toggle saved in the global store.
    getDebug :: CmdLineResultST Bool
    getDebug = do Store (_,_,_,_,d) <- get
                  return d

    data Command =
          Quit                      -- ^ The quit command.
        | ShowConfig                -- ^ The command for printing the 
                                    -- configuration file to the user.
        | SetFirstName String       -- ^ The command for setting the first 
                                    -- name of the athlete being
                                    -- inspected.
        | SetLastName String        -- ^ The command for setting the last
                                    -- name of the athlete being
                                    -- inspected.
        | SetEmail String           -- ^ The command for setting the email 
                                    -- of the athlete being inspected.
        | SetConnection String      -- ^ The command for setting the 
                                    -- connection string for MongoDB 
                                    -- Atlas.
        | SetDebug Bool             -- ^ Toggles debugging mode. 
        | InsertTDMLFile FilePath   -- ^ The command for inserting a TDML 
                                    -- file.
        | CalculateMacros           -- ^ The command to calculate an 
                                    -- athlete's macros.
        deriving (Show)

    -- | Parses a `Command` from a `String`. 
    -- A parse error is indicated by `Nothing`.
    parseCmd :: String -> Maybe Command    
    parseCmd "quit" = Just Quit
    parseCmd "calculate macros" = Just CalculateMacros
    parseCmd ('s':'e':'t':' ':rest) =
        mkSetCmd . (second trimWhitespace) $
            break (== ' ') $ trimWhitespace rest
        where
            mkSetCmd ("firstname", f)  = Just $ SetFirstName f
            mkSetCmd ("lastname", l)   = Just $ SetLastName l
            mkSetCmd ("email", e)      = Just $ SetEmail e
            mkSetCmd ("connection", s) = Just $ SetConnection s
            mkSetCmd ("debug", "on")   = Just $ SetDebug True
            mkSetCmd ("debug", "off")  = Just $ SetDebug False
            mkSetCmd _                 = Nothing
    parseCmd ('s':'h':'o':'w':' ':rest) =
        mkShowCmd . trimWhitespace $ rest
        where
            mkShowCmd "config" = Just ShowConfig
            mkShowCmd _        = Nothing
    parseCmd ('i':'n':'s':'e':'r':'t':' ':'d':'a':'y':' ':filePath) = do
        unless (isExtensionOf ".tdml" filePath) Nothing
        return $ InsertTDMLFile filePath
    parseCmd _ = Nothing

    data CmdValue = Str String | Boolean Bool | None
        deriving Show

    -- | Returns the parameter of a `Command` that has one.
    cmdValue :: Command -> CmdValue
    cmdValue (SetFirstName f)  = Str f
    cmdValue (SetLastName l)   = Str l
    cmdValue (SetEmail e)      = Str e
    cmdValue (SetConnection s) = Str s
    cmdValue (SetDebug b)      = Boolean b
    cmdValue _                 = None

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
    lookupProp :: String -> CmdLineResultST (Maybe String)
    lookupProp prop = lookupProp' prop <$> getConfig
        where
            lookupProp' _ [] = Nothing
            lookupProp' key (p:_) | key == propKey p = Just . propValue $ p
            lookupProp' key (_:ps) = lookupProp' key ps

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
    mainLoop = do
        lift $ outputStrLn header
        lift preprocess
        innerLoop
      where
        preprocess :: ResultST Store ()
        preprocess = do
            setupConf
            mConStr <- lookupProp "connection"
            just mConStr
              (\conStr -> do loggedIn <- authWithAtlas conStr
                             if loggedIn
                             then initAthleteId
                             else outputStrLn "preprocess: Failed to authenticate with Atlas."
              )

        authWithAtlas :: String -> CmdLineResultST Bool
        authWithAtlas conStr = do
            creds <- liftResult $ extractMongoAtlasCredentials (T.pack conStr)
            pipe <- liftResult $ connectAtlas (atlas_host creds)
            putPipe pipe
            liftIO $ authAtlas (atlas_user creds) (atlas_password creds) pipe

        setupConf :: CmdLineResultST ()
        setupConf = do
            confFilePath <- getConfigFilePath
            confFileExists <- liftIO $ doesFileExist confFilePath
            if confFileExists
            then readConfFile
            else liftIO noConfFile

        initAthleteId :: ResultST Store ()
        initAthleteId = do
            pipeM <- getPipe
            maybeCase pipeM
                (outputStrLn "initAthleteId: hit an unreachable point where the pipe is not set in the state after authentication.")
                (\pipe -> do firstnameM <- lookupProp "firstname"
                             lastnameM <- lookupProp "lastname"
                             emailM <- lookupProp "email"
                             case (firstnameM, lastnameM, emailM) of
                                (Just firstname, Just lastname, Just email) ->
                                    do aid <- liftIO . runAction pipe $ selsertAthleteId firstname lastname email
                                       putAthleteId aid
                                _ -> liftIO noConfFile)

        innerLoop = do -- Show prompt:
                       minput <- getInputLine "trd> "
                       maybeCase minput innerLoop $ \input ->
                        do let i = trimWhitespace input
                           if i == ""
                           then innerLoop
                           else do let cmdM = parseCmd i
                                   let errorMsg = "Unrecognized command: "
                                   maybeCase cmdM
                                    ((lift . outputStrLn $ errorMsg ++ i) >> innerLoop)
                                    handleCommand

        handleCommand :: Command -> InputT (ResultST Store) ()
        handleCommand Quit       = do
            pipeM <- lift getPipe
            maybeCase pipeM
                (return ())
                (\pipe -> void (liftIO . close $ pipe))
        handleCommand ShowConfig = do
            lift handleShowConfig
            innerLoop
        handleCommand (SetFirstName f) = do
            lift $ writePropToConfFile (FirstName f)
            innerLoop
        handleCommand (SetLastName l) = do
            lift $ writePropToConfFile (LastName l)
            innerLoop
        handleCommand (SetEmail e) = do
            lift $ writePropToConfFile (Email e)
            innerLoop
        handleCommand (SetConnection s) = do
            lift $ writePropToConfFile (ConnectionString s)
            innerLoop
        handleCommand (SetDebug d) = do   
            lift $ putDebug d         
            innerLoop
        handleCommand (InsertTDMLFile fp) = do
            lift $ handleInsertTDMLFile fp
            innerLoop
        handleCommand CalculateMacros = do
            liftIO macroCalculator
            innerLoop

        handleInsertTDMLFile :: FilePath -> CmdLineResultST ()
        handleInsertTDMLFile fp = do            
            pipeM <- getPipe
            aidM <- getAthleteId
            maybeCase pipeM
                (returnError $ DBError "handleInsertTDMLFile: failed to get the pipe from state.")
                (\pipe -> maybeCase aidM
                            (returnError $ DBError "handleInsertTDMLFile: failed to get the athlete id from state.")
                            (\aid -> do trainingJournal <- liftIO $ parse fp
                                        let act = trainingJournalToDoc aid trainingJournal
                                        doc <- liftIO $ runAction pipe act
                                        void $ liftIO $ runAction pipe $ selectInsert (T.pack "training-journals") "athlete_id" doc))

        handleShowConfig :: CmdLineResultST ()
        handleShowConfig =
            do filePath <- getConfigFilePath
               conf <- getConfig
               outputStrLn $ "Configuration File: " ++ filePath
               outputStr $ propsToString conf

        noConfFile :: IO ()
        noConfFile = do
            putStrLn "No configuration was found."
            putStrLn $ "Please issue the following commands " ++
                       "to setup your database:\n"++
                       "set firstname Jane\n"++
                       "set lastname Doe\n"++
                       "set email jane@example.com\n"++
                       "set connection mongodb-atlas-"++
                       "connection-string"

        header :: String
        header = "Welcome to the Training Days database.\n"

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
    writePropToConfFile :: Property -> CmdLineResultST ()
    writePropToConfFile prop = do conf <- getConfig
                                  let newConfig = setProp prop conf
                                  putConfig newConfig
                                  writeConfFile
                                  return ()

    -- | Writes the configuration stored in the global store to the
    -- configuration file on disk.
    writeConfFile ::  CmdLineResultST ()
    writeConfFile = do confFilePath <- getConfigFilePath
                       config <- getConfig
                       let contents = propsToString config
                       liftIO $ writeFile confFilePath contents
                       return ()

    -- | Reads the configuration file on disk, parses its contents,
    -- and stores the configuration in the global store.
    readConfFile :: CmdLineResultST ()
    readConfFile = do confFilePath <- getConfigFilePath
                      contents <- liftIO $ readFile confFilePath
                      let contentsLines = lines contents
                      props <- parseLines contentsLines
                      putConfig props
        where
            parseLines :: [String] -> CmdLineResultST [Property]
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

