{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}
module ResultST where

    import Control.Monad.State.Lazy
        ( StateT,
          evalStateT,
          MonadState (get, put) )

    import Control.Monad.Trans 
        ( MonadIO, 
          MonadTrans )

    import Control.Monad.IO.Class
        ( liftIO )

    import Control.Monad.Catch
        ( MonadMask(..),
          MonadCatch,
          MonadThrow (..),
          ExitCase (..))

    import GHC.Stack
        ( HasCallStack )

    import Control.Monad.Catch.Pure
        ( MonadCatch(..) )

    import Control.Exception
        ( Exception,
          SomeException )

    import Data.Functor
        ( (<&>) )
    import Control.Monad.Trans.Class (lift)

    data Error = FileDoesNotExist        -- ^ File does not exist error.
               | ParseError String       -- ^ A parse error.
               | Exception SomeException -- ^ Exceptions as errors.
               | DBError String          -- ^ Database Errors.
               | Aborted                 -- ^ Thread abort error.
        deriving Show

    data Validate a
        = Invalid [Error]
        | Valid a

    validate :: Validate a -> ([Error] -> c) -> (a -> c) -> c
    validate (Valid a) _ f = f a
    validate (Invalid e) f _ = f e

    instance Functor Validate where
        fmap f (Valid a) = Valid . f $ a
        fmap _ (Invalid e) = Invalid e

    instance Applicative Validate where
        pure = Valid

        (Valid f) <*> (Valid a) = pure . f $ a
        (Invalid e1) <*> (Invalid e2) = Invalid $ e1 ++ e2
        (Invalid e) <*> _ = Invalid e
        _ <*> (Invalid e) = Invalid e

    instance Monad Validate where
        (Valid r) >>= f = f r
        (Invalid e) >>= _ = Invalid e

    newtype ResultT m a = ResultT (m (Validate a))

    type Result a = ResultT IO a

    runResultT :: ResultT m a -> m (Validate a)
    runResultT (ResultT c) = c

    throwError :: Monad m => Error -> ResultT m a
    throwError = ResultT . return . Invalid . (:[])

    instance MonadIO (ResultT IO) where
      liftIO m = ResultT $ do r <- m
                              return . pure $ r

    instance Monad m => Functor (ResultT m) where
      fmap f (ResultT aM) = ResultT $ fmap f <$> aM

    instance Monad m => Applicative (ResultT m) where
        pure = ResultT . return . Valid
        (ResultT aM) <*> (ResultT bM) = ResultT $ do
            a <- aM
            b <- bM
            return $ a <*> b

    instance Monad m => Monad (ResultT m) where
      (ResultT comp) >>= continue =
        ResultT $ do comp >>= \case
                        Valid a -> runResultT . continue $ a
                        Invalid e -> return . Invalid $ e

    instance MonadTrans ResultT where
      lift m = ResultT $ return <$> m

    newtype ResultST s a =
        ResultST (StateT s IO (Validate a))

    -- | Converts a `ResultST` into a `StateT`.
    toState :: ResultST s a -> StateT s IO (Validate a)
    toState (ResultST r) = r

    -- | Executes a result on the underlying store.
    runResultST :: ResultST s a -> (s -> IO (Validate a))
    runResultST (ResultST a) = evalStateT a

    -- | Lifts an `IO` computation to a result.
    liftIOR :: IO a -> ResultST s a
    liftIOR am = ResultST $ liftIO am <&> Valid

    liftResultST :: Validate a -> StateT s IO (Validate a)
    liftResultST (Valid r) = return . Valid $ r
    liftResultST (Invalid e) = return . Invalid $ e

    liftResult :: Result a -> ResultST s a
    liftResult (ResultT a) = do r <- liftIO a
                                ResultST . return $ r

    -- | Composition of results.
    bindResult :: (a -> ResultST s b)
               -> ResultST s a
               -> ResultST s b
    bindResult comp (ResultST x) = ResultST $ do
        rM <- x
        validate rM (return . Invalid) (toState . comp)

    -- | Converts a pure computation into a result.
    returnResult :: a -> ResultST s a
    returnResult = ResultST . return . Valid

    -- | Throws an error within a result.
    returnError :: Error -> ResultST s a
    returnError = ResultST . return . Invalid . (:[])

    -- | Outputs a newline terminated string to STDOUT.
    outputStrLn :: String -> ResultST s ()
    outputStrLn = liftIO . putStrLn

    -- | Outputs a string to STDOUT.
    outputStr :: String -> ResultST s ()
    outputStr = liftIO . putStr

    instance MonadIO (ResultST s) where
      liftIO = liftIOR

    instance Functor (ResultST s) where
        fmap f (ResultST am)= ResultST $ am <&> fmap f

    instance Applicative (ResultST s) where
        pure = returnResult
        (<*>) (ResultST fm) (ResultST am) =
            ResultST $ fm >>= (\fe ->
                       am >>= (\ae ->
                        return ((\f -> (Valid . f) =<< ae) =<< fe)))

    instance Monad (ResultST s) where
        x >>= f = bindResult f x

    instance MonadState s (ResultST s) where
        get :: ResultST s s
        get = ResultST $ do s <- get
                            return . Valid $ s
        put :: s -> ResultST s ()
        put s = ResultST $ do put s
                              return . Valid $ ()

    instance MonadThrow (ResultST s) where
      throwM :: (HasCallStack, Exception e) => e -> ResultST s a
      throwM = ResultST . throwM

    instance MonadCatch (ResultST s) where
      catch (ResultST r) f = ResultST $ catch r $ toState . f

    instance MonadMask (ResultST s) where
        mask f = ResultST $
            mask (\h -> toState $ f $ ResultST . h . toState)

        uninterruptibleMask f = ResultST $
            uninterruptibleMask (\h ->
                toState $ f $ ResultST . h . toState)

        generalBracket r f g =
            ResultST $ do x <- generalBracket (toState r) f' g'
                          return $ case x of
                            (Invalid e1, Invalid e2) -> Invalid $ e1 ++ e2
                            (Valid r1, Valid r2) -> Valid (r1,r2)
                            (Invalid e, _) -> Invalid e
                            (_, Invalid e) -> Invalid e

            where
                f' (Valid r') (ExitCaseSuccess (Valid b)) =
                    toState $ f r' $ ExitCaseSuccess b
                f' (Invalid e) _ =
                    return . Invalid $ e
                f' (Valid _) (ExitCaseSuccess (Invalid e)) =
                    return . Invalid $ e
                f' (Valid _) (ExitCaseException e) =
                    return . Invalid $ [Exception e]
                f' (Valid _) ExitCaseAbort =
                    return . Invalid $ [Aborted]

                g' (Valid r') = toState . g $ r'
                g' (Invalid e)  = return . Invalid $ e