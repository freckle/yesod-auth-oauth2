{-# OPTIONS_GHC -Wno-orphans #-}

module UnliftIO.Except
    () where

import Control.Monad.Except
import UnliftIO

instance (MonadUnliftIO m, Exception e) => MonadUnliftIO (ExceptT e m) where
    withRunInIO exceptToIO = ExceptT $ try $ do
        withRunInIO $ \runInIO ->
            exceptToIO (runInIO . (either throwIO pure <=< runExceptT))
