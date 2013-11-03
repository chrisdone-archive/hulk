{-# LANGUAGE OverloadedStrings #-}

module Hulk.Providers where


import           Prelude                    hiding (readFile)

-- instance MonadProvider HulkIO where
--   providePreface = maybeReadFile configPreface
--   provideMotd = maybeReadFile configMotd
--   provideKey = mustReadFile configPasswdKey
--   providePasswords = mustReadFile configPasswd
--   provideWriteUser udata = do
--     path <- asks configUserData
--     liftIO $ L.writeFile (path </> normalizeUser (unpack (userText (userDataUser udata)))) $ encode udata
--   provideLogger name rpl params = do
--     path <- asks configLogFile
--     now <- liftIO $ getCurrentTime
--     liftIO $
--       L.appendFile path $ encode ((now
--                                   ,name
--                                   ,rpl
--                                   ,params))
--                           <> "\n"
--   provideLog = do
--     path <- asks configLogFile
--     contents <- liftIO $ L.readFile path
--     return $ mapMaybe decode $ L8.lines contents


-- maybeReadFile :: (Config -> Maybe FilePath) -> HulkIO (Maybe Text)
-- maybeReadFile get = do
--   path <- asks get
--   case path of
--     Nothing -> return Nothing
--     Just path -> Just <$> liftIO (T.readFile path)

-- mustReadFile :: (Config -> FilePath) -> HulkIO Text
-- mustReadFile get = asks get >>= liftIO . T.readFile
