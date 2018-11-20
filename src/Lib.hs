module Lib
    ( readEnv
    )
where

import           System.Environment             ( lookupEnv )

data Config =
    Config
        { oneskyProjectId :: String
        , oneskyApiKey :: String
        , oneskySecretKet :: String
        } deriving (Show)

readEnv :: IO (Maybe Config)
readEnv = do
    projectId <- lookupEnv "ONESKY_PROJECT_ID"
    apiKey    <- lookupEnv "ONESKY_API_KEY"
    password  <- lookupEnv "ONESKY_SECRET_KEY"
    return $ Config <$> projectId <*> password <*> password
