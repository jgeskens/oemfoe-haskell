{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
import Control.Monad.Trans.Either
import Data.Aeson
import Data.List
import Data.Maybe
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Evaluator


type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
      :<|> "eval" :> Capture "expr" String :> Get '[JSON] String

data Position = Position
  { x :: Int
  , y :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { name :: String
  , email :: String
  , age :: Int
  , interested_in :: [String]
  } deriving Generic

instance FromJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'

  where from'    = "great@company.com"
        to'      = email c
        subject' = "Hey " ++ name c ++ ", we miss you!"
        body'    = "Hi " ++ name c ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (age c)
                ++ ", have you checked out our latest "
                ++ intercalate ", " (interested_in c)
                ++ " products? Give us a visit!"

server :: Server API
server = position
    :<|> hello
    :<|> marketing
    :<|> doeval

  where position :: Int -> Int -> EitherT ServantErr IO Position
        position x y = return (Position x y)

        hello :: Maybe String -> EitherT ServantErr IO HelloMessage
        hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous coward"
          Just n  -> "Hello, " ++ n

        marketing :: ClientInfo -> EitherT ServantErr IO Email
        marketing clientinfo = return (emailForClient clientinfo)

        doeval :: String -> EitherT ServantErr IO String
        doeval expr = return $ show $ evaluator' expr
            where extract (TInteger i, _) = i

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8081 app
