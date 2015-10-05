{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
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


type API = "eval-monadic" :> Capture "expr" String :> Get '[JSON] String
      :<|> "eval-non-monadic" :> Capture "expr" String :> Get '[JSON] String


server :: Server API
server = evalMonadic
    :<|> evalNonMonadic

  where evalMonadic :: String -> EitherT ServantErr IO String
        evalMonadic expr = return $ show $ evaluator' expr

        evalNonMonadic :: String -> EitherT ServantErr IO String
        evalNonMonadic expr = return $ show $ evaluator expr


api :: Proxy API
api = Proxy


app :: Application
app = serve api server


main :: IO ()
main = run 8081 app
