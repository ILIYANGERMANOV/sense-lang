module Data.SFunction where

import Data.Identifier (Identifier (Identifier))
import Data.SType

{- 
helloWorld :: String
> "Hello, world!"

greet :: String -> Unit
name> print "Hello, " + name

greet("Iliyan")

authenticate :: User | Admin -> AccessToken | RejectReason
u: User> if valid(u.id)
    True> AccessToken
    False> RejectReason("Invalid ID")
Admin> AccessToken
-}
data SFunction = SFunction
  { fId :: Identifier,
    fSignature :: (SType, SType)
  }
  deriving Show