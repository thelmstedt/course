module Algorithm.FastAnagrams where

import Data.Char
import Data.List
import Data.Function
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  String
  -> FilePath
  -> IO [String]
fastAnagrams =
  error "todo"

newtype NoCaseString =
  NoCaseString {
    ncString :: String
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
