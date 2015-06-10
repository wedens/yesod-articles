module Model.Types where

import ClassyPrelude.Yesod
import Data.Char
import qualified Data.Text as T
import Database.Persist.Sql

newtype Slug = Slug Text deriving (Show, Read, Eq, PathPiece, PersistField, PersistFieldSql)

createSlug :: Text -> Slug
createSlug = Slug .
             T.dropAround (== '-') .
             T.map (\c -> if isAlphaNum c then c else '-') .
             T.strip .
             T.toLower
