{-# LANGUAGE FlexibleInstances #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Database.Persist
import Database.Persist.Postgresql
import Text.Markdown
import Yesod.Text.Markdown ()
import Yesod.Auth.HashDB (HashDBUser(..))
import Model.Types

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance HashDBUser User where
  userPasswordHash = userPassword
  setPasswordHash h u = u { userPassword = Just h }

