module Handler.User where

import Import
import Yesod.Auth.HashDB (setPassword)

getSignUpR :: Handler Html
getSignUpR = defaultLayout $ setTitle "Sign Up" >> $(widgetFile "signup")

postSignUpR :: Handler Html
postSignUpR = do
  (username, password) <- runInputPost $ (,)
    <$> ireq textField "username"
    <*> ireq passwordField "password"
  user <- setPassword password (User username Nothing)
  runDB $ insert_ user
  setMessage $ toHtml $ "User " ++ username ++ " successfully registered"
  redirect ArticlesR
