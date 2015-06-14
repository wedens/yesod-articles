module Handler.Comments where

import Import
import Yesod.Text.Markdown
import Database.Persist.Sql
import qualified Data.Text as T

postCommentsR :: ArticleId -> Handler Html
postCommentsR articleId = do
  content <- runInputPost $ ireq markdownField "content"
  userId <- requireAuthId
  now <- liftIO getCurrentTime
  let comment = Comment articleId userId content now Nothing
  commentId <- runDB $ insert comment
  redirectToComment articleId commentId
  where
    redirectToComment aid cid = do
        render <- getUrlRender
        redirect $ (render $ ArticleR aid) <> "#" <> commentAnchorId cid

commentAnchorId :: CommentId -> Text
commentAnchorId cid = "comment" <> (T.pack . show . fromSqlKey $ cid)
