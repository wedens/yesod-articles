module Handler.Comments where

import Import
import Yesod.Text.Markdown
import Text.Markdown
import Database.Persist.Sql
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

postCommentsR :: ArticleId -> Handler Html
postCommentsR articleId = do
  userId <- requireAuthId
  contentRes <- runInputPostResult $ ireq commentContentField "content"
  case contentRes of
    FormSuccess content -> do
      now <- liftIO getCurrentTime
      let comment = Comment articleId userId content now Nothing
      commentId <- runDB $ insert comment
      redirectToComment articleId commentId
    FormFailure errors -> undefined
    FormMissing -> undefined
  where
    redirectToComment aid cid = do
        render <- getUrlRender
        redirect $ (render $ ArticleR aid) <> "#" <> commentAnchorId cid
    commentContentField = checkBool (isBlank . markdownToText) ("Should not be blank" :: Text) markdownField

markdownToText :: Markdown -> LT.Text
markdownToText (Markdown text) = text


isBlank :: LT.Text -> Bool
isBlank = LT.null . LT.strip

commentAnchorId :: CommentId -> Text
commentAnchorId cid = "comment" <> (T.pack . show . fromSqlKey $ cid)
