module Handler.Article where

import Import
import Handler.NewArticle (articleForm, ArticleData(..))
-- import Data.Text (splitOn)

getArticleR :: ArticleId -> Handler TypedContent
getArticleR articleId = do
  article <- runDB $ get404 articleId
  currentUserId <- maybeAuthId
  let canEdit = isAuthorizedToEdit currentUserId article
  selectRep $ do
    provideRep . defaultLayout $ do
      setTitle . toHtml $ articleTitle article
      $(widgetFile "article")
    provideJson article

isAuthorizedToEdit :: Maybe UserId -> Article -> Bool
isAuthorizedToEdit userId article =
  fromMaybe False $ (articleAuthor article ==) <$> userId

putArticleR :: ArticleId -> Handler Html
putArticleR articleId = do
  article <- runDB $ get404 articleId
  currentUserId <- requireAuthId
  unless (isAuthorizedToEdit (Just currentUserId) article) $
    permissionDenied "Not authorized to edit this article"
  ((res, articleWidget), enctype) <- runFormPost $ articleForm Nothing
  case res of
    FormSuccess articleData -> do
      runDB $ update articleId [ ArticleTitle =. articleDataTitle articleData
                               , ArticleContent =. articleDataContent articleData
                               ]
      setMessage "Article edited"
      redirect $ ArticleR articleId
    _ -> defaultLayout $ do
      setTitle "Edit article"
      $(widgetFile "editArticle")

