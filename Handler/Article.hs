module Handler.Article where

import Import
import Handler.NewArticle (articleForm, ArticleData(..))

getArticleR :: ArticleId -> Handler TypedContent
getArticleR articleId = do
  article <- runDB $ get404 articleId
  currentUserId <- maybeAuthId
  let canEdit = fromMaybe False $ (articleAuthor article ==) <$> currentUserId
  selectRep $ do
    provideRep $ defaultLayout $ do
      setTitle $ toHtml $ articleTitle article
      $(widgetFile "article")
    provideJson article

putArticleR :: ArticleId -> Handler Html
putArticleR articleId = do
  _ <- runDB $ get404 articleId
  -- TODO: authorization
  ((res, articleWidget), enctype) <- runFormPost $ articleForm Nothing
  case res of
    FormSuccess articleData -> do
      runDB $ update articleId [ ArticleTitle =. articleDataTitle articleData
                               , ArticleContent =. articleDataContent articleData ]
      setMessage $ "Article edited"
      redirect $ ArticleR articleId
    _ -> defaultLayout $ do
      setTitle "Edit article"
      $(widgetFile "editArticle")

