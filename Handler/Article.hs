module Handler.Article where

import Import
import Handler.NewArticle (articleForm, ArticleData(..))
import Handler.Articles (tagsFromArticleData)
import Handler.Comments (commentAnchorId)
import qualified Database.Esqueleto as E
import           Database.Esqueleto      ((^.))

getArticleR :: ArticleId -> Handler TypedContent
getArticleR articleId = do
  (article, tags, comments) <- runDB $ (,,)
    <$> get404 articleId
    <*> selectList [ TagArticle ==. articleId ] []
    <*> fetchComments articleId
  currentUserId <- maybeAuthId
  let canEdit = isAuthorizedToEdit currentUserId article
  let canComment = isJust currentUserId
  selectRep $ do
    provideRep . defaultLayout $ do
      setTitle . toHtml $ articleTitle article
      $(widgetFile "article")
    provideJson article
    

fetchComments :: ArticleId -> YesodDB App ([(Entity Comment, Entity User)])
fetchComments articleId =
    E.select $ E.from $ \(c `E.InnerJoin` u) -> do
    E.on (c ^. CommentAuthor E.==. u ^. UserId)
    E.where_ (c ^. CommentArticle E.==. E.val articleId)
    return (c,  u)

isAuthorizedToEdit :: Maybe UserId -> Article -> Bool
isAuthorizedToEdit userId article =
  fromMaybe False $ (articleAuthor article ==) <$> userId

putArticleR :: ArticleId -> Handler Html
putArticleR articleId = do
  article <- runDB $ get404 articleId
  currentUserId <- requireAuthId
  unless (isAuthorizedToEdit (Just currentUserId) article) $
    permissionDenied "You're not authorized to edit this article"
  ((res, articleWidget), enctype) <- runFormPost $ articleForm Nothing
  case res of
    FormSuccess articleData -> do
      let tags = tagsFromArticleData articleId articleData
      runDB $ do
        deleteWhere [ TagArticle ==. articleId ]
        insertMany_ tags
        update articleId [ ArticleTitle   =. articleDataTitle articleData
                         , ArticleContent =. articleDataContent articleData
                         ]
      setMessage "Article edited"
      redirect $ ArticleR articleId
    _ -> defaultLayout $ do
      setTitle "Edit article"
      $(widgetFile "editArticle")
