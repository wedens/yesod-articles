module Handler.NewArticle where

import Import
import Yesod.Form.Bootstrap3
import Text.Markdown
import qualified Data.Text as T
import Yesod.Text.Markdown

data ArticleData = ArticleData
    { articleDataTitle :: Text
    , articleDataContent :: Markdown
    , articleDataTags :: Maybe Text
    }

articleForm :: Maybe ArticleData -> Form ArticleData
articleForm article = renderBootstrap3 BootstrapBasicForm $ ArticleData
              <$> areq textField (bfs ("Title" :: Text)) (articleDataTitle <$> article)
              <*> areq markdownField (bfs ("Content" :: Text)) (articleDataContent <$> article)
              <*> aopt textField (bfs ("Tags" :: Text)) (articleDataTags <$> article)

getNewArticleR :: Handler Html
getNewArticleR = do
  (articleWidget, enctype) <- generateFormPost $ articleForm Nothing
  defaultLayout $ do
    setTitle "Enter new article"
    $(widgetFile "newArticle")

getEditArticleR :: ArticleId -> Handler Html
getEditArticleR articleId = do
  (article, tags) <- runDB $ (,)
                     <$> get404 articleId
                     <*> selectList [ TagArticle ==. articleId ] []
  (articleWidget, enctype) <- generateFormPost $ articleForm (Just $ articleToData ((\(Entity _ t) -> t) <$> tags) article)
  defaultLayout $ do
    setTitle "Edit article"
    $(widgetFile "editArticle")

articleToData :: [Tag] -> Article -> ArticleData
articleToData tags Article{..} = ArticleData articleTitle articleContent (Just $ tagsToText tags)

tagsToText :: [Tag] -> Text
tagsToText = T.intercalate "," . fmap tagName
