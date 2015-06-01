module Handler.NewArticle where

import Import
import Yesod.Form.Bootstrap3
import Text.Markdown
import Yesod.Text.Markdown

data ArticleData = ArticleData
    { articleDataTitle :: Text
    , articleDataContent :: Markdown
    }

articleForm :: Maybe ArticleData -> Form ArticleData
articleForm article = renderBootstrap3 BootstrapBasicForm $ ArticleData
              <$> areq textField (bfs ("Title" :: Text)) (articleDataTitle <$> article)
              <*> areq markdownField (bfs ("Content" :: Text)) (articleDataContent <$> article)

getNewArticleR :: Handler Html
getNewArticleR = do
  (articleWidget, enctype) <- generateFormPost $ articleForm Nothing
  defaultLayout $ do
    setTitle "Enter new article"
    $(widgetFile "newArticle")

getEditArticleR :: ArticleId -> Handler Html
getEditArticleR articleId = do
  article <- runDB $ get404 articleId
  (articleWidget, enctype) <- generateFormPost $ articleForm (Just $ articleToData article)
  defaultLayout $ do
    setTitle "Edit article"
    $(widgetFile "editArticle")

articleToData :: Article -> ArticleData
articleToData Article{..} = ArticleData articleTitle articleContent
