module Handler.Articles where

import Import
import Handler.NewArticle (articleForm, ArticleData(..))
-- import Data.Text (splitOn)
-- import qualified Database.Esqueleto as E

articlesPageSize :: Int
articlesPageSize = 2

getArticlesR :: Handler TypedContent
getArticlesR = do
  page <- getPageFromRequest
  (articles, pagesCount) <- runDB $ fetchArticlesPage page articlesPageSize
  selectRep $ do
    provideRep . defaultLayout $ do
      setTitle "articles"
      $(widgetFile "articles")
    provideJson articles

getPageFromRequest :: MonadHandler m => m Int
getPageFromRequest = (\p -> fromMaybe 1 (readIntegral . unpack =<< p)) <$> lookupGetParam "page"

fetchArticlesPage :: Int -> Int -> YesodDB App ([Entity Article], Int)
fetchArticlesPage page pageSize = do
  articlesCount <- count ([] :: [Filter Article])
  let pagesCount = (\(n, r) -> n + min r 1) $ articlesCount `divMod` pageSize
  articles <- selectList [] [ Desc ArticlePostedAt
                            , OffsetBy ((page - 1) * pageSize)
                            , LimitTo pageSize
                            ]
  return (articles, pagesCount)

postArticlesR :: Handler Html
postArticlesR = do
  now <- liftIO getCurrentTime
  currentUserId <- requireAuthId
  ((res, articleWidget), enctype) <- runFormPost $ articleForm Nothing
  case res of
    FormSuccess articleData -> do
      articleId <- runDB . insert $ createArticle articleData now currentUserId
      setMessage "New article created"
      redirect $ ArticleR articleId
    _ -> defaultLayout $ do
      setTitle "Please correct your article"
      $(widgetFile "newArticle")

createArticle :: ArticleData -> UTCTime -> UserId -> Article
createArticle ArticleData{..} = Article articleDataTitle articleDataContent

getArticlesFeedR :: Handler TypedContent
getArticlesFeedR = do
  articles@(newest:_) <- runDB $ selectList [] [Desc ArticlePostedAt, LimitTo 10]
  newsFeed Feed
    { feedTitle = "Articles"
    , feedLinkSelf = ArticlesFeedR
    , feedLinkHome = ArticlesR
    , feedUpdated = articlePostedAt $ entityVal newest
    , feedEntries = articleToFeedEntry <$> articles
    , feedDescription = "Articles feed"
    , feedLanguage = "en"
    , feedAuthor = "everyone"
    }
  where
    articleToFeedEntry (Entity aid a) = FeedEntry
        { feedEntryLink = ArticleR aid
        , feedEntryUpdated = articlePostedAt a
        , feedEntryTitle = articleTitle a
        , feedEntryContent = toHtml $ articleContent a
        }
