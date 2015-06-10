module Handler.Articles where

import Import
import Handler.NewArticle (articleForm, ArticleData(..))
import Data.Text (splitOn)

import Model.Types
import qualified Database.Esqueleto as E
import           Database.Esqueleto      ((^.))

articlesPageSize :: Int
articlesPageSize = 2

getArticlesR :: Handler TypedContent
getArticlesR = do
  page <- getPageFromRequest
  currentUserId <- maybeAuthId
  let canCreateArticles = isJust currentUserId
  (articlesWithTags, pagesCount) <- runDB $ fetchArticlesPage page articlesPageSize
  selectRep $ do
    provideRep . defaultLayout $ do
      setTitle "articles"
      $(widgetFile "articles")
    provideJson $ fst <$> articlesWithTags -- TODO: with tags

getPageFromRequest :: MonadHandler m => m Int
getPageFromRequest = (\p -> fromMaybe 1 (readIntegral . unpack =<< p)) <$> lookupGetParam "page"

fetchArticlesPage :: Int -> Int -> YesodDB App ([(Entity Article, [Entity Tag])], Int)
fetchArticlesPage page pageSize = do
  articlesCount <- count ([] :: [Filter Article])
  let pagesCount = (\(n, r) -> n + min r 1) $ articlesCount `divMod` pageSize
  articles <- selectList [] [ Desc ArticlePostedAt
                            , OffsetBy ((page - 1) * pageSize)
                            , LimitTo pageSize
                            ]
  let articleIds = entityKey <$> articles
  tags <- selectList [ TagArticle <-. articleIds ] []
  let articlesWithTags = pairArticlesWithTags articles tags
  return (articlesWithTags, pagesCount)

fetchArticlesPageByTag :: Int -> Int -> Slug -> YesodDB App ([(Entity Article, [Entity Tag])], Int)
fetchArticlesPageByTag page pageSize slug = do
  articlesWithTagCount <- count [ TagSlug ==. slug ]
  let pagesCount = (\(n, r) -> n + min r 1) $ articlesWithTagCount `divMod` pageSize
  articleTags <- selectList [ TagSlug ==. slug ] [ Desc TagId
                                       , OffsetBy ((page - 1) * pageSize)
                                       , LimitTo pageSize
                                       ]
  articles <- selectList [ ArticleId <-. ((tagArticle . entityVal) <$> articleTags) ] [ Desc ArticlePostedAt ]
  let articleIds = entityKey <$> articles
  tags <- selectList [ TagArticle <-. articleIds ] []
  let articlesWithTags = pairArticlesWithTags articles tags
  return (articlesWithTags, pagesCount)

fetchArticlesPageByTag2 :: Int -> Int -> Slug -> YesodDB App ([(Entity Article, [Entity Tag])], Int)
fetchArticlesPageByTag2 page pageSize slug = do
  articlesWithTagCount <- count [ TagSlug ==. slug ]
  let pagesCount = (\(n, r) -> n + min r 1) $ fromIntegral (articlesWithTagCount `divMod` pageSize) :: Int64
  articles <- E.select $ E.from $ \(article `E.InnerJoin` tag) -> do
    E.on $ article ^. ArticleId E.==. tag ^. TagArticle
    E.where_ $ tag ^. TagSlug E.==. E.val slug
    E.orderBy [E.desc (article ^. ArticlePostedAt)]
    E.offset $ E.val $ fromIntegral ((page - 1) * pageSize) :: Int64
    E.limit $ fromIntegral pageSize :: Int64
    return article
  error ""

pairArticlesWithTags :: [Entity Article] -> [Entity Tag] -> [(Entity Article, [Entity Tag])]
pairArticlesWithTags articles tags =
  (\a -> (a, filter (isTagFromArticle a) tags)) <$> articles
  where
    isTagFromArticle (Entity articleId _) (Entity _ tag) = tagArticle tag == articleId

getByTagR :: Slug -> Handler TypedContent
getByTagR slug = do
  page <- getPageFromRequest
  currentUserId <- maybeAuthId
  let canCreateArticles = isJust currentUserId
  (articlesWithTags, pagesCount) <- runDB $ fetchArticlesPageByTag page articlesPageSize slug
  selectRep $ do
    provideRep . defaultLayout $ do
      setTitle "articles"
      $(widgetFile "articles")
    provideJson $ fst <$> articlesWithTags -- TODO: with tags

postArticlesR :: Handler Html
postArticlesR = do
  now <- liftIO getCurrentTime
  currentUserId <- requireAuthId
  ((res, articleWidget), enctype) <- runFormPost $ articleForm Nothing
  case res of
    FormSuccess articleData -> do
      articleId <- runDB $ do
        articleId <- insert $ createArticle articleData now currentUserId
        let tags = tagsFromArticleData articleId articleData
        insertMany_ tags
        return articleId
      setMessage "New article created"
      redirect $ ArticleR articleId
    _ -> defaultLayout $ do
      setTitle "Please correct your article"
      $(widgetFile "newArticle")

tagsFromArticleData :: ArticleId -> ArticleData -> [Tag]
tagsFromArticleData articleId articleData =
  fmap (\t -> Tag articleId t (createSlug t)) . fromMaybe [] $ splitOn "," <$> articleDataTags articleData

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
