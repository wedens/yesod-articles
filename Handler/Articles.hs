{-# LANGUAGE ScopedTypeVariables#-}
module Handler.Articles where

import Import
import Handler.NewArticle (articleForm, ArticleData(..))
import Data.Text (splitOn)
import Database.Persist.Sql (Single(..), rawSql, unSingle)
import Text.Shakespeare.Text (st)

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

getSearchR :: Handler Html
getSearchR = do
  page <- getPageFromRequest
  query<- runInputGet $ ireq textField "query"
  currentUserId <- maybeAuthId
  let canCreateArticles = isJust currentUserId
  (articlesWithTags, pagesCount) <- runDB $ searchArticles query page articlesPageSize
  defaultLayout $ $(widgetFile "articles")

q :: Text
q = [st|
    WITH q AS (SELECT plainto_tsquery(?))
    SELECT ??
    FROM article
    WHERE fulltext @@ (SELECT * FROM q)
    ORDER BY ts_rank_cd(fulltext, (SELECT * FROM q)) DESC
    OFFSET ?
    LIMIT ?
    |]

q2 :: Text
q2 = [st|
     WITH q AS (SELECT plainto_tsquery(?))
     SELECT COUNT(1)
     FROM article WHERE fulltext @@ (SELECT * FROM q)
     |]

searchArticles :: Text -> Int -> Int -> YesodDB App ([(Entity Article, [Entity Tag])], Int)
searchArticles query page pageSize = do
  total :: [Single Int] <- rawSql q2 [toPersistValue query]
  let (x : []) = total
  let y = unSingle x
  let pagesCount = ((\(n, r) -> n + min r 1) . (flip divMod pageSize)) y
  let offset = (page - 1) * pageSize
  articles :: [Entity Article] <- rawSql q [toPersistValue query, toPersistValue offset, toPersistValue pageSize]
  let articleIds = entityKey <$> articles
  tags <- selectList [ TagArticle <-. articleIds ] []
  let articlesWithTags = pairArticlesWithTags articles tags
  return (articlesWithTags, pagesCount)

fetchArticlesPage :: Int -> Int -> YesodDB App ([(Entity Article, [Entity Tag])], Int)
fetchArticlesPage page pageSize = do
  pagesCount <- countPages pageSize ([] :: [Filter Article])
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
  pagesCount <- countPages pageSize [ TagSlug ==. slug ]
  let offset = (page - 1) * pageSize
  let offset' = fromIntegral offset :: Int64
  let pageSize' = fromIntegral pageSize :: Int64
  articles <- E.select $ E.from $ \(article `E.InnerJoin` tag) -> do
    E.on $ article ^. ArticleId E.==. tag ^. TagArticle
    E.where_ $ tag ^. TagSlug E.==. E.val slug
    E.orderBy [E.desc (article ^. ArticlePostedAt)]
    E.offset $ offset'
    E.limit $ pageSize'
    return article
  let articleIds = entityKey <$> articles
  tags <- selectList [ TagArticle <-. articleIds ] []
  let articlesWithTags = pairArticlesWithTags articles tags
  return (articlesWithTags, pagesCount)


countPages :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend) => Int -> [Filter val] -> YesodDB App Int
countPages pageSize filters =
  ((\(n, r) -> n + min r 1) . (flip divMod pageSize)) <$> count filters

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


