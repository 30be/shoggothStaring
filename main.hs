{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import Data.Text (stripSuffix)
import Hakyll
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 as H hiding (main, object)
import qualified Text.Blaze.Html5 as HTML (main)
import Text.Blaze.Html5.Attributes as A

data PostData = PostData
  { postTitle :: String,
    postDate :: String,
    postRoute :: String,
    postContent :: String
  }
  deriving (Generic, ToJSON)

getPostsData :: [Item a] -> Compiler [PostData]
getPostsData posts = forM posts $ \item -> do
  [title, date] <- forM ["title", "date"] $ getMetadataField' (itemIdentifier item)
  let getRoute' i = fromMaybe (error $ "Route not found for " <> show i) <$> getRoute i
  route <- getRoute' $ itemIdentifier item
  content <- loadBody $ itemIdentifier item
  return $ PostData title date route content

jsonIndexCompiler :: [Item String] -> Compiler (Item String)
jsonIndexCompiler = getPostsData >=> encode >>> decodeUtf8 >>> makeItem

indexCompiler :: [Item String] -> Item String -> Compiler (Item String)
indexCompiler posts text = do
  postsData <- forM posts $ \item -> do
    [title, date] <- forM ["title", "date"] $ getMetadataField' (itemIdentifier item)
    route <- getRoute $ itemIdentifier item
    return (title, date, route ?: "#")

  let strip url = toValue $ stripSuffix ".html" url ?: url
      renderText text = renderHtml $ do
        preEscapedToHtml text
        ul $ forM_ postsData $ \(title, date, url) -> li $ a ! href (strip $ toText url) $ toHtml $ title <> " - " <> date
  pure $ renderText <$> text

defaultTemplate :: Item String -> Compiler (Item String)
defaultTemplate item = do
  title <- maybeToMonoid <$> getMetadataField (itemIdentifier item) "title"
  let headerLinks = [("/", "Home"), ("/about", "About"), ("/me", "Me"), ("/rss.xml", "RSS")]
      defaultHTML contents = docTypeHtml ! lang "en" $ do
        H.head $ do
          H.title $ "shoggothStaring" <> (if null title then "" else " :: ") <> toHtml title
          meta ! charset "utf-8"
          meta ! httpEquiv "x-ua-compatible" ! content "ie=edge"
          meta ! name "viewport" ! content "width=device-width, initial-scale=1"
          meta ! name "color-scheme" ! content "light dark"
          link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/sakura.css/css/sakura.css" ! media "screen"
          link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/sakura.css/css/sakura-vader.css" ! media "screen and (prefers-color-scheme: dark)"
          link ! rel "stylesheet" ! href "/static/pandoc-pygments.css" ! media "screen and not (prefers-color-scheme: dark)"
          link ! rel "stylesheet" ! href "/static/pandoc-zenburn.css" ! media "screen and (prefers-color-scheme: dark)"
          link ! rel "stylesheet" ! href "/static/style.css"
          script ! src "/static/search.js" $ mempty
        body ! A.style "font-family: 'Roboto', sans-serif" $ HTML.main ! class_ "container" $ do
          nav ! class_ "main-nav" $ do
            forM_ headerLinks $ \(link, label) -> (a ! href link $ label) >> " | "

            -- H.span ! A.id "search-container" $ do
            input ! type_ "text" ! A.id "search-input" ! placeholder "Search..."

          H.div ! A.id "search-results" $ mempty
          H.article ! class_ "user-content" $ contents
          footer $ do
            hr
            p $ do
              "© 2025 LS4. The page source and code are available on "
              a ! href "https://github.com/30be/shoggothStaring" $ "GitHub"
              "."
  relativizeUrls $ renderHtml . defaultHTML . preEscapedToHtml <$> item

postTemplate :: Item String -> Compiler (Item String)
postTemplate item = do
  [title, date] <- forM ["title", "date"] $ fmap toHtml . getMetadataField' (itemIdentifier item)
  let postHTML contents = article $ do
        h1 title
        small date
        contents
  pure $ renderHtml . postHTML . preEscapedToHtml <$> item

main :: IO ()
main = hakyll $ do
  match "static/*.css" $ route idRoute >> compile compressCssCompiler
  match "static/favicon.ico" $ route (gsubRoute "static/" (const "")) >> compile copyFileCompiler
  match "static/*" $ route idRoute >> compile copyFileCompiler

  match "index.md" $ do
    route $ setExtension "html"
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      pandocCompiler >>= indexCompiler posts >>= defaultTemplate

  match "posts/*" $ do
    route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
    compile $ pandocCompiler >>= postTemplate >>= saveSnapshot "content" >>= defaultTemplate
  makeFeed renderAtom ["atom.xml", "feed.atom"]
  makeFeed renderRss ["feed.rss", "rss.xml", "feed", "rss"]
  create ["search.json"] $ do
    route idRoute
    compile $ loadAllSnapshots "posts/*" "content" >>= jsonIndexCompiler

makeFeed :: (FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)) -> [Identifier] -> Rules ()
makeFeed render targets =
  create targets $ do
    route idRoute
    compile $ loadAllSnapshots "posts/*" "content" >>= fmap (take 10) . recentFirst >>= render configuration (defaultContext <> bodyField "description")
  where
    configuration =
      FeedConfiguration
        { feedTitle = "shoggothStaring",
          feedDescription = "Thoughts about now and future, written mostly for myself.",
          feedAuthorName = "LS4",
          feedAuthorEmail = "lykd@pm.me",
          feedRoot = "https://shoggothstaring.com"
        }
