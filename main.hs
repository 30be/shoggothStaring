import Data.Aeson
import Data.Text (stripSuffix)
import Hakyll
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A

data PostData = PostData
  { postTitle :: String,
    postDate :: String,
    postRoute :: String,
    postContent :: String
  }
  deriving (Generic, ToJSON)

getPostData :: Item a -> Compiler PostData
getPostData =
  itemIdentifier >>> \postID ->
    PostData
      <$> getMetadataField' postID "title"
      <*> getMetadataField' postID "date"
      <*> (fromMaybe (fail $ "Route not found for " <> show postID) <$> getRoute postID)
      <*> loadBody postID

jsonIndexCompiler :: [Item String] -> Compiler (Item String)
jsonIndexCompiler = traverse getPostData >=> encode >>> decodeUtf8 >>> makeItem

indexCompiler :: [Item String] -> Item String -> Compiler (Item String)
indexCompiler posts text = fmap . renderText <$> traverse getPostData posts ?? text
  where
    strip url = toValue $ stripSuffix ".html" url ?: url
    renderText postsData text = renderHtml $ do
      preEscapedToHtml text
      ul $ forM_ postsData $ \(PostData {..}) ->
        li $ a ! href (strip $ toText postRoute) $ toHtml $ postTitle <> " - " <> postDate

defaultTemplate :: Item String -> Compiler (Item String)
defaultTemplate item = do
  title <- getMetadataField (itemIdentifier item) "title"
  relativizeUrls $ renderHtml . defaultHTML (maybeToMonoid title) . preEscapedToHtml <$> item
  where
    headerLinks = [("/", "Home"), ("/about", "About"), ("/me", "Me"), ("/rss.xml", "RSS")]
    defaultHTML :: String -> Html -> Html
    defaultHTML title contents = docTypeHtml ! lang "en" $ do
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
      body $ do
        nav ! class_ "main-nav" $ do
          forM_ headerLinks $ \(link, label) -> (a ! href link $ label) >> " | "
          input ! type_ "text" ! A.id "search-input" ! placeholder "Search..."

        H.div ! A.id "search-results" $ mempty
        article ! class_ "user-content" $ contents
        footer $ do
          hr
          p $ do
            "© 2025 LS4. The page source and code are available on "
            a ! href "https://github.com/30be/shoggothStaring" $ "GitHub"
            "."

postTemplate :: Item String -> Compiler (Item String)
postTemplate item = do
  [title, date] <- fmap toHtml <$> forM ["title", "date"] (getMetadataField' (itemIdentifier item))
  let postHTML contents = do
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
      posts <- recentFirst =<< loadAll (fromVersion Nothing .&&. "posts/*")
      pandocCompiler >>= indexCompiler posts >>= defaultTemplate
  match "posts/*" $ version "raw" $ do
    route $ gsubRoute "posts/" (const "")
    compile getResourceString
  match "posts/*" $ do
    route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
    compile $ pandocCompiler >>= postTemplate >>= saveSnapshot "content" >>= defaultTemplate

  create ["search.json"] $ do
    route idRoute
    compile $ loadAll (fromVersion (Just "raw") .&&. "posts/*") >>= jsonIndexCompiler

  makeFeed renderRss ["feed.rss", "rss.xml", "feed", "rss"]
  makeFeed renderAtom ["atom.xml", "feed.atom"]

makeFeed :: (FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)) -> [Identifier] -> Rules ()
makeFeed render targets =
  create targets $ do
    route idRoute
    compile $
      loadAllSnapshots (fromVersion Nothing .&&. "posts/*") "content"
        >>= fmap (take 10) . recentFirst
        >>= render configuration (defaultContext <> bodyField "description")
  where
    configuration =
      FeedConfiguration
        { feedTitle = "shoggothStaring",
          feedDescription = "Thoughts about now and future, written mostly for myself.",
          feedAuthorName = "LS4",
          feedAuthorEmail = "lykd@pm.me",
          feedRoot = "https://shoggothstaring.com"
        }
