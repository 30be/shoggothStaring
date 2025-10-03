import Data.Text (stripSuffix)
import Hakyll
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 as H hiding (main)
import qualified Text.Blaze.Html5 as HTML (main)
import Text.Blaze.Html5.Attributes as A

index :: [Item String] -> Item String -> Compiler (Item String)
index posts text = do
  postsData <- forM posts $ \item -> do
    [title, date] <- forM ["title", "date"] $ getMetadataField' (itemIdentifier item)
    route <- getRoute $ itemIdentifier item
    return (title, date, route ?: "#")
  pure $ compile postsData <$> text
 where
  compile postsData text = renderHtml $ do
    preEscapedToHtml text
    ul $ forM_ postsData $ \(title, date, url) ->
      let strip url = toValue $ stripSuffix ".html" url ?: url
       in li $ a ! href (strip $ toText url) $ toHtml title <> " - " <> toHtml date

defaultTemplate :: Item String -> Compiler (Item String)
defaultTemplate item = do
  title <- maybeToMonoid <$> getMetadataField (itemIdentifier item) "title"
  let
    headerLinks = [("/", "Home"), ("/about", "About"), ("/rss.xml", "RSS"), ("https://github.com/30be", "GitHub"), ("https://t.me/ls4wrong", "Telegram")]
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
      body ! A.style "font-family: 'Roboto', sans-serif" $ HTML.main ! class_ "container" $ do
        nav $ forM_ (zip [0 ..] headerLinks) $ \(index, (link, label)) -> do
          a ! href link $ label
          when (index < length headerLinks - 1) " | "

        -- And now with a list
        H.div ! class_ "user-content" $ contents
        footer $ do
          hr -- Pico uses <hr> for horizontal separators
          p $ do
            "© 2025 LS4. The page source and code are available on "
            a ! href "https://github.com/30be/shoggothStaring" $ "GitHub"
            "."
  pure $ renderHtml . defaultHTML . preEscapedToHtml <$> item

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
  match "static/*" $ route idRoute >> compile copyFileCompiler

  match "index.md" $ do
    route $ setExtension "html"
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      pandocCompiler >>= index posts >>= defaultTemplate >>= relativizeUrls

  match "posts/*" $ do
    route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
    compile $ pandocCompiler >>= postTemplate >>= saveSnapshot "content" >>= defaultTemplate >>= relativizeUrls

  makeFeed renderAtom ["atom.xml", "feed.atom"]
  makeFeed renderRss ["feed.rss", "rss.xml", "feed", "rss"]

makeFeed :: (FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)) -> [Identifier] -> Rules ()
makeFeed render targets =
  create targets $ do
    route idRoute
    compile $ do
      let feedCtx = defaultContext <> bodyField "description"
      posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
      render feedConfiguration feedCtx posts
 where
  feedConfiguration =
    FeedConfiguration
      { feedTitle = "shoggothStaring"
      , feedDescription = "Thoughts about now and future, written mostly for myself."
      , feedAuthorName = "LS4"
      , feedAuthorEmail = "lykd@pm.me"
      , feedRoot = "https://shoggothstaring.com"
      }
