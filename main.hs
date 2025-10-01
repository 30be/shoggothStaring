import Hakyll
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A

makeIndex :: [Item String] -> Compiler (Item String)
makeIndex posts = do
  postsData <- forM posts $ \item -> do
    [title, date] <- forM ["title", "date"] $ getMetadataField' (itemIdentifier item)
    route <- getRoute $ itemIdentifier item
    return (title, date, fromMaybe "#" route)

  makeItem $ renderHtml $ do
    h2 "Welcome"
    img ! src "/images/haskell-logo.png" ! A.style "float: right; margin: 10px;"
    p "Welcome to my blog!"
    h2 "This is what I have written about:"
    ul $ forM_ postsData $ \(title, date, url) -> li $ a ! href (toValue url) $ toHtml title <> " - " <> toHtml date

defaultTemplate :: Item String -> Compiler (Item String)
defaultTemplate item = do
  title <- fromMaybe "" <$> getMetadataField (itemIdentifier item) "title"
  let
    defaultHTML contents = docTypeHtml ! lang "en" $ do
      H.head $ do
        meta ! charset "utf-8"
        meta ! httpEquiv "x-ua-compatible" ! content "ie=edge"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1"
        H.title $ "shoggothStaring" <> (if null title then "" else " :: ") <> toHtml title
        link ! rel "stylesheet" ! href "/static/default.css"
      body $ do
        header $ H.div ! class_ "logo" $ a ! href "/" $ "My Hakyll Blog"
        H.div ! class_ "user-content" $ contents
  pure $ renderHtml . defaultHTML . preEscapedToHtml <$> item

postTemplate :: Item String -> Compiler (Item String)
postTemplate item = do
  [title, date] <- forM ["title", "date"] $ getMetadataField' (itemIdentifier item)
  let postHTML contents = do
        h1 $ toHtml title
        h4 $ toHtml date
        contents
  pure $ renderHtml . postHTML . preEscapedToHtml <$> item

main :: IO ()
main = hakyll $ do
  match "static/*" $ route idRoute >> compile copyFileCompiler

  match "posts/*" $ do
    route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
    compile $ pandocCompiler >>= postTemplate >>= saveSnapshot "content" >>= defaultTemplate >>= relativizeUrls

  create ["index.html"] $ do
    route idRoute
    compile $ loadAll "posts/*" >>= recentFirst >>= makeIndex >>= defaultTemplate >>= relativizeUrls

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
      , feedAuthorName = "Lykd"
      , feedAuthorEmail = "lykd@pm.me"
      , feedRoot = "https://shoggothstaring.com"
      }
