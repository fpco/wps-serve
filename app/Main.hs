import RIO
import RIO.Directory
import RIO.FilePath
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import qualified Data.Yaml as Yaml
import qualified RIO.Text as T
import qualified RIO.Map as Map
import Network.Mime (defaultMimeLookup)
import System.Environment (getArgs)

usage :: IO a
usage = error "Usage: wordpress-serve --root <dir> --port <portnumber> --404 <file> --redirects <file>"

main :: IO ()
main = do
  args <- getArgs
  (root, port, notFoundHtml, redirectsFP) <- maybe usage pure $ do
    ["--root", root, "--port", portS, "--404", notFoundHtml, "--redirects", redirectsFP] <- Just args
    port <- readMaybe portS
    pure (root, port, notFoundHtml, redirectsFP)

  redirects <- Yaml.decodeFileThrow redirectsFP
  let notFound = responseFile status404 [("Content-Type", "text/html; charset=utf-8")] notFoundHtml Nothing
  run port $ \req send ->
    case pathInfo req of
      p
        | any (\x -> x == "." || x == "..") p -> send $ responseBuilder status400 [] "Nice try"
        | ("index.html":rest) <- reverse p ->
            send $ responseBuilder status302 [("Location", encodeUtf8 $ "/" <> T.intercalate "/" (reverse rest))] "Redirecting"
      p
        | Just dest <- Map.lookup (T.intercalate "/" (filter (not . T.null) p)) redirects ->
            send $ responseBuilder status302 [("Location", encodeUtf8 dest)] "Redirecting"
        | otherwise -> do
            let fp = root </> T.unpack (T.intercalate "/" p)
            file <- doesFileExist fp
            if file
              then send $ responseFile status200 [("Content-Type", defaultMimeLookup $ T.pack fp)] fp Nothing
              else do
                  dir <- doesDirectoryExist fp
                  if dir
                    then do
                      let index = fp </> "index.html"
                      indexExists <- doesFileExist index
                      if indexExists
                        then
                          case cleanupPath p of
                            Nothing -> send $ responseFile status200 [("Content-Type", "text/html; charset=utf-8")] index Nothing
                            Just clean -> send $ responseBuilder status302 [("Location", clean)] "Redirecting"
                        else send notFound
                    else send notFound

cleanupPath :: [Text] -> Maybe ByteString
cleanupPath [] = Nothing
cleanupPath xs =
  case reverse xs of
    "":rest | all (not . T.null) rest -> Nothing
    otherwise -> Just $ encodeUtf8 $ foldMap ("/" <>) $ filter (not . T.null) xs ++ [""]
