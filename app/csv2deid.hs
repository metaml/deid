module Main where

import Control.Monad (when)
import Control.Lens
import Data.Aeson as A
import Data.ByteString as B
import Data.Csv as Csv
import Data.Int
import Data.IORef
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding as T
import Gogol.DLP.Types
import Model.Csv
import Model.Deid
import Prelude as P hiding (getLine)
import Streamly.Data.Stream as S
import System.IO (hPutStrLn, stderr)
import qualified Data.Aeson.KeyMap as M
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text.IO as IO
import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import qualified Etc.Csv2Deid as Cli
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Unfold as U

main :: IO ()
main = do
  arg <- Cli.args
  when arg.verbose (print arg)

  csvCounter <- newIORef (0 :: Int)

  S.repeatM B.getLine
    & S.trace (increment csvCounter)
    & fmap C.fromStrict
    & S.mapM (\l -> case (Csv.decode NoHeader l :: Either String (V.Vector LogRow)) of
                      Left e  -> print e >> pure []
                      Right v -> pure $ V.toList v
             )
    & S.filter (not . P.null)
    & S.trace (stderr' . show)
    & S.unfoldMany U.fromList
    & fmap (\l -> (l.ackId, T.encodeUtf8 l.payload, l.timestamp))
    & fmap (\(aid, l, t) -> (aid, C.fromStrict l, t))
    & fmap (\(aid, l, t) -> (aid, (A.decode l) :: Maybe Object, t))
    & S.filter (\(_, l, _) -> isJust l)
    & fmap (\(aid, o, t) -> (aid, fromJust o, t))
    & fmap (\(aid, o, t) -> (aid, M.lookup "jsonPayload" o, M.lookup "logName" o, M.lookup "resource" o, t))
    & S.filter (\(_, p, n, r, _) -> isJust p && isJust n && isJust r)
    & fmap (\(aid, p, n, r, t) -> (aid, fromJust p, fromJust n, fromJust r, t))
    & fmap (\(aid, Object p, n, Object r, t) -> (aid, M.lookup "labels" r, n, p, t))
    & S.filter (\(_, l, _, _, _) -> isJust l)
    & fmap (\(aid, l, n, p, t) -> (aid, fromJust l, n, p, t))
    & fmap (\(aid, Object l, String n, p, t) -> (aid, M.lookup "project_id" l, P.last (T.splitOn "/" n), p, t))
    & S.filter (\(_, pid, _, _, _) -> isJust pid)
    -- & S.trace (\t -> hPutStrLn stderr (show t))
    & fmap (\(aid, pid, n, p, t) -> (aid, fromJust pid, n, p, t))
    & fmap (\(aid, String pid, n, p, t) -> (aid, pid, n, A.encode p, t))
    & S.mapM (\(aid, pid, n, p, t) -> do
                r <- inspect (T.decodeUtf8 (B.toStrict p))
                pure (aid, pid, n, r, p, t)
             )
    & fmap (\(aid, pid, n, r, p, t) -> (aid, pid, n, toFindings r, p, t))
    & S.trace (stderr' . show)
    & S.filter (\(_, _, _, fs, _, _) -> (not . P.null) fs)
    & fmap (\(aid, pid, n, fs, p, t) -> (\f -> (aid, pid, n, f, p, t)) <$> fs)
    & S.unfoldMany U.fromList    
    & fmap (\(aid, pid, n, f, p, t) -> (aid, pid, n, f.quote, f.infoType, f.likelihood, f.location, p, t))
    & S.filter (\(_, _, _, q, i, l, loc, _, _) -> isJust q && isJust i && isJust l && isJust loc)
    & fmap (\(aid, pid, n, q, i, l, loc, p, t) -> (aid, pid, n, fromJust q, fromJust i, fromJust l, fromJust loc, p, t))
    & fmap (\(aid, pid, n, q, i, l, loc, p, t) -> (aid, pid, n, q, i.name, l.fromGooglePrivacyDlpV2Finding_Likelihood, loc.byteRange, p, t))
    & S.filter (\(_, _, _, _, i, _, r, _, _) -> isJust i && isJust r)
    & fmap (\(aid, pid, n, q, i, l, r, p, t) -> (aid, pid, n, q, fromJust i, l, fromJust r, p, t))
    & fmap (\(aid, pid, n, q, i, l, r, p, t) -> (aid, pid, n, q, i, l, (r.start, r.end), p, t))
    & S.filter (\(_, _, _, _, _, _, (s, e), _, _) -> isJust s && isJust e)
    & fmap (\(aid, pid, n, q, i, l, (s, e), p, t) -> (aid, pid, n, q, i, l, (fromJust s, fromJust e), p, t))
    & fmap (\(aid, pid, n, q, i, l, pair, p, t) -> (aid, pid, n, q, i, l, pad pair p, (decodeUtf8 . B.toStrict) p, t))
    & fmap (\(aid, pid, n, q, i, l, pair, p, t) -> Log (BH.DocId aid) pid n p ((T.pack . show) t) (Just q) (Just i) (Just l) (Just pair))
    & fmap (\l -> Csv.encode [l])
    & S.mapM (IO.putStr . T.decodeUtf8 . B.toStrict)
    & S.fold F.drain

  csvs <- readIORef csvCounter

  S.fromList [csvs]
    & fmap show
    & S.mapM (hPutStrLn stderr)
    & S.fold F.drain

  where
    toFindings :: GooglePrivacyDlpV2InspectContentResponse ->  [GooglePrivacyDlpV2Finding]
    toFindings (GooglePrivacyDlpV2InspectContentResponse r) = case r of
      Just (GooglePrivacyDlpV2InspectResult (Just fs) _) -> fs
      Just (GooglePrivacyDlpV2InspectResult Nothing _)   -> []
      Nothing                                            -> []

    pad :: (Int64, Int64) -> C.ByteString -> (Int, Int)
    pad (s, e) p = let s' = fromIntegral (s - 21) :: Int
                       e' = fromIntegral (e + 21) :: Int
                       p' = B.toStrict p
                       len = B.length p'
                   in ( if s' < 0 then 0 else s'
                      , if e' > len then len else e'
                      )

    increment :: IORef Int -> a -> IO ()
    increment r _ = modifyIORef' r (+1)

    stderr' :: String -> IO ()
    stderr' = hPutStrLn stderr
