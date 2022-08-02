module Model.Elastic where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Sort
import Data.Text as T hiding (filter)
import Data.Text.Encoding as T
import Data.Time.Calendar
import Data.Time.Clock
import Database.Bloodhound hiding (key)
import Network.HTTP.Client
import Prelude as P
import qualified Data.ByteString.Lazy as B
import qualified Data.List as L
import qualified Data.List.NonEmpty as N
import qualified Streamly.Prelude as S

indexes :: Server -> IO [IndexName]
indexes s = withBH defaultManagerSettings s $ do
  ins <- listIndices
  liftIO $ pure ins

currentIndexes :: Server -> IO [IndexName]
currentIndexes s = do
  (year, _, _) <- (toGregorian . utctDay) <$> getCurrentTime
  idxs <- indexes s
  let ixs = (\(IndexName i) -> i) <$> idxs
  ips <- S.fromPure (ixs, year)
         & S.map (\(ixs', y) -> ((L.nub $ (P.head . T.splitOn (tshow y)) <$> ixs'), y-1))
         & S.map (\(ixs', y) -> ((L.nub $ (P.head . T.splitOn (tshow y)) <$> ixs'), y-1))
         & S.map (\(ixs', y) -> ((L.nub $ (P.head . T.splitOn (tshow y)) <$> ixs'), y-1))
         & S.map (\(ixs', _) -> ((L.nub . L.sort) ixs'))
         & S.toList
  ixss <- S.fromPure (P.concat ips, ixs)
          & S.map (\(ips', ixs') -> (\ip -> (P.filter (\ix' -> ip `isPrefixOf` ix') ixs')) <$> ips')
          & S.map (\ixss -> P.filter (\ixs' -> (not . P.null) ixs') ixss)
          & S.map (\ixss -> if P.null ixss
                            then []
                            else (P.last . sort) <$> ixss
                  )
          & S.map (\ixss -> IndexName <$> ixss)
          & S.toList
  pure $ P.concat ixss

documents :: Server -> IndexName -> Int -> Int -> IO [ParsedEsResponse (SearchResult Value)]
documents srv ix' from' size' = S.fromPure (filter', spec)
                                & S.map (\(f, s) -> (QueryBoolQuery $ mkBoolQuery [] [f] [] [], f, s))
                                & S.map (\(q, f, s) -> (mkSearch (Just q) (Just f)) { sortBody = Just [s] })
                                & S.map (pageSearch (From from') (Size size'))
                                & S.mapM (\s -> withBH defaultManagerSettings srv (searchByIndex ix' s))
                                & S.mapM parseEsResponse
                                & S.toList
  where filter' = Filter $ MatchAllQuery Nothing
        spec = DefaultSortSpec $ mkSort (FieldName "@timestamp") Descending

search :: Server -> FieldName -> QueryString -> [IndexName] -> IO [ParsedEsResponse (SearchResult Value)]
search srv fn qs ixs = S.fromPure (query, filter', spec)
                       & S.map (\(q, f, s) -> (QueryBoolQuery $ mkBoolQuery [q] [] [] [], f, s))
                       & S.map (\(q, f, s) -> (mkSearch (Just q) (Just f)) { sortBody = Just [s] })
                       & S.map (\s -> (srv, pageSearch (From 0) (Size 1) s, ixs))
                       & S.mapM (\(srv', s, ixs') -> if L.null ixs'
                                                     then withBH defaultManagerSettings srv' (searchAll s)
                                                     else withBH defaultManagerSettings srv' (searchByIndices (N.fromList ixs) s)
                                )
                       & S.mapM parseEsResponse -- (parseEsResponse :: (MonadThrow m, FromJSON body) => BHResponse body -> m (ParsedEsResponse body))
                       & S.toList
  where query = QueryMatchQuery $ (mkMatchQuery fn qs) { matchQueryOperator = And }
        filter' = Filter $ MatchAllQuery Nothing
        spec = DefaultSortSpec $ mkSort (FieldName "@timestamp") Descending

server :: Text -> Int -> Server
server s p  = Server $ T.concat ["http://", s, ":", (pack . show) p]

toText :: Value -> Text
toText = T.decodeUtf8 . B.toStrict . encode

tshow :: Show a => a -> Text
tshow = T.pack . show
