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
import qualified Data.Vector as V
import qualified Streamly.Data.Stream as S

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
         & fmap (\(ixs', y) -> ((L.nub $ (P.head . T.splitOn (tshow y)) <$> ixs'), y-1))
         & fmap (\(ixs', y) -> ((L.nub $ (P.head . T.splitOn (tshow y)) <$> ixs'), y-1))
         & fmap (\(ixs', y) -> ((L.nub $ (P.head . T.splitOn (tshow y)) <$> ixs'), y-1))
         & fmap (\(ixs', _) -> ((L.nub . L.sort) ixs'))
         & S.toList
  ixss <- S.fromPure (P.concat ips, ixs)
          & fmap (\(ips', ixs') -> (\ip -> (P.filter (\ix' -> ip `isPrefixOf` ix') ixs')) <$> ips')
          & fmap (\ixss -> P.filter (\ixs' -> (not . P.null) ixs') ixss)
          & fmap (\ixss -> if P.null ixss
                            then []
                            else (P.last . sort) <$> ixss
                  )
          & fmap (\ixss -> IndexName <$> ixss)
          & S.toList
  pure $ P.concat ixss

type FromPos = Int
type HitSize = Int

document :: Server -> IndexName -> DocId  -> IO [ParsedEsResponse (SearchResult Value)]
document srv ix' id' = S.fromPure (filter', spec)
                       & fmap (\(f, s) -> (QueryBoolQuery $ mkBoolQuery [] [f] [] [], f, s))
                       & fmap (\(q, f, s) -> (mkSearch (Just q) (Just f)) { sortBody = Just [s] })
                       & S.mapM (\s -> withBH defaultManagerSettings srv (searchByIndex ix' s))
                       & S.mapM parseEsResponse
                       & S.toList
  where filter' = Filter $ IdsQuery [id']
        spec = DefaultSortSpec $ mkSort (FieldName "@timestamp") Descending

documents :: Server -> IndexName -> FromPos -> HitSize -> IO [ParsedEsResponse (SearchResult Value)]
documents srv ix' from' size' = S.fromPure (filter', spec)
                                & fmap (\(f, s) -> (QueryBoolQuery $ mkBoolQuery [] [f] [] [], f, s))
                                & fmap (\(q, f, s) -> (mkSearch (Just q) (Just f)) { sortBody = Just [s] })
                                & fmap (pageSearch (From from') (Size size'))
                                & S.mapM (\s -> withBH defaultManagerSettings srv (searchByIndex ix' s))
                                & S.mapM parseEsResponse
                                & S.toList
  where filter' = Filter $ MatchAllQuery Nothing
        spec = DefaultSortSpec $ mkSort (FieldName "@timestamp") Descending

type LpOwner' = Text
type ServiceName' = Text

documents' :: Server -> LpOwner' -> ServiceName' -> FromPos -> HitSize -> IO [ParsedEsResponse (SearchResult Value)]
documents' srv o n from' size' = S.fromPure (fs' o n, spec)
                                 & fmap (\(fs, s) -> (QueryBoolQuery $ mkBoolQuery [] fs [] [], f', s))
                                 & fmap (\(q, f, s) -> (mkSearch (Just q) (Just f)) { sortBody = Just [s] })
                                 & fmap (pageSearch (From from') (Size size'))
                                 & S.mapM (\s -> withBH defaultManagerSettings srv (searchAll s))
                                 & S.mapM parseEsResponse
                                 & S.toList
  where fs' owner service = [ Filter $ TermQuery (Term "lp_owner.keyword" owner) Nothing
                            , Filter $ TermQuery (Term "service_name.keyword" service) Nothing
                            ]
        f' = Filter $ MatchAllQuery Nothing
        spec = DefaultSortSpec $ mkSort (FieldName "@timestamp") Descending

ownerServiceNames :: Server -> IndexName -> IO [ParsedEsResponse (SearchResult Value)]
ownerServiceNames srv idx = do
  let q = MatchAllQuery Nothing
      src = SourceIncludeExclude (Include [ Pattern "lp_owner.keyword"
                                          , Pattern "servce_name.keyword"
                                          , Pattern "message"
                                          , Pattern "@timestamp"
                                         ]
                                 )
                                 (Exclude [])
      s = (mkSearch (Just q) Nothing) { source = Just src
                                      , size = Size 10000
                                      }
  r <- withBH defaultManagerSettings srv (searchByIndex idx s)
  pure $ parseEsResponse r

search :: Server -> FieldName -> QueryString -> [IndexName] -> IO [ParsedEsResponse (SearchResult Value)]
search srv fn qs ixs = S.fromPure (query, filter', spec)
                       & fmap (\(q, f, s) -> (QueryBoolQuery $ mkBoolQuery [q] [] [] [], f, s))
                       & fmap (\(q, f, s) -> (mkSearch (Just q) (Just f)) { sortBody = Just [s] })
                       & fmap (\s -> (srv, pageSearch (From 0) (Size 1) s, ixs))
                       & S.mapM (\(srv', s, ixs') -> if L.null ixs'
                                                     then withBH defaultManagerSettings srv' (searchAll s)
                                                     else withBH defaultManagerSettings srv' (searchByIndices (N.fromList ixs) s)
                                )
                       & S.mapM parseEsResponse -- (parseEsResponse :: (MonadThrow m, FromJSON body) => BHResponse body -> m (ParsedEsResponse body))
                       & S.toList
  where query = QueryMatchQuery $ (mkMatchQuery fn qs) { matchQueryOperator = And }
        filter' = Filter $ MatchAllQuery Nothing
        spec = DefaultSortSpec $ mkSort (FieldName "@timestamp") Descending

update :: Server -> IndexName -> DocId -> Value -> IO (BHResponse a)
update srv idx did v = let b = BulkUpdate idx did v
                       in withBH defaultManagerSettings srv (bulk (V.singleton b))

update' :: Server -> [BulkOperation] -> IO (BHResponse a)
update' srv bs = withBH defaultManagerSettings srv (bulk (V.fromList bs))

server :: Text -> Int -> Server
server s p  = Server $ T.concat ["http://", s, ":", (pack . show) p]

toText :: Value -> Text
toText = T.decodeUtf8 . B.toStrict . encode

tshow :: Show a => a -> Text
tshow = T.pack . show
