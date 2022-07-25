module Model.BigQuery where

import Data.Aeson (Object)
import Data.Aeson.KeyMap (toHashMapText)
import Gcp.Send (send')
import Gogol.BigQuery
import Gogol.Prelude

type DataSetId = Text
type ProjectId = Text
type TableId = Text

tableInsert :: DataSetId -> [Object] -> ProjectId -> TableId -> IO (Rs BigQueryTabledataInsertAll)
tableInsert did obs pid tid = do
  let os = (\o -> newTableDataInsertAllRequest_RowsItem { insertId = Nothing, json = Just (newJsonObject . toHashMapText $ o) }) <$> obs
      irq = mkTableDataInsertAllRequest os
  tableInsert' did irq pid tid

tableInsert' :: DataSetId -> TableDataInsertAllRequest -> ProjectId -> TableId -> IO (Rs BigQueryTabledataInsertAll)
tableInsert' did irq pid tid = do
  let r = newBigQueryTabledataInsertAll did irq pid tid
      p = Proxy :: Proxy '[Bigquery'FullControl]
  send' r p

mkTableDataInsertAllRequest :: [TableDataInsertAllRequest_RowsItem] -> TableDataInsertAllRequest
mkTableDataInsertAllRequest rs = newTableDataInsertAllRequest { rows = Just rs }

tablesList :: DataSetId -> ProjectId -> IO (Rs BigQueryTablesList)
tablesList did pid = do
  let r = newBigQueryTablesList did pid
      p = Proxy :: Proxy '[Bigquery'FullControl]
  send' r p

tablesGet :: DataSetId -> ProjectId -> TableId -> IO (Rs BigQueryTablesGet)
tablesGet did pid tid = do
  let r = newBigQueryTablesGet did pid tid
      p = Proxy :: Proxy '[Bigquery'FullControl]
  send' r p
