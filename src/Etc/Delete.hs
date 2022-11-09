module Etc.Delete where

import Data.Text
import GHC.Generics
import Options.Applicative
import Prelude as P

type DocId = Text
type IndexName = Text

data Arg = Arg { index :: IndexName
               , docId :: DocId
               , verbose :: Bool
               , debug :: Bool
               }
           deriving (Eq, Generic, Show)

args :: IO Arg
args = execParser parser

parser :: ParserInfo Arg
parser = info (parseArg <**> helper) (fullDesc <> header "cvs2deid"
                                               <> progDesc "detect PII data from stdin"
                                     )

parseArg :: Parser Arg
parseArg = Arg <$> strOption ( long "index"
                               <> short 'i'
                               <> metavar "INDEX"
                               <> help "name of ES index"
                             )
               <*> strOption ( long "id"
                               <> short 'd'
                               <> metavar "DOCUMENT_ID"
                               <> help "id of document"
                             )
               <*> switch ( long "verbose"
                            <> short 'v'
                            <> help "verbose (stderr)"
                          )
               <*> switch ( long "debug"
                            <> short 'd'
                            <> help "debug (stderr; very noisy)"
                          )
