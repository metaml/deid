module Etc.Redact where

import Data.Text
import GHC.Generics
import Options.Applicative

type DocId = Text
type IndexName = Text

data Arg = Arg { index :: IndexName
               , docId :: Text
               , dlp :: Bool
               , regex :: Bool
               , server :: Text
               , port :: Int
               , verbose :: Bool
               }
           deriving (Eq, Generic, Show)

arg :: IO Arg
arg = execParser parser

parser :: ParserInfo Arg
parser = info (parseArg <**> helper) (fullDesc <> header "deid"
                                               <> progDesc "detect PII data in ES or PubSub"
                                     )

parseArg :: Parser Arg
parseArg = Arg <$> strOption ( long "index"
                               <> short 'i'
                               <> metavar "INDEX"
                               <> help "index name"
                             )
               <*> strOption ( long "id"
                               <> short 'd'
                               <> metavar "DOCUMENT_ID"
                               <> help "doc ID"
                             )
               <*> switch ( long "dlp"
                            <> short 'p'
                            <> help "DLP redaction"
                          )
               <*> switch ( long "regex"
                            <> short 'r'
                            <> help "regex redaction"
                          )
               <*> strOption ( long "server"
                               <> short 's'
                               <> value "localhost"
                               <> showDefault
                               <> metavar "SERVER"
                               <> help "ESC server"
                             )
               <*> option auto ( long "port"
                                 <> short 'p'
                                 <> value 9200
                                 <> showDefault
                                 <> metavar "PORT"
                                 <> help "ES port"
                               )
               <*> switch ( long "verbose"
                            <> short 'v'
                            <> help "verbose (stderr)"
                          )
