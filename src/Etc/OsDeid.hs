module Etc.OsDeid where

import Data.Set as S
import Data.Text
import GHC.Generics
import Options.Applicative

data Arg = Arg { owner :: Text
               , service :: Text
               , server :: Text
               , port :: Int
               , maxResults :: Int
               , verbose :: Bool
               , debug :: Bool
               }
           deriving (Eq, Generic, Show)

data Indices = IndicesAll | Indices (Set Text)
  deriving (Eq, Show)

data Query = Query Indices
  deriving (Eq, Show)

arg :: IO Arg
arg = execParser parser

parser :: ParserInfo Arg
parser = info (parseArg <**> helper) (fullDesc <> header "deid"
                                               <> progDesc "detect PII data in ES or PubSub"
                                     )

parseArg :: Parser Arg
parseArg = Arg <$> strOption (long "owner"
                              <> short 'o'
                              <> metavar "OWNER"
                              <> help "lp_owner"
                             )
               <*> strOption (long "service"
                              <> short 'n'
                              <> metavar "SERVICE_NAME"
                              <> help "service_name"
                             )
               <*> strOption (long "server"
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
               <*> option auto ( long "max"
                                 <> short 'm'
                                 <> value 3
                                 <> showDefault
                                 <> metavar "MAXRESULTS"
                                 <> help "max results from each index"
                               )
               <*> switch ( long "verbose"
                            <> short 'v'
                            <> help "verbose (stderr)"
                          )
               <*> switch ( long "debug"
                            <> short 'd'
                            <> help "debug (stderr; very noisy)"
                          )
