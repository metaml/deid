module Etc.Deid where

import Data.Set as S
import Data.Text
import GHC.Generics
import Options.Applicative

data Arg = Arg { server :: Text
               , port :: Int
               , query :: Query
               , maxResults :: Int
               , verbose :: Bool
               , debug :: Bool
               }
           deriving (Eq, Generic, Show)

data Indices = IndicesAll | Indices (Set Text)
  deriving (Eq, Show)

data Query = Query Indices
  deriving (Eq, Show)

data Verbosity = Quiet | Debug | Verbose
  deriving (Eq, Ord, Show)

arg :: IO Arg
arg = execParser parser

parser :: ParserInfo Arg
parser = info (parseArg <**> helper) (fullDesc <> header "deid"
                                               <> progDesc "detect PII data in ES"
                                     )

parseArg :: Parser Arg
parseArg = Arg <$> strOption (long "server"
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
               <*> option parseQuery ( long "indices"
                                       <> short 'i'
                                       <> value (Query IndicesAll)
                                       <> showDefault
                                       <> metavar "INDICES"
                                       <> help "ES comma separated indices"
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
  where parseQuery :: ReadM Query
        parseQuery = eitherReader $ \s -> let is = splitOn "," (pack s)
                                          in Right $ Query $ Indices (fromList is)
