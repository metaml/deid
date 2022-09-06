module Etc.Deid where

import Data.Set as S
import Data.Text
import GHC.Generics
import Options.Applicative

data Arg = Arg { server :: Text
               , port :: Int
               , query :: Query
               , results :: Int
               , verbosity :: Set Verbosity
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
parseArg = Arg <$> strOption  ( long "server"
                                <> value "localhost"
                                <> showDefault
                                <> short 's'
                                <> metavar "SERVER"
                                <> help "ES server, e.g.: slor-esc100"
                              )
               <*> option auto ( long "port"
                                 <> value 9200
                                 <> showDefault
                                 <> short 'p'
                                 <> metavar "PORT"
                                 <> help "ES port, e.g.: 9200"
                               )
               <*> option parseQuery ( long "indices"
                                       <> value (Query IndicesAll)
                                       <> showDefault
                                       <> short 'i'
                                       <> metavar "INDICES"
                                       <> help "ES comma separated indices"
                                     )
               <*> option auto ( long "max"
                                 <> value 3
                                 <> showDefault
                                 <> short 'm'
                                 <> metavar "MAXRESULTS"
                                 <> help "max number of results from each index"
                               )
               <*> option parseVerbosity ( long "verbose"
                                           <> value (S.empty)
                                           <> showDefault
                                           <> short 'v'
                                           <> metavar "VERBOSITY"
                                           <> help ("verbosity modes: " <> show [Debug, Verbose])
                                         )
  where parseQuery :: ReadM Query
        parseQuery = eitherReader $ \s -> let is = splitOn "," (pack s)
                                          in Right $ Query $ Indices (fromList is)
        parseVerbosity :: ReadM (Set Verbosity)
        parseVerbosity = eitherReader $ \s -> let vs = toVerbosity <$> splitOn "," (toLower . pack $ s)
                                                  vs' = delete Quiet $ fromList vs
                                              in Right vs'
        toVerbosity :: Text -> Verbosity
        toVerbosity s = case s of
          "verbose" -> Verbose
          "debug"   -> Debug
          _         -> Quiet
