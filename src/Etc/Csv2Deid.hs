module Etc.Csv2Deid where

import GHC.Generics
import Options.Applicative
import Prelude as P

data Arg = Arg { verbose :: Bool
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
parseArg = Arg <$> switch ( long "verbose"
                            <> short 'v'
                            <> help "verbose (stderr)"
                          )
               <*> switch ( long "debug"
                            <> short 'd'
                            <> help "debug (stderr; very noisy)"
                          )
