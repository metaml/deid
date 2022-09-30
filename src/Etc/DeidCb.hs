module Etc.DeidCb where

import Data.Int
import Data.Text
import GHC.Generics
import Model.PubSub (subscription)
import Options.Applicative
import Prelude as P

data Arg = Arg { project :: Text
               , subs :: [Text]
               , maxResults :: Int32
               , verbose :: Bool
               , debug :: Bool
               }
           deriving (Eq, Generic, Show)

args :: IO Arg
args = do
  a <- execParser parser
  let subs' = if P.null a.subs
              then cbSubs a.project
              else subscription a.project <$> a.subs
  pure $ a { subs = subs' }
  where
    cbSubs :: Text -> [Text]
    cbSubs p = subscription p <$> [ "lpgprj-p-logapp-usea1-gkelogs-deid-sub"
                                  , "lpgprj-p-logapp-usea1-vmlogs-deid-sub"
                                  , "lpgprj-p-logapp-usea1-gkelogs-deid-sub"
                                  , "lpgprj-p-logapp-usea1-vmlogs-deid-sub"
                                  ]

parser :: ParserInfo Arg
parser = info (parseArg <**> helper) (fullDesc <> header "deid-cb"
                                               <> progDesc "detect PII data in CB's logs in PubSub"
                                     )

parseArg :: Parser Arg
parseArg = Arg <$> strOption ( long "project"
                               <> short 'p'
                               <> value "lpgprj-gss-p-ctrlog-gl-01"
                               <> showDefault
                               <> metavar "PROJECT"
                               <> help "GCP project"
                             )
               <*> (many . strOption) ( long "sub"
                                        <> short 's'
                                        <> metavar "SUB"
                                        <> help "GCP PubSub subscription(s)"
                                      )
               <*> option auto ( long "max"
                                 <> short 'm'
                                 <> value 1
                                 <> showDefault
                                 <> metavar "MAXRESULTS"
                                 <> help "max responses for each pull request"
                               )
               <*> switch ( long "verbose"
                            <> short 'v'
                            <> help "verbose (stderr)"
                          )
               <*> switch ( long "debug"
                            <> short 'd'
                            <> help "debug (stderr; very noisy)"
                          )
