{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Options.Applicative

import Lib

data Result = 
  Result { original :: T.Text
         , fullParse :: Bool
         , tree :: [Tree Symbol]
         } deriving (Generic, Show)

instance A.ToJSON Result

sentence :: CFG ChomskyRule -> T.Text -> Result
sentence gr original = 
  case cky gr (sentenceTerminals original) of
    Left partial -> do
      Result original False partial 
    Right full -> do
      Result original True full 

data Options = Options
  { grammarFile   :: FilePath
  , sentencesFile :: FilePath
  }

sample :: Parser Options
sample = Options
      <$> strOption
          ( long "grammmar"
         <> short 'g'
         <> help "Grammar" )
      <*> strOption
          ( long "sentences"
         <> short 's'
         <> help "Sentences" )

-- | Remove leading and trailing space (including newlines) from string.
-- (from pandoc)
trim :: T.Text -> T.Text
trim = T.dropAround C.isSpace

process :: Options -> IO ()
process (Options {..}) = do
  gr <- chomsky . parseGrammar <$> T.readFile grammarFile
  sens <- map trim . T.lines <$> T.readFile sentencesFile
  B.putStr $ A.encodePretty $ map (sentence gr) sens

main :: IO ()
main = process =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "You can parse sentences according to a grammar."
     <> header "A CYK parser" )
