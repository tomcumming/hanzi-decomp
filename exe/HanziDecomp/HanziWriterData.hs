module HanziDecomp.HanziWriterData
  ( Character (..),
    loadFromFile,
  )
where

import Control.Monad ((>=>))
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.Text.IO qualified as T

data Character = Character
  { chrStrokes :: ![T.Text],
    chrMedians :: ![[(Double, Double)]]
  }
  deriving Show

instance Aeson.FromJSON Character where
  parseJSON = Aeson.withObject "Character" $ \obj ->
    Character
      <$> obj Aeson..: "strokes"
      <*> obj Aeson..: "medians"

loadFromFile :: FilePath -> IO Character
loadFromFile = T.readFile >=> Aeson.throwDecodeStrictText
