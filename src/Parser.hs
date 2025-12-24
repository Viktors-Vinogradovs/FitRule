{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseRulesFile
  , parsePlanFile
  ) where

import Domain
import Control.Monad (void)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type P = Parsec Void Text

-- Public API

parseRulesFile :: FilePath -> IO (Either Text [Rule])
parseRulesFile fp = do
  txt <- TIO.readFile fp
  pure $ case parse (rulesP <* eof) fp txt of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right rs -> Right rs

parsePlanFile :: FilePath -> IO (Either Text Plan)
parsePlanFile fp = do
  txt <- TIO.readFile fp
  pure $ case parse (planP <* eof) fp txt of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right pairs -> Right (normalizePlan pairs)

-- Space consumer: handles spaces and tabs ONLY (not newlines)
sc :: P ()
sc = L.space space1' empty empty
  where
    space1' = void $ takeWhile1P (Just "space") (\c -> c == ' ' || c == '\t')

lexeme :: P a -> P a
lexeme = L.lexeme sc

symbol :: Text -> P Text
symbol = L.symbol sc

-- Rules DSL

rulesP :: P [Rule]
rulesP = do
  rules <- many (try blankLine <|> (Just <$> ruleLineP <* optional eol))
  eof
  pure (catMaybes rules)
  where
    blankLine = sc *> eol *> pure Nothing
    catMaybes = foldr (\x acc -> case x of Just v -> v:acc; Nothing -> acc) []

ruleLineP :: P Rule
ruleLineP = do
  sc  -- consume leading spaces
  r <- choice
    [ try maxSetsPerMusclePerWeekP
    , try maxTrainingDaysPerWeekP
    , try requireMuscleSplitP
    , try maxSetsP
    , try minRestP
    , try noSameMuscleP
    , allowedMusclesP
    ]
  sc  -- consume trailing spaces
  optional (char '#' *> takeWhileP (Just "comment") (/= '\n'))
  pure r

maxSetsP :: P Rule
maxSetsP = do
  _ <- symbol "MAX_SETS_PER_DAY"
  n <- lexeme L.decimal
  pure (MaxSetsPerDay n)

minRestP :: P Rule
minRestP = do
  _ <- symbol "MIN_REST_DAYS"
  n <- lexeme L.decimal
  pure (MinRestDays n)

noSameMuscleP :: P Rule
noSameMuscleP = do
  _ <- symbol "NO_SAME_MUSCLE_CONSECUTIVE"
  pure NoSameMuscleConsecutive

allowedMusclesP :: P Rule
allowedMusclesP = do
  _ <- symbol "ALLOWED_MUSCLES"
  ms <- some (lexeme muscleP)
  pure (AllowedMuscles ms)

maxSetsPerMusclePerWeekP :: P Rule
maxSetsPerMusclePerWeekP = do
  _ <- symbol "MAX_SETS_PER_MUSCLE_PER_WEEK"
  m <- lexeme muscleP
  n <- lexeme L.decimal
  pure (MaxSetsPerMusclePerWeek m n)

maxTrainingDaysPerWeekP :: P Rule
maxTrainingDaysPerWeekP = do
  _ <- symbol "MAX_TRAINING_DAYS_PER_WEEK"
  n <- lexeme L.decimal
  pure (MaxTrainingDaysPerWeek n)

requireMuscleSplitP :: P Rule
requireMuscleSplitP = do
  _ <- symbol "REQUIRE_MUSCLE_SPLIT"
  ms <- some (lexeme muscleP)
  pure (RequireMuscleSplit ms)

-- Plan DSL

planP :: P [(Day, DayPlan)]
planP = do
  pairs <- many (try blankLine <|> (Just <$> dayLineP <* optional eol))
  eof
  pure (catMaybes pairs)
  where
    blankLine = sc *> eol *> pure Nothing
    catMaybes = foldr (\x acc -> case x of Just v -> v:acc; Nothing -> acc) []

dayLineP :: P (Day, DayPlan)
dayLineP = do
  sc  -- consume leading spaces
  d <- dayP
  sc
  _ <- char ':'
  sc
  dp <- try restP <|> trainingP
  sc
  optional (char '#' *> takeWhileP (Just "comment") (/= '\n'))
  pure (d, dp)

restP :: P DayPlan
restP = do
  _ <- string "Rest"
  pure RestDay

trainingP :: P DayPlan
trainingP = do
  exs <- exerciseP `sepBy1` (sc *> char ';' <* sc)
  pure (TrainingDay exs)

exerciseP :: P Exercise
exerciseP = do
  name <- wordP
  sc
  mus <- muscleP
  sc
  sets <- L.decimal
  pure (Exercise name mus sets)

-- Atoms

dayP :: P Day
dayP = choice
  [ Mon <$ string "Mon"
  , Tue <$ string "Tue"
  , Wed <$ string "Wed"
  , Thu <$ string "Thu"
  , Fri <$ string "Fri"
  , Sat <$ string "Sat"
  , Sun <$ string "Sun"
  ]

muscleP :: P Muscle
muscleP = choice
  [ Chest     <$ string "Chest"
  , Back      <$ string "Back"
  , Legs      <$ string "Legs"
  , Shoulders <$ string "Shoulders"
  , Arms      <$ string "Arms"
  , Core      <$ string "Core"
  ]

wordP :: P Text
wordP = T.pack <$> some (satisfy isWordChar)
  where
    isWordChar c = isAlphaNum c || c == '_' || c == '-'