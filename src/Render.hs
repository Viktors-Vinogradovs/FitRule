{-# LANGUAGE OverloadedStrings #-}

module Render
  ( renderResult
  ) where

import Domain
import Data.Text (Text)
import qualified Data.Text as T

renderResult :: [Violation] -> Text
renderResult vs
  | null vs   = T.unlines
      [ "================================"
      , "  [VALID] PLAN PASSED"
      , "================================"
      , "All rules satisfied."
      , ""
      ]
  | otherwise = T.unlines $
      [ "================================"
      , "  [INVALID] PLAN FAILED"
      , "================================"
      , "Violations found:"
      , ""
      ] ++ map renderViolation vs ++ [""]

renderViolation :: Violation -> Text
renderViolation (Violation rule mDay reason) =
  let dayTxt :: Text
      dayTxt = maybe "" (\d -> T.pack (" [" ++ show d ++ "]")) mDay
      ruleName :: Text
      ruleName = case rule of
        MaxSetsPerDay _ -> "MaxSetsPerDay"
        MinRestDays _ -> "MinRestDays"
        NoSameMuscleConsecutive -> "NoSameMuscleConsecutive"
        AllowedMuscles _ -> "AllowedMuscles"
        MaxSetsPerMusclePerWeek m _ -> T.pack ("MaxSetsPerMusclePerWeek(" ++ show m ++ ")")
        MaxTrainingDaysPerWeek _ -> "MaxTrainingDaysPerWeek"
        RequireMuscleSplit _ -> "RequireMuscleSplit"
  in T.concat
      [ "  * "
      , ruleName
      , dayTxt
      , ": "
      , reason
      ]