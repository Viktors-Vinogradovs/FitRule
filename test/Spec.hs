{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Domain
import Validate
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

-- Generator for valid exercises
genExercise :: Gen Exercise
genExercise = do
  name <- elements ["BenchPress", "Squat", "PullUps", "Dips", "Row", "Press"]
  muscle <- elements [Chest, Back, Legs, Shoulders, Arms, Core]
  sets <- choose (1, 5)
  return $ Exercise (T.pack name) muscle sets

-- Generator for day plans
genDayPlan :: Gen DayPlan
genDayPlan = frequency
  [ (3, return RestDay)
  , (7, TrainingDay <$> listOf1 genExercise)
  ]

-- Generator for plans (always full week via normalization)
genPlan :: Gen Plan
genPlan = do
  days <- vectorOf 7 genDayPlan
  return $ normalizePlan (zip [Mon .. Sun] days)

-- Generator that INTENTIONALLY violates MaxSetsPerDay
genPlanWithTooManySets :: Int -> Gen Plan
genPlanWithTooManySets limit = do
  -- Create one day with too many sets
  violatingDay <- do
    numExercises <- choose (3, 6)
    exs <- vectorOf numExercises $ do
      name <- elements ["BenchPress", "Squat", "PullUps"]
      muscle <- elements [Chest, Back, Legs]
      sets <- choose (3, 5)
      return $ Exercise (T.pack name) muscle sets
    -- Ensure total exceeds limit
    let total = sum (map exSets exs)
    if total > limit
      then return (TrainingDay exs)
      else return (TrainingDay (exs ++ [Exercise "Extra" Chest (limit - total + 1)]))
  
  -- Fill rest of week
  otherDays <- vectorOf 6 genDayPlan
  day <- elements allDays
  return $ normalizePlan ((day, violatingDay) : zip (filter (/= day) allDays) otherDays)

-- Generator that violates consecutive muscle rule
genPlanWithConsecutiveMuscle :: Gen Plan
genPlanWithConsecutiveMuscle = do
  muscle <- elements [Chest, Back, Legs]
  day1 <- TrainingDay <$> listOf1 (genExerciseForMuscle muscle)
  day2 <- TrainingDay <$> listOf1 (genExerciseForMuscle muscle)
  otherDays <- vectorOf 5 genDayPlan
  return $ normalizePlan $
    (Mon, day1) : (Tue, day2) : zip [Wed .. Sun] otherDays
  where
    genExerciseForMuscle m = do
      name <- elements ["Exercise1", "Exercise2"]
      sets <- choose (1, 4)
      return $ Exercise (T.pack name) m sets

-- Property: MaxSetsPerDay with targeted violation
prop_maxSetsDetectsViolations :: Property
prop_maxSetsDetectsViolations = forAll (genPlanWithTooManySets 12) $ \plan ->
  let rule = MaxSetsPerDay 12
      violations = validate [rule] plan
  in not (null violations)  -- Must detect the violation

-- Property: Valid rest days count
prop_minRestDaysWorks :: Property
prop_minRestDaysWorks = forAll genPlan $ \plan ->
  let rule = MinRestDays 2
      violations = validate [rule] plan
      Plan dayMap = plan
      restCount = length [() | RestDay <- Map.elems dayMap]
  in (restCount >= 2) == null violations

-- Property: Consecutive muscle detection
prop_noConsecutiveMuscleDetects :: Property
prop_noConsecutiveMuscleDetects = forAll genPlanWithConsecutiveMuscle $ \plan ->
  let violations = validate [NoSameMuscleConsecutive] plan
  in not (null violations)  -- Must detect consecutive muscle

-- Property: New rule - max training days per week
prop_maxTrainingDaysWorks :: Property
prop_maxTrainingDaysWorks = forAll genPlan $ \plan ->
  let rule = MaxTrainingDaysPerWeek 4
      violations = validate [rule] plan
      Plan dayMap = plan
      trainingDays = length [() | TrainingDay _ <- Map.elems dayMap]
  in (trainingDays <= 4) == null violations

-- Property: Plan normalization ensures all 7 days present
prop_planNormalizationComplete :: Property
prop_planNormalizationComplete = forAll genPlan $ \(Plan dayMap) ->
  Map.size dayMap == 7

main :: IO ()
main = do
  putStrLn "==================================="
  putStrLn "FitRule QuickCheck Property Tests"
  putStrLn "==================================="
  
  putStrLn "\n1. Plan normalization (always 7 days):"
  quickCheck prop_planNormalizationComplete
  
  putStrLn "\n2. MaxSetsPerDay detects violations:"
  quickCheck prop_maxSetsDetectsViolations
  
  putStrLn "\n3. MinRestDays validation:"
  quickCheck prop_minRestDaysWorks
  
  putStrLn "\n4. NoSameMuscleConsecutive detection:"
  quickCheck prop_noConsecutiveMuscleDetects
  
  putStrLn "\n5. MaxTrainingDaysPerWeek validation:"
  quickCheck prop_maxTrainingDaysWorks
  
  putStrLn "\n==================================="
  putStrLn "All tests completed!"
  putStrLn "==================================="