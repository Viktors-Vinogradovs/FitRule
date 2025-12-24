{-# LANGUAGE OverloadedStrings #-}

module Generator
  ( generateValidPlan
  , generatePlanText
  ) where

import Domain
import Validate
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import System.Random
import Data.List (delete)

-- Generate a valid plan that satisfies given rules
-- Pure function with random seed
generateValidPlan :: [Rule] -> Int -> Plan
generateValidPlan rules seed =
  let gen = mkStdGen seed
      maxAttempts = 100
  in tryGenerate rules gen maxAttempts

-- Try generating until we get a valid plan
tryGenerate :: [Rule] -> StdGen -> Int -> Plan
tryGenerate rules gen attempts
  | attempts <= 0 = normalizePlan []  -- Give up, return empty plan
  | otherwise =
      let (plan, gen') = generatePlanWithGen rules gen
          violations = validate rules plan
      in if null violations
         then plan
         else tryGenerate rules gen' (attempts - 1)

generatePlanWithGen :: [Rule] -> StdGen -> (Plan, StdGen)
generatePlanWithGen rules gen0 =
  let maxSetsPerDay = findMaxSets rules
      minRestDays = findMinRest rules
      requiredMuscles = findRequiredMuscles rules
      maxTrainingDays = 7 - minRestDays
      
      -- Determine training days (spread them out)
      (trainingDays, gen1) = selectSpreadOutDays maxTrainingDays gen0
      
      -- Assign required muscles to training days first
      (muscleAssignments, gen2) = assignMuscles requiredMuscles trainingDays gen1
      
      -- Generate exercises for each training day
      (dayPlans, gen3) = generateDayPlansForMuscles muscleAssignments maxSetsPerDay gen2
      
      -- Fill rest days
      allDayPlans = [(d, maybe RestDay id (lookup d dayPlans)) | d <- allDays]
  in (normalizePlan allDayPlans, gen3)

findMaxSets :: [Rule] -> Int
findMaxSets rules = case [n | MaxSetsPerDay n <- rules] of
  (x:_) -> x
  [] -> 12

findMinRest :: [Rule] -> Int
findMinRest rules = case [n | MinRestDays n <- rules] of
  (x:_) -> x
  [] -> 2

findRequiredMuscles :: [Rule] -> [Muscle]
findRequiredMuscles rules = case [ms | RequireMuscleSplit ms <- rules] of
  (x:_) -> x
  [] -> []

-- Select days that are spread out (avoid consecutive)
selectSpreadOutDays :: Int -> StdGen -> ([Day], StdGen)
selectSpreadOutDays n gen =
  let allDaysList = [Mon, Tue, Wed, Thu, Fri, Sat, Sun]
      (selected, gen') = selectWithGaps n allDaysList [] gen
  in (selected, gen')
  where
    selectWithGaps 0 _ acc g = (reverse acc, g)
    selectWithGaps count available acc g
      | null available = (reverse acc, g)
      | otherwise =
          let (idx, g1) = randomR (0, length available - 1) g
              day = available !! idx
              -- Remove selected day and adjacent days to avoid consecutive
              newAvailable = filter (\d -> not (isAdjacent d day)) (delete day available)
          in selectWithGaps (count - 1) newAvailable (day : acc) g1
    
    isAdjacent d1 d2 =
      let idx1 = fromEnum d1
          idx2 = fromEnum d2
      in abs (idx1 - idx2) == 1

-- Assign each required muscle to a different training day
assignMuscles :: [Muscle] -> [Day] -> StdGen -> ([(Day, Muscle)], StdGen)
assignMuscles muscles days gen
  | null muscles = ([], gen)
  | null days = ([], gen)
  | length muscles <= length days =
      -- Assign each muscle to a different day
      let pairs = zip days muscles
      in (pairs, gen)
  | otherwise =
      -- More muscles than days, need to combine some
      let cycles = (length muscles + length days - 1) `div` length days
          (shuffled, gen') = shuffle muscles gen
          assignments = zip (concat (replicate cycles days)) shuffled
      in (take (length muscles) assignments, gen')

shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle [] g = ([], g)
shuffle xs g =
  let (idx, g1) = randomR (0, length xs - 1) g
      selected = xs !! idx
      rest = take idx xs ++ drop (idx + 1) xs
      (shuffledRest, g2) = shuffle rest g1
  in (selected : shuffledRest, g2)

generateDayPlansForMuscles :: [(Day, Muscle)] -> Int -> StdGen -> ([(Day, DayPlan)], StdGen)
generateDayPlansForMuscles assignments maxSets gen =
  let go [] g acc = (reverse acc, g)
      go ((day, muscle):rest) g acc =
        let (exercises, g1) = generateExercisesForMuscle muscle maxSets g
            dayPlan = (day, TrainingDay exercises)
        in go rest g1 (dayPlan : acc)
  in go assignments gen []

generateExercisesForMuscle :: Muscle -> Int -> StdGen -> ([Exercise], StdGen)
generateExercisesForMuscle muscle maxSets gen =
  let exerciseNames = case muscle of
        Chest -> ["BenchPress", "Dips", "Flyes", "PushUps"]
        Back -> ["PullUps", "Rows", "Deadlift", "LatPull"]
        Legs -> ["Squat", "Lunge", "LegPress", "Curl"]
        Shoulders -> ["Press", "Raise", "Shrug", "FrontRaise"]
        Arms -> ["BicepCurl", "TricepDip", "HammerCurl", "Extension"]
        Core -> ["Plank", "Crunch", "LegRaise", "RussianTwist"]
      
      (numExercises, g1) = randomR (2, min 3 (maxSets `div` 3)) gen
      (selected, g2) = selectRandomN numExercises exerciseNames g1
      setsPerEx = maxSets `div` max 1 numExercises
      
      go [] g acc = (reverse acc, g)
      go (name:ns) g acc =
        let (sets, g1) = randomR (3, min 5 setsPerEx) g
            ex = Exercise (T.pack name) muscle sets
        in go ns g1 (ex : acc)
  in go selected g2 []

selectRandomN :: Int -> [a] -> StdGen -> ([a], StdGen)
selectRandomN 0 _ g = ([], g)
selectRandomN n xs g
  | n >= length xs = (xs, g)
  | otherwise =
      let (idx, g1) = randomR (0, length xs - 1) g
          selected = xs !! idx
          rest = take idx xs ++ drop (idx + 1) xs
          (more, g2) = selectRandomN (n - 1) rest g1
      in (selected : more, g2)

-- Convert plan to text format
generatePlanText :: Plan -> Text
generatePlanText (Plan dayMap) =
  T.unlines [formatDay d (dayMap Map.! d) | d <- allDays]
  where
    formatDay d RestDay = T.pack (show d) <> ": Rest"
    formatDay d (TrainingDay exs) =
      T.pack (show d) <> ": " <> T.intercalate "; " (map formatEx exs)
    
    formatEx (Exercise name muscle sets) =
      name <> " " <> T.pack (show muscle) <> " " <> T.pack (show sets)