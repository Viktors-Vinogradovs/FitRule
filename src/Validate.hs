module Validate
  ( validate
  ) where

import Domain
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

validate :: [Rule] -> Plan -> [Violation]
validate rules plan =
  concatMap (\r -> applyRule r plan) rules

applyRule :: Rule -> Plan -> [Violation]
applyRule r p =
  case r of
    MaxSetsPerDay n              -> maxSetsPerDay n p
    MinRestDays n                -> minRestDays n p
    NoSameMuscleConsecutive      -> noSameMuscleConsecutive p
    AllowedMuscles ms            -> allowedMuscles ms p
    MaxSetsPerMusclePerWeek m n  -> maxSetsPerMusclePerWeek m n p
    MaxTrainingDaysPerWeek n     -> maxTrainingDaysPerWeek n p
    RequireMuscleSplit ms        -> requireMuscleSplit ms p

-- Rule: Max sets per day
maxSetsPerDay :: Int -> Plan -> [Violation]
maxSetsPerDay limit (Plan dayMap) =
  [ Violation (MaxSetsPerDay limit) (Just d)
      (T.pack ("Total sets = " ++ show total ++ ", max allowed = " ++ show limit))
  | (d, TrainingDay exs) <- Map.toList dayMap
  , let total = sum (map exSets exs)
  , total > limit
  ]

-- Rule: Minimum rest days per week
minRestDays :: Int -> Plan -> [Violation]
minRestDays minR (Plan dayMap) =
  let restCount = length [() | RestDay <- Map.elems dayMap]
  in if restCount >= minR
     then []
     else [Violation (MinRestDays minR) Nothing
            (T.pack ("Rest days = " ++ show restCount ++ ", min required = " ++ show minR))]

-- Rule: No same muscle on consecutive days
-- Now works correctly with normalized full week
noSameMuscleConsecutive :: Plan -> [Violation]
noSameMuscleConsecutive (Plan dayMap) =
  let orderedDays = [(d, dayMap Map.! d) | d <- allDays]
  in concatMap checkPair (zip orderedDays (drop 1 orderedDays))
  where
    checkPair ((_, dp1), (d2, dp2)) =
      case (muscles dp1, muscles dp2) of
        (Just m1s, Just m2s) ->
          let overlap = filter (`elem` m2s) m1s
          in if null overlap
             then []
             else [Violation NoSameMuscleConsecutive (Just d2)
                    (T.pack ("Muscles repeated from previous day: " ++ show overlap))]
        _ -> []

    muscles RestDay = Nothing
    muscles (TrainingDay exs) = Just (uniq (map exMuscle exs))

uniq :: Eq a => [a] -> [a]
uniq = foldr (\x acc -> if x `elem` acc then acc else x:acc) []
    
-- Rule: Allowed muscles only
allowedMuscles :: [Muscle] -> Plan -> [Violation]
allowedMuscles allowed (Plan dayMap) =
  concat
    [ [Violation (AllowedMuscles allowed) (Just d)
        (T.pack ("Muscle not allowed: " ++ show m))]
    | (d, TrainingDay exs) <- Map.toList dayMap
    , m <- map exMuscle exs
    , m `notElem` allowed
    ]

-- NEW RULE: Max sets per muscle per week
maxSetsPerMusclePerWeek :: Muscle -> Int -> Plan -> [Violation]
maxSetsPerMusclePerWeek muscle maxSets (Plan dayMap) =
  let totalSets = sum
        [ exSets ex
        | TrainingDay exs <- Map.elems dayMap
        , ex <- exs
        , exMuscle ex == muscle
        ]
  in if totalSets > maxSets
     then [Violation (MaxSetsPerMusclePerWeek muscle maxSets) Nothing
            (T.pack ("Total " ++ show muscle ++ " sets = " ++ show totalSets ++
                     ", max allowed = " ++ show maxSets))]
     else []

-- NEW RULE: Max training days per week
maxTrainingDaysPerWeek :: Int -> Plan -> [Violation]
maxTrainingDaysPerWeek maxDays (Plan dayMap) =
  let trainingDays = length [() | TrainingDay _ <- Map.elems dayMap]
  in if trainingDays > maxDays
     then [Violation (MaxTrainingDaysPerWeek maxDays) Nothing
            (T.pack ("Training days = " ++ show trainingDays ++
                     ", max allowed = " ++ show maxDays))]
     else []

-- NEW RULE: Require muscle split (each muscle must appear at least once)
requireMuscleSplit :: [Muscle] -> Plan -> [Violation]
requireMuscleSplit required (Plan dayMap) =
  let trainedMuscles = uniq
        [ exMuscle ex
        | TrainingDay exs <- Map.elems dayMap
        , ex <- exs
        ]
      missing = filter (`notElem` trainedMuscles) required
  in if null missing
     then []
     else [Violation (RequireMuscleSplit required) Nothing
            (T.pack ("Missing muscles in weekly plan: " ++ show missing))]