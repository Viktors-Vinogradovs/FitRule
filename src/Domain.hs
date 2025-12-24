module Domain
  ( Day(..)
  , Muscle(..)
  , Exercise(..)
  , DayPlan(..)
  , Plan(..)
  , Rule(..)
  , Violation(..)
  , normalizePlan
  , allDays
  ) where

import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Show, Enum, Bounded)

data Muscle = Chest | Back | Legs | Shoulders | Arms | Core
  deriving (Eq, Ord, Show, Read)

data Exercise = Exercise
  { exName   :: Text
  , exMuscle :: Muscle
  , exSets   :: Int
  } deriving (Eq, Show)

data DayPlan
  = RestDay
  | TrainingDay [Exercise]
  deriving (Eq, Show)

-- Plan now uses Map for consistent access
newtype Plan = Plan (Map Day DayPlan)
  deriving (Eq, Show)

data Rule
  = MaxSetsPerDay Int
  | MinRestDays Int
  | NoSameMuscleConsecutive
  | AllowedMuscles [Muscle]
  | MaxSetsPerMusclePerWeek Muscle Int
  | MaxTrainingDaysPerWeek Int
  | RequireMuscleSplit [Muscle]
  deriving (Eq, Show)

data Violation = Violation
  { vRule   :: Rule
  , vDay    :: Maybe Day
  , vReason :: Text
  } deriving (Eq, Show)

-- Helper: all days of the week
allDays :: [Day]
allDays = [Mon, Tue, Wed, Thu, Fri, Sat, Sun]

-- Pure function: normalize plan to always have all 7 days
-- Missing days default to RestDay
normalizePlan :: [(Day, DayPlan)] -> Plan
normalizePlan pairs =
  let dayMap = Map.fromList pairs
      fullWeek = Map.fromList [(d, RestDay) | d <- allDays]
  in Plan (Map.union dayMap fullWeek)  -- dayMap takes precedence