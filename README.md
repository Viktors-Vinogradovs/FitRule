# FitRule

A functional workout plan validator and generator written in Haskell.

## Overview

FitRule validates workout plans against configurable training rules. It ensures plans follow proper principles regarding rest days, training volume, and muscle group balance.

Built with functional programming - pure functions, immutable data, algebraic types.

## Features

- **Custom DSL** for rules and workout plans
- **7 validation rules** (rest days, volume limits, muscle balance)
- **Pure functional validation** - no side effects
- **Plan generator** with configurable random seeds
- **Property-based testing** with QuickCheck (500+ test cases)
- **Type-safe** domain modeling with ADTs

## Quick Start

### Prerequisites

- GHC 9.6.7 or later
- Cabal 3.12 or later

### Installation

```bash
git clone https://github.com/Viktors-Vinogradovs/FitRule.git
cd fitrule
cabal build
```

### Usage

**Validate a plan:**
```bash
cabal run fitrule -- examples/rules.txt examples/plan.txt
```

**Generate a valid plan:**
```bash
cabal run fitrule -- generate examples/rules.txt output.txt
```

**Generate with specific seed:**
```bash
cabal run fitrule -- generate examples/rules.txt output.txt 12345
```

**Run tests:**
```bash
cabal test
```

## DSL Format

### Rules File (rules.txt)

```
MAX_SETS_PER_DAY 12
MIN_REST_DAYS 2
NO_SAME_MUSCLE_CONSECUTIVE
ALLOWED_MUSCLES Chest Back Legs Shoulders Arms Core
MAX_SETS_PER_MUSCLE_PER_WEEK Chest 20
MAX_TRAINING_DAYS_PER_WEEK 5
REQUIRE_MUSCLE_SPLIT Chest Back Legs
```

### Plan File (plan.txt)

```
Mon: BenchPress Chest 4; Dips Chest 3
Tue: PullUps Back 4; Rows Back 3
Wed: Rest
Thu: Squat Legs 4; Curl Legs 3
Fri: Rest
Sat: Press Shoulders 4
Sun: Rest
```

## Validation Rules

| Rule | Description |
|------|-------------|
| `MAX_SETS_PER_DAY` | Maximum sets per training day |
| `MIN_REST_DAYS` | Minimum rest days per week |
| `NO_SAME_MUSCLE_CONSECUTIVE` | Prevents same muscle on consecutive days |
| `ALLOWED_MUSCLES` | Whitelist of allowed muscle groups |
| `MAX_SETS_PER_MUSCLE_PER_WEEK` | Weekly volume limit per muscle |
| `MAX_TRAINING_DAYS_PER_WEEK` | Maximum training days per week |
| `REQUIRE_MUSCLE_SPLIT` | Ensures listed muscles are trained |

## Example Output

**Valid plan:**
```
[VALID] PLAN PASSED
All rules satisfied.
```

**Invalid plan:**
```
[INVALID] PLAN FAILED
Violations found:

  * NoSameMuscleConsecutive [Tue]: Muscles repeated from previous day: [Chest]
  * MaxSetsPerDay 12 [Wed]: Total sets = 15, max allowed = 12
```

## Project Structure

```
fitrule/
├── app/           # Entry point
├── src/           # Core logic (Domain, Parser, Validate, Render, Generator)
├── test/          # QuickCheck property tests
├── examples/      # Example rules and plans
└── fitrule.cabal  # Build configuration
```

## Testing

Run the test suite:

```bash
cabal test --test-show-details=direct
```

Tests include:
- Plan normalization (always 7 days)
- MaxSetsPerDay violation detection
- MinRestDays validation
- Consecutive muscle detection
- Training days limit validation

All tests use QuickCheck with targeted generators.

## Use Cases

### For Fitness Coaches
- Validate client plans before publishing
- Catch programming errors automatically
- Ensure proper recovery and volume
- Save time on manual checking

### For Developers
- Generate valid workout plans programmatically
- Integrate into workout apps
- Automated quality control
- User-configurable rules

## License

Educational project for Functional Programming course.


## Acknowledgments

Built as part of Functional Programming coursework, demonstrating practical applications of FP principles.
