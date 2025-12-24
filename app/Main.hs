module Main where

import Parser (parseRulesFile, parsePlanFile)
import Validate (validate)
import Render (renderResult)
import Generator (generateValidPlan, generatePlanText)

import System.Environment (getArgs)
import System.IO (stdout, hSetEncoding, utf8)
import Data.Text (Text)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  -- Set UTF-8 encoding to handle all characters
  hSetEncoding stdout utf8
  
  args <- getArgs
  case args of
    [rulesPath, planPath] -> runValidator rulesPath planPath
    ["generate", rulesPath, outputPath] -> runGenerator rulesPath outputPath 42
    ["generate", rulesPath, outputPath, seedStr] -> 
      case reads seedStr of
        [(seed, "")] -> runGenerator rulesPath outputPath seed
        _ -> do
          putStrLn "Error: Seed must be an integer"
          printUsage
    _ -> printUsage

printUsage :: IO ()
printUsage = do
  putStrLn "FitRule: Workout Plan Validator & Generator"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  Validate:  cabal run fitrule -- <rules.txt> <plan.txt>"
  putStrLn "  Generate:  cabal run fitrule -- generate <rules.txt> <output.txt> [seed]"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  cabal run fitrule -- examples\\rules.txt examples\\plan.txt"
  putStrLn "  cabal run fitrule -- generate examples\\rules.txt generated.txt"
  putStrLn "  cabal run fitrule -- generate examples\\rules.txt generated.txt 123"
  putStrLn ""
  putStrLn "Note: seed is optional (default: 42). Use different seeds for different plans."

runValidator :: FilePath -> FilePath -> IO ()
runValidator rulesPath planPath = do
  putStrLn "Parsing rules..."
  eRules <- parseRulesFile rulesPath
  
  putStrLn "Parsing plan..."
  ePlan  <- parsePlanFile planPath

  case (eRules, ePlan) of
    (Left err, _) -> printErr "Parse Error (Rules):" err
    (_, Left err) -> printErr "Parse Error (Plan):" err
    (Right rules, Right plan) -> do
      putStrLn "Validating plan..."
      putStrLn ""
      let violations = validate rules plan
      TIO.putStrLn (renderResult violations)

printErr :: String -> Text -> IO ()
printErr title msg = do
  putStrLn "================================"
  putStrLn title
  putStrLn "================================"
  TIO.putStrLn msg
  putStrLn ""

runGenerator :: FilePath -> FilePath -> Int -> IO ()
runGenerator rulesPath outputPath seed = do
  putStrLn "Parsing rules..."
  eRules <- parseRulesFile rulesPath
  
  case eRules of
    Left err -> printErr "Parse Error (Rules):" err
    Right rules -> do
      putStrLn $ "Generating valid plan (seed: " ++ show seed ++ ")..."
      let plan = generateValidPlan rules seed
          planText = generatePlanText plan
      
      TIO.writeFile outputPath planText
      putStrLn $ "Generated plan saved to: " ++ outputPath
      
      putStrLn "\nValidating generated plan..."
      let violations = validate rules plan
      TIO.putStrLn (renderResult violations)