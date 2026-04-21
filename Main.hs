module Main where

import MedicalOptimizer
import Library

-- Runs the Strategic AI across the entire spectrum of patient care,
-- then demonstrates the authors' randomized simulation (pick / R monad).
main :: IO ()
main = do

    let patients = [ Patient "John Doe"        Healthy
                   , Patient "Jane Smith"       Recovering
                   , Patient "Bob Jones"        Stable
                   , Patient "Alice Vance"      Critical
                   , Patient "Unknown Patient"  Deceased ]

    -- Deterministic full-distribution reports for every patient
    mapM_ (\p -> recommendationReport p >> putStrLn "\n") patients

    -- FIX #4: demonstrate the authors' randomised simulation path (Section 4
    -- of the paper) using the 'pick' function from Library.hs.
    -- We sample 10 random outcomes from the surgery distribution for a
    -- Critical patient to show that the same model can run in either exact
    -- or approximate mode without changing the transition definition.
    putStrLn "=== RANDOMISED SIMULATION DEMO (Section 4 of paper) ==="
    putStrLn "Sampling 10 random surgery outcomes for a Critical patient:"
    let surgeryDist = surgery Critical
    samples <- mapM (\_ -> pick surgeryDist) [1..10 :: Int]
    mapM_ (\(i, s) -> putStrLn $ "  Trial " ++ show i ++ ": " ++ show s)
          (zip [1..] samples)
    putStrLn "\n(Run again to see different samples — same model, approximate mode)"
