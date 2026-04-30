-- Date: April 23rd, 2026

module Main where

import MedicalOptimizer
import Library

--runs the optimzer and decision making across the entire spectrum of patient care
main :: IO ()
main = do
    
    let patients = [ Patient "John Doe" Healthy
               , Patient "Jane Smith" Recovering
               , Patient "Bob Jones" Stable
               , Patient "Alice Vance" Critical
               , Patient "Unknown Patient" Deceased ]
    
    --this runs the report for every patient state in the list
    mapM_ (\p -> recommendationReport p >> putStrLn "\n") patients
