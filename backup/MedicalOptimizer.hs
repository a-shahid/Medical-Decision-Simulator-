{-# LANGUAGE InstanceSigs #-}
module MedicalOptimizer where

import Library
import Data.List (maximumBy)
import Data.Ord (comparing)

------------------------------------------------------------------------------
-- 1. DOMAIN MODEL (Inspired by Author's MontyHall 'State')
------------------------------------------------------------------------------

----represents a patient with a specific name and a current medical status
data Patient = Patient { name :: String, status :: PatientState }

--defines the possible health states a patient can occupy within the simulation
data PatientState = Healthy | Recovering | Stable | Critical | Deceased 
                   deriving (Eq, Ord, Show)

--maps patient states to a numerical health value between 0.0 and 1.0
instance ToFloat PatientState where
    toFloat :: PatientState -> Float
    toFloat Healthy    = 1.0
    toFloat Recovering = 0.8
    toFloat Stable     = 0.6  
    toFloat Critical   = 0.3 
    toFloat Deceased   = 0.0

------------------------------------------------------------------------------
-- 2. STRATEGIC AI: UTILITY (Extending Library's Expected Class)
------------------------------------------------------------------------------
--converts a health state into a 100-point utility score for the AI to optimize
instance Expected PatientState where
    expected :: PatientState -> Float
    expected s = toFloat s * 100.0

------------------------------------------------------------------------------
-- 3. INTERVENTIONS (Inspired by Transitions in MontyHall/Dice)
------------------------------------------------------------------------------

-- models a surgical procedure where success and failure odds scale based on patient health.
surgery :: Trans PatientState
surgery Deceased = return Deceased
surgery s = enum [success, rehab, failure] [Healthy, Recovering, Deceased]
  where
    h       = toFloat s
    success = h * 0.75          -- for example Healthy (1.0) -> 80% | Critical (0.5) -> 40%
    failure = (1.0 - h) * 0.4  -- for example Healthy (1.0) -> 0%  | Critical (0.5) -> 20%
    rehab   = 1.0 - success - failure


-- models a drug treatment that attempts to move the patient up one health state
medication :: Trans PatientState
medication Deceased = return Deceased
medication Healthy  = return Healthy -- No need for meds if healthy
medication s = enum [0.6, 0.3, 0.1] [better, s, worse]
  where
    -- 'better' and 'worse' logic makes it work for ANY state
    better = if s == Critical then Stable else Healthy
    worse  = if s == Stable then Critical else Deceased


--models a post-treatment recovery phase that boosts Recovering
--it specifically helps patients who are "below" Healthy but "above" Critical.
physicalTherapy :: Trans PatientState
physicalTherapy s
    | s == Healthy    = return Healthy
    | s == Deceased   = return Deceased
    | s >= Recovering = choose 0.7 Healthy s  -- 70% chance to reach 'Healthy'
    | otherwise       = return s              -- Too sick for PT to help

--models a passive strategy where no action is taken and the patient's state remains unchanged
observation :: Trans PatientState
observation s = return s

------------------------------------------------------------------------------
-- 4. THE OPTIMIZER (The Strategic Extension)
------------------------------------------------------------------------------

--Selects the intervention that yields the highest mathematical expected utility for a given state.
optimize :: [Trans PatientState] -> PatientState -> Trans PatientState
optimize actions currentState = 
    maximumBy (comparing (\action -> expected (action currentState))) actions

------------------------------------------------------------------------------
-- 5. COMPLEX PATHWAYS (Using Authors' 'sequ' logic from MontyHall)
------------------------------------------------------------------------------

--combines a primary treatment and subsequent physical therapy into a single multi-step outcome
--this uses the 'sequ' function we extracted from the authors' Probability.hs
fullClinicalPath :: Trans PatientState -> PatientState -> Dist PatientState
fullClinicalPath treatment = sequ [treatment, physicalTherapy]

------------------------------------------------------------------------------
-- 6. RISK ANALYSIS 
------------------------------------------------------------------------------

--calculates the total probability of a patient ending in a Critical or Deceased state
-- Based on the predicate logic in Dice.hs
riskOfFailure :: Dist PatientState -> Probability
riskOfFailure d = (oneOf [Critical, Deceased]) ?? d


--analyzes a patient's case and prints a detailed report of the AI's strategic recommendation.
recommendationReport :: Patient -> IO ()
recommendationReport (Patient pName pStatus) = do
    let s = pStatus
    let actions = [surgery, medication, observation]
    let bestAction = optimize actions s
    let longTermDist   = fullClinicalPath bestAction s
    let immediateScore = expected (bestAction s)
    let longTermScore  = expected longTermDist
    let risk = riskOfFailure longTermDist
    let actionName 
                   | s == Deceased                            = "NONE (Patient Deceased)"
                   | longTermScore == expected (observation s) = "OBSERVATION (Stay Course)"
                   | immediateScore > 80                      = "SURGERY"
                   | otherwise                                = "MEDICATION"
    
    putStrLn $ "=== STRATEGIC REPORT FOR: " ++ pName ++ " ==="    
    putStrLn $ "Current Patient Status: " ++ show s
    putStrLn "----------------------------------------------"
    putStrLn $ "Recommended Action: " ++ actionName    
    putStrLn $ "Immediate Utility:  " ++ show immediateScore ++ "/100"
    putStrLn $ "Long-term Utility:   " ++ show longTermScore ++ "/100 (Includes Rehab)"
    putStrLn $ "Safety Check (Long-term Risk): " ++ show risk
    putStrLn "----------------------------------------------"
    putStrLn "Predicted Final Outcome (after Rehab):"
    print longTermDist