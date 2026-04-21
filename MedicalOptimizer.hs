{-# LANGUAGE InstanceSigs #-}
module MedicalOptimizer where

import Library
import Data.List (maximumBy)
import Data.Ord (comparing)

------------------------------------------------------------------------------
-- 1. DOMAIN MODEL (Inspired by Author's MontyHall 'State')
------------------------------------------------------------------------------

-- represents a patient with a specific name and a current medical status
data Patient = Patient { name :: String, status :: PatientState }

-- defines the possible health states a patient can occupy within the simulation
data PatientState = Healthy | Recovering | Stable | Critical | Deceased
                   deriving (Eq, Ord, Show)

-- maps patient states to a numerical health value between 0.0 and 1.0
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

-- converts a health state into a 100-point utility score for the AI to optimize
instance Expected PatientState where
    expected :: PatientState -> Float
    expected s = toFloat s * 100.0

------------------------------------------------------------------------------
-- 3. INTERVENTIONS (Inspired by Transitions in MontyHall/Dice)
------------------------------------------------------------------------------

-- models a surgical procedure where success and failure odds scale based on patient health
surgery :: Trans PatientState
surgery Deceased = return Deceased
surgery s = enum [success, rehab, failure] [Healthy, Recovering, Deceased]
  where
    h       = toFloat s
    success = h * 0.75          -- e.g. Healthy (1.0) -> 75% | Critical (0.3) -> 22.5%
    failure = (1.0 - h) * 0.4  -- e.g. Healthy (1.0) ->  0% | Critical (0.3) -> 28%
    rehab   = 1.0 - success - failure

-- FIX #1 (medication logic): moves a patient up or down exactly ONE health state,
-- preventing the jarring Recovering -> Deceased skip in the original.
-- The state ordering Healthy > Recovering > Stable > Critical > Deceased is used
-- directly so 'better' and 'worse' are always the immediate neighbours.
medication :: Trans PatientState
medication Deceased   = return Deceased
medication Healthy    = return Healthy  -- already at peak; no benefit
medication Recovering = enum [0.6, 0.3, 0.1] [Healthy,    Recovering, Stable]
medication Stable     = enum [0.6, 0.3, 0.1] [Recovering, Stable,     Critical]
medication Critical   = enum [0.6, 0.3, 0.1] [Stable,     Critical,   Deceased]

-- models a post-treatment recovery phase that boosts Recovering patients
-- it specifically helps patients who are "below" Healthy but "above" Critical
physicalTherapy :: Trans PatientState
physicalTherapy s
    | s == Healthy  = return Healthy
    | s == Deceased = return Deceased
    | s >= Recovering = choose 0.7 Healthy s  -- 70% chance to reach Healthy
    | otherwise       = return s              -- too sick for PT to help

-- models a passive strategy where no action is taken and state remains unchanged
observation :: Trans PatientState
observation s = return s

------------------------------------------------------------------------------
-- 4. NAMED ACTIONS  (Fix #2: wrap Trans in a record so the optimizer can
--    return the action's name alongside the function, eliminating the
--    fragile hardcoded threshold used for labelling in the original)
------------------------------------------------------------------------------

data Action = Action
    { actionName :: String
    , applyAction :: Trans PatientState
    }

surgeryAction :: Action
surgeryAction = Action "SURGERY" surgery

medicationAction :: Action
medicationAction = Action "MEDICATION" medication

observationAction :: Action
observationAction = Action "OBSERVATION (Stay Course)" observation

------------------------------------------------------------------------------
-- 5. COMPLEX PATHWAYS  (Item 1: explicit monadic bind)
--
-- The paper's central contribution is that Dist is a Monad, and (>>=) is
-- the correct tool for *dependent* multi-step events — exactly what a
-- treatment followed by rehab represents.
--
-- The bind chain below reads:
--   start in state s
--     >>= apply the primary treatment  (Dist PatientState)
--     >>= apply physicalTherapy        (Dist PatientState)
--
-- Each (>>=) multiplies the probabilities across the two steps, so the
-- final Dist contains every reachable state with its exact joint probability.
-- This is the monadic composition the paper demonstrates in MontyHall with
--   firstChoice >>= switch
------------------------------------------------------------------------------

-- Chains a primary treatment into physicalTherapy via (>>=), producing the
-- joint probability distribution over all final states.
fullClinicalPath :: Trans PatientState -> PatientState -> Dist PatientState
fullClinicalPath treatment s = treatment s >>= physicalTherapy

------------------------------------------------------------------------------
-- 6. THE OPTIMIZER
--
-- Scores every candidate Action by computing the long-term Dist with the
-- explicit monadic bind inside fullClinicalPath, then picks the maximum
-- expected utility.  Using the full distribution (not just the immediate
-- one-step score) means the optimizer accounts for how physicalTherapy
-- amplifies — or can't rescue — each treatment's outcome.
------------------------------------------------------------------------------

-- Selects the Action whose full clinical path yields the highest expected
-- utility, using the monadic Dist to evaluate every treatment option.
optimize :: [Action] -> PatientState -> Action
optimize actions s =
    maximumBy (comparing (\a -> expected (fullClinicalPath (applyAction a) s))) actions

------------------------------------------------------------------------------
-- 7. RISK ANALYSIS
------------------------------------------------------------------------------

-- calculates the total probability of a patient ending in Critical or Deceased
-- based on the predicate query logic from the paper's Dice.hs
riskOfFailure :: Dist PatientState -> Probability
riskOfFailure d = (oneOf [Critical, Deceased]) ?? d

------------------------------------------------------------------------------
-- 8. REPORTING
------------------------------------------------------------------------------

-- analyzes a patient's case and prints a detailed strategic report
recommendationReport :: Patient -> IO ()
recommendationReport (Patient pName pStatus) = do
    let s       = pStatus
    let allActions = [surgeryAction, medicationAction, observationAction]

    -- Deceased patients skip optimisation entirely
    let best         = if s == Deceased
                         then observationAction { actionName = "NONE (Patient Deceased)" }
                         else optimize allActions s

    let longTermDist  = fullClinicalPath (applyAction best) s
    let immediateScore = expected (applyAction best $ s)
    let longTermScore  = expected longTermDist
    let risk           = riskOfFailure longTermDist

    putStrLn $ "=== STRATEGIC REPORT FOR: " ++ pName ++ " ==="
    putStrLn $ "Current Patient Status: " ++ show s
    putStrLn "----------------------------------------------"
    putStrLn $ "Recommended Action:    " ++ actionName best
    putStrLn $ "Immediate Utility:     " ++ show immediateScore ++ "/100"
    putStrLn $ "Long-term Utility:     " ++ show longTermScore  ++ "/100 (Includes Rehab)"
    putStrLn $ "Safety Check (Long-term Risk): " ++ show risk
    putStrLn "----------------------------------------------"
    putStrLn "Predicted Final Outcome (after Rehab):"
    print longTermDist
