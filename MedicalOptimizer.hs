-- Date: April 23rd, 2026

module MedicalOptimizer where

import Library
import Data.List (maximumBy)
import Data.Ord (comparing)

------------------------------------------------------------------------------
--inspired by author's montyhall 'state'
------------------------------------------------------------------------------

---represents a patient with a specific name and a current medical status
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
--utility(extending library's expected Class)
------------------------------------------------------------------------------
--converts a health state into a 100-point utility score for the AI to optimize
instance Expected PatientState where
    expected :: PatientState -> Float
    expected s = toFloat s * 100.0

------------------------------------------------------------------------------
--interventions(inspired by transitions in montyHall/dice)
------------------------------------------------------------------------------

--models a surgical procedure where success and failure odds scale based on patient health.
surgery :: Trans PatientState
surgery Deceased = return Deceased
surgery s = enum [success, rehab, failure] [Healthy, Recovering, Deceased]
  where
    h       = toFloat s
    success = h * 0.8          --for example Healthy (1.0) -> 80% 
    failure = (1.0 - h) * 0.4  --for example Healthy (1.0) -> 0%  
    rehab   = 1.0 - success - failure


--models a drug treatment that attempts to move the patient up one health state
medication :: Trans PatientState
medication Deceased = return Deceased
medication Healthy  = return Healthy --no need for meds if healthy
medication s = enum [0.6, 0.3, 0.1] [better, s, worse]
  where
    --'better' and 'worse' logic makes it work for any state
    better = if s == Critical then Stable else Healthy
    worse  = if s == Stable then Critical else Deceased


--models a post-treatment recovery phase that boosts Recovering
--it specifically helps patients who are "below" Healthy but "above" Critical.
physicalTherapy :: Trans PatientState
physicalTherapy s
    | s == Healthy    = return Healthy
    | s == Deceased   = return Deceased
    | s == Recovering || s == Stable = choose 0.7 Healthy s 
    | otherwise       = return s              

--models a passive strategy where no action is taken and the patient's state remains unchanged
observation :: Trans PatientState
observation s = return s

------------------------------------------------------------------------------
--optimizer
------------------------------------------------------------------------------

--selects the intervention that yields the highest mathematical expected utility for a given state.
optimize :: [Trans PatientState] -> PatientState -> Trans PatientState
optimize actions currentState = 
    maximumBy (comparing (\action -> expected (action currentState))) actions

------------------------------------------------------------------------------
--complex pathways (using authors' 'sequ' logic from monty hall)
------------------------------------------------------------------------------

--combines a primary treatment and subsequent physical therapy into a single multi-step outcome
fullClinicalPath :: Trans PatientState -> PatientState -> Dist PatientState
fullClinicalPath treatment = sequ [treatment, physicalTherapy]

------------------------------------------------------------------------------
--risk analysis
------------------------------------------------------------------------------

--calculates the total probability of a patient ending in a Critical or Deceased state
-- Based on the predicate logic in Dice.hs
riskOfFailure :: Dist PatientState -> Probability
riskOfFailure d = (oneOf [Critical, Deceased]) ?? d


--analyzes a patient's case and prints a detailed report of the systems's strategic recommendation.
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
