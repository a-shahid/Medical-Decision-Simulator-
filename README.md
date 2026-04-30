# Project Title: Strategic Medical Optimizer
**Based on Probabilistic Functional Programming (PFP)**

---

## Part 1: The Base (`Library.hs`)

### Source & Origin
The foundation of this project is built upon the **pfp-jun06** library developed by **Martin Erwig and Steve Kollmansberger**. The original source code was obtained from the official [Oregon State University PFP page](https://oregonstate.edu).

### Implementation Process
To create a foundation for the extension, core logic from the original library was taken and improved where needed:
*   **`probability.hs`**: The primary `Dist` (Distribution) monad and `Expected` value logic.
*   **`listutils.hs`**: Helper functions for list manipulation and distribution normalization.
*   **`show.hs`**: Formatting utilities for output.

### Main Refinements and Modernization (there are also others which are mentioned in the comments in the code)
*   **Consolidation**: Merged three separate legacy files into a single, cohesive `Library.hs` 
*   **Modern GHC Compatibility**: Implemented `Functor` and `Applicative` instances and updated `Monad` syntax to meet 2024+ Haskell standards.
*   **Dependency-Free Randomness**: Replaced `System.Random` with a custom **Linear Congruential Generator (LCG)** seeded by `getPOSIXTime`, allowing the project to run on any standard Haskell installation.

---

## Part 2: The Extension (`MedicalOptimizer.hs`)

This module transforms the library into a **Strategic AI** capable of automated decision-making.
*   **Patient Modeling**: Defines a `PatientState` (Healthy, Recovering, Stable, Critical, Deceased) and maps them to numerical utility scores (0-100).
*   **Interventions**: Implements medical actions (`surgery`, `medication`, `observation`) as probabilistic transitions (`Trans`). For example, surgery success rates scale based on the patient's current health.
*   **The Optimizer**: A function that evaluates the **Expected Value** of every possible treatment path and selects the one that mathematically maximizes patient outcomes.

---

## Part 3: Testing & Evaluation (`Main.hs`)

This is for executing the simulation and viewing the decision-making process.
*    Runs the optimizer across a spectrum of patient cases to verify logical consistency.
*    Generates a detailed breakdown for each case, showing the recommended action, immediate vs. long-term utility, and a risk analysis.

---

## Instructions for usage & testing

### 1. Automated Testing
The quickest way to test is to run the pre-configured tests in `Main.hs`. You can update it to see results for criteria you'd like.
*   **Action**: Run `main` in ghci.
*   **Result**: This will trigger a `recommendationReport` for five different patient types, ranging from "Healthy" to "Deceased," allowing you to see how the decision making strategy is adjusted based on the type of the case.

**Execution Instructions for 1:**
1. Open your terminal in the project folder.
2. Load the project: `ghci Main.hs`
3. Execute the test suite: `main`


### 2. Indirect Testing 
You can manually test specific scenarios by interacting with `MedicalOptimizer` module directly in the interpreter.

**Example Commands for 2:**
```haskell
--check the outcome distribution of surgery on a Stable patient
*ghci> surgery Stable

--manually calculate the expected utility of medication for a Critical patient
*ghci> expected (medication Critical)

--test the optimizer for a specific state manually
*ghci> optimize [surgery, medication, observation] Stable Critical
```

