# Option Pricing in the Hypergeometric Volatility Model

This repository contains all files related to my Master’s thesis titled  
**"Option Pricing in the Hypergeometric Volatility Model"**  
<!-- submitted at WU Vienna, Institute for Statistics and Mathematics, in 2025. -->

## Overview

In this thesis, I explore the option pricing implications of the **Hypergeometric Volatility Model**, a recent class of rough volatility models. Using Monte Carlo simulations, I evaluate its ability to capture stylized features of implied volatility surfaces such as skew, smile convexity, and power-law decay of at-the-money volatility skew.

## Contents

- `latex/`  
  LaTeX source files used to typeset the thesis.

- `code/`  
  R scripts for running simulations, analyzing results, and generating plots.

    - `main.R`, `config.R`, `analysis.R`, `main.sh`  
      Core scripts for model configuration, simulation, post-processing, and batch execution.

    - `code/models/`  
      Implementation of the asset price process, hypergeometric volatility model, and option pricing logic.

    - `code/utils/`  
      Supporting functions for data handling, plotting, and filtering of simulation outputs.

## Highlights

- Implementation of a simulation-based pricing framework for rough volatility models.
- Model evaluation based on empirical criteria: smile shape, skew behavior, and power-law fits.
- Modular R code designed for extensibility and reproducibility.

## How to Use

To run the full simulation and analysis pipeline:

1. Install required R packages.
2. Adjust parameters in `code/config.R` as needed.
3. Run the simulation:
    ```r
    source("code/config.R")
    source("code/main.R")
    ```
4. Perform analysis:
    ```r
    source("code/analysis.R")
    ```
5. Alternatively, execute the workflow via the shell script:
    ```bash
    bash code/main.sh
    ```

## Author

Simon Mitterhofer  
Supervised by: Univ.Prof. Dr. Rüdiger Frey  
Institute for Statistics and Mathematics, WU Vienna

---
