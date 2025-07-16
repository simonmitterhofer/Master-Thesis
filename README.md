# Master-Thesis-Code  
**Option Pricing in the Hypergeometric Volatility Model – Simulation and Analysis**

This repository contains the full codebase for simulations and statistical analysis conducted as part of my Master's thesis. The project focuses on evaluating the performance of the **Hypergeometric Volatility Model** in option pricing, using simulated asset paths and volatility surfaces.

---

## Project Structure

```

main.R               # Main script to run the full simulation pipeline
main.sh              # Shell script for execution in a batch environment
config.R             # Centralized configuration of model parameters
analysis.R           # Post-simulation statistical analysis and plotting

models/
asset.R            # Asset price dynamics and simulation
volatility.R       # Hypergeometric volatility model implementation

utils/
data\_management.R  # Data I/O utilities (e.g., loading/saving results)
plotting.R         # Custom plotting functions for smile/skew/etc.

````

---

## How to Run

### From R
Run the full pipeline from R:

```r
source("config.R")
source("main.R")
````

### From Bash (e.g., server or cluster)

```bash
bash main.sh
```

The script will execute all necessary R components and save output to predefined folders.

---

## Dependencies

Make sure the following R packages are installed:

* `parallel`

You can install it using:

```r
install.packages(c("parallel"))
```

Or use a project manager like `{renv}` for reproducibility.

---

## Outputs

* **Simulation Results**: raw and processed asset paths and volatility samples
* **Plots**: smile curves, skewness, power-law fits, and other diagnostics
* **Tables**: summary statistics for model evaluation

These are saved into folders like `results/`, `plots/`, and `output/` (create them if necessary).

---

## License & Usage

This repository is private and intended for academic purposes only.
Please do not redistribute or use parts of this work without explicit permission.

---

## Author

Simon Mitterhofer

Vienna University of Economics and Business, 2025

Master's Thesis in Quantitative Finance
