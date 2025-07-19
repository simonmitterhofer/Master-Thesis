#!/bin/bash
#SBATCH --job-name=rough_vol_sim
#SBATCH --output=simulation_%j.out
#SBATCH --error=simulation_%j.err
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=64
#SBATCH --mem=32G
#SBATCH --time=120:00:00
#SBATCH --partition=compute  # Adjust based on your cluster

echo "=== Rough Volatility Simulation ==="
echo "Job starting at: $(date)"
echo "Job ID: $SLURM_JOB_ID"
echo "Node: $SLURMD_NODENAME"
echo "CPUs allocated: $SLURM_CPUS_PER_TASK"
echo "Memory allocated: ${SLURM_MEM_PER_NODE}MB"
echo "Working directory: $(pwd)"
echo ""

# Check if required files exist
echo "Checking required files..."
required_files=("config.R" "main.R" "models/volatility.R" "models/asset.R" "models/option_pricing.R" "models/implied_volatility.R" "utils/data_management.R")

for file in "${required_files[@]}"; do
    if [ ! -f "$file" ]; then
        echo "ERROR: Required file $file not found!"
        exit 1
    fi
done
echo "All required files found."
echo ""

# Create output directory
mkdir -p SimulationData
echo "Created SimulationData directory"

# Load R module (adjust module name/version for your cluster)
echo "Loading R module..."
module purge

# Try different common R module names - adjust for your specific cluster
if module avail r 2>&1 | grep -q "r-4"; then
    # Try R 4.x versions
    module load r-4.4.1-gcc-12.2.0-x6ecfp6 2>/dev/null || \
    module load R/4.4.1 2>/dev/null || \
    module load r/4.4.1 2>/dev/null || \
    module load R/4.3.0 2>/dev/null || \
    module load r/4.3.0 2>/dev/null || \
    module load R 2>/dev/null || \
    echo "Warning: Could not load R module automatically. Please adjust manually in main.sh"
else
    module load R 2>/dev/null || \
    echo "Warning: Could not load R module. Please adjust manually in main.sh"
fi

# Check if R is available
if command -v R &> /dev/null; then
    echo "R version: $(R --version | head -n1)"
else
    echo "ERROR: R not found in PATH!"
    exit 1
fi
echo ""

# Check available memory and CPU info
echo "System resources:"
echo "Available memory: $(free -h | awk '/^Mem:/ {print $7}')"
echo "CPU info: $(nproc) cores available"
echo ""

# Start simulation
echo "Starting simulation..."
echo "Command: Rscript main.R"
echo ""

# Run the simulation with error checking
if Rscript main.R; then
    echo ""
    echo "=== Simulation completed successfully ==="
else
    echo ""
    echo "=== Simulation failed with exit code $? ==="
    exit 1
fi

echo "Job finished at: $(date)"
echo ""

# Show results summary - with better error handling
if [ -d "SimulationData" ]; then
    echo "SimulationData directory exists"
    
    # Check if directory has any files
    if [ -n "$(ls -A SimulationData/ 2>/dev/null)" ]; then
        echo "Files in SimulationData/:"
        ls -la SimulationData/
        echo ""
        echo "File sizes:"
        du -h SimulationData/*
    else
        echo "WARNING: SimulationData directory is empty!"
    fi
else
    echo "ERROR: SimulationData directory not found!"
fi

echo ""
echo "=== Job Complete ==="