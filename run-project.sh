#!/bin/bash
echo "Run repository..."

# Run data processing scripts
echo "Step 1: Running data processing..."
./0-data-processing/run-data-processing.sh

# Run analysis scripts
echo "Step 2: Running analysis..."
./1-analysis/run-analysis.sh

# Run figure generation scripts
echo "Step 3: Generating figures..."
./2-figure-scripts/run-figures.sh

# Run table generation scripts
echo "Step 4: Generating tables..."
./3-table-scripts/run-tables.sh

echo “Run complete”
