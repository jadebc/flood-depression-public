#!/bin/bash
echo "Running analysis scripts..."

python3 ../utilities/runFileSaveLogs.py -i "analysis" 1-analysis/1-depression-flood-regression.R
python3 ../utilities/runFileSaveLogs.py -i "analysis" 1-analysis/2-depression-surface-water-dist-regression.R
python3 ../utilities/runFileSaveLogs.py -i "analysis" 1-analysis/3-depression-surface-water-prop-regression.R
python3 ../utilities/runFileSaveLogs.py -i "analysis" 1-analysis/4-depression-water-level-regression.R
python3 ../utilities/runFileSaveLogs.py -i "analysis" 1-analysis/5-e-value-calculations.R
