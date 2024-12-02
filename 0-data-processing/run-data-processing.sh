#!/bin/bash
echo "Running data processing scripts..."

python3 ../utilities/runFileSaveLogs.py -i "data-processing" 0-data-processing/1-percent-surface-water.R
python3 ../utilities/runFileSaveLogs.py -i "data-processing" 0-data-processing/2-process-data.R
