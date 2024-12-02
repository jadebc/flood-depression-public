#!/bin/bash
echo "Running figure generation scripts..."

python3 ../utilities/runFileSaveLogs.py -i "figures" 2-figure-scripts/1-flood-water-map.R
python3 ../utilities/runFileSaveLogs.py -i "figures" 2-figure-scripts/2-plot-water-level-flood.R
