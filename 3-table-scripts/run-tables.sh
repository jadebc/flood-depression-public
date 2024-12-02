#!/bin/bash
echo "Running table generation scripts..."

python3 ../utilities/runFileSaveLogs.py -i "tables" 3-table-scripts/1-tables-flood-depression.R
python3 ../utilities/runFileSaveLogs.py -i "tables" 3-table-scripts/2-appendix-tables-flood-depression.R
