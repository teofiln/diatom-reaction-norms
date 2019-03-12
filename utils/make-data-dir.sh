#!/bin/bash

GR='\033[0;32m'
NC='\033[0m'

echo -e "${GR}Created a new 'data' directory in ../ ${NC}"
mkdir ../data
echo -e "${GR}Copied utility scripts to newly-created data directory${NC}"
cp calculate-slopes.R prepare-data.R prepare-all-the-data.R ../data
echo  -e "${GR}Created new subdirectories in ../data for measurements, metadata and transfer-files ${NC}"
cd ../data
mkdir measurements metadata transfer-files

echo
echo -e "${GR}Put your meta data file in the newly-created 'data/metadata' folder. The app will read it from there. Make sure it is called simply metadata.csv${NC}"
echo
echo -e "${GR}Put your raw data files in the newly-created 'data/measurements. The app will load them from there.' folder${NC}"
