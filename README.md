# Supporting code and data for our manuscript 'Measuring global monetary damages from particulate matter and carbon dioxide to track sustainable growth'

## Overview
This code and data will replicate the results reported in our manuscript.

## Requirements

R, RStudio

## Steps

1. Download or clone the repository on your machine.

2. Open the Tracking_Environmental_Damages.Rproj file in Rstudio. This sets your working directory to the project folder.

3. Open and run the file new_pmmortality_2022.R. This will tabulate the premature mortality estimates across all end points.

4. In the same environment, now run econ_damages_2022.r. This will tabulate damages from PM2.5 (based on the premature mortality estimates from Step 3) and CO2 emissions.

5. Run sensitivity_exploration.r in the same environment for further results. All the main results should now be generated as dataframes in your environment. Several of them can also be found in the Results folder in the repository.

6. Run plots.r to generate the plots in the manuscript.


For any questions or comments please contact the corresponding author on the manuscript, Nicholas Z. Muller - nzm@andrew.cmu.edu 
