# Script to build SAFE Report Tables/Figures
# Set report_year to the year for which the safe report is being generated.
# Scripts will automatically pull data files for the previous year.
# (Make sure to save data files for 2024 SAFE report in './data/2023' folder)
source("./functions/WrangleSAFEData.R")

report_year = 2023

# First, data files are loaded into R and cleaned up
# file.edit("./functions/WrangleSAFEData.R")
WrangleSAFEData(report_year = report_year)


# Compile all Chapter 5 data
  # Generate xlsx workbooks and .html
# Compile all Chapter 6 data
  # Generate xlsx workbooks and .html