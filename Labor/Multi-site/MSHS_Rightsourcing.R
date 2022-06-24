# GENERAL REMINDERS -------------------------------------------------------

# 1. The outline can be customized based on project requirements.
# 2. Sections may be added, removed, reordered, renamed, etc. as needed.
# 3. Reference checklist for commenting and style guidelines
# 4. Create Sections using the menu Code > Insert Section
#    or the shortcut Ctrl+Shift+R
# 5. Create subsections by starting a Section with two # signs (e.g. ##).  
#    - See example within this section.

# Libraries ---------------------------------------------------------------
# Include all the packages that will be used throughout the code.
# This is where packages can be installed if the user does not have them
# currently installed.

# Ensuring the appropriate package versions are used for the project based on
# renv usage
renv::restore()

# Common Packages
library(readxl)
# library(rmarkdown)
# library(shiny)

# tidyverse includes: dplyr, ggplot2, lubridate, purrr, readr, readxl,
# reprex, stringr, tidyr...and more
# See this link for full list: https://tidyverse.tidyverse.org/
# library(tidyverse)


# Assigning Directory ------------------------------------------------

# rightsourcing project path
project_path <- paste0("//researchsan02b/shr2/deans/Presidents/SixSigma/",
                       "MSHS Productivity/Productivity/Labor - Data/",
                       "Rightsourcing Labor/")
# universal mapping path
mapping_path <- paste0("//researchsan02b/shr2/deans/Presidents/SixSigma/",
                       "MSHS Productivity/Productivity/universal Data/",
                       "Mapping/")

# Data Import / Data References--------------------------------------------

# jobcode list to map job description to job code
jobcode_list <- read.csv(paste0(project_path, 
                                "Rightsource Job Code.csv"))
# pay period mapping file to determine max date of next upload
pay_period_mapping <- read_xlsx(paste0(mapping_path, 
                                       "MSHS_Pay_Cycle.xlsx"))
# code conversion mapping file to convert legacy to oracle cc
code_conversion <- read_xlsx(paste0(mapping_path, 
                                    "MSHS_Code_Conversion_Mapping.xlsx"))

# Data Pre-processing -----------------------------------------------------
# Cleaning raw data and ensuring that all values are accounted for such as
# blanks and NA. As well as excluding data that may not be used or needed. This
# section can be split into multiple ones based on the data pre-processing
# needed.
# One of the first steps could be to perform initial checks to make sure data is
# in the correct format.  This might also be done as soon as the data is
# imported.


# Data Formatting ---------------------------------------------------------
# How the data will look during the output of the script.
# For example, if you have a data table that needs the numbers to show up as
# green or red depending on whether they meet a certain threshold.


# Quality Checks ----------------------------------------------------------
# Checks that are performed on the output to confirm data consistency and
# expected outputs.


# Visualization -----------------------------------------------------------
# How the data will be plotted or how the data table will look including axis
# titles, scales, and color schemes of graphs or data tables.
# (This section may be combined with the Data Formatting section.)

# For Matt & Greg - perhaps there's a quick plot that can be used as a quality
# check.

# File Saving -------------------------------------------------------------
# Writing files or data for storage


# Script End --------------------------------------------------------------