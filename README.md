R-code and data for a blog titled: "Consumer Use of Personal Checks" by Oz Shy

Instructions:

1. From here: https://github.com/ozshy/k_economy, download 10 data files: 
"dcpc-YEAR-indlevel-public.RDS" and "dcpc-YEAR-tranlevel-public.RDS". 
YEAR should be replaced with: 2021, 2022, 2023, 2024, and 2025.

2. Download the R-code check25_2026_MM_DD.R from this folder.

3. Start R with the R code, and reset the working directory 5 times! To do that, search for #2025_begins, then #2024_begins, down to #2021_begins. 
In all 5, you will see the old setwd(~xxx/yyy) which you must change to identify where the data files that you just downloaded are located. 
If you are using RStudio, you can find these by clicking on the list of contents at the left-bottom corner. Run the ENTIRE R-code first.

Note 1: The working directory needs to be modified 5 times because I place the data from each year in a separate folder/directory. 
Therefore, if you put all the data files in a single directory, you can modify the code to set the working directory only once (and delete the other setwd(~xxx) commands from the code).
