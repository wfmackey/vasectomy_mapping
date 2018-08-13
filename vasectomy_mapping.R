
# "Mapping" Australian higher education ####

library(tidyverse)
library(maptools)
library(scales) # better scales on maps
library(rgdal)     # R wrapper around GDAL/OGR
library(tictoc) # Timing code runs
library(readxl) # Read Excel files

# Set working directory
setwd("~/Google Drive/git/vasectomy_mapping")

# Read in SA3 to postcode mapping
postcodes <-  read_excel(path = "data/1270055006_CG_POSTCODE_2011_SA3_2011.xls",
                  sheet = "Table 3",
                  skip = 5) %>% 
              slice(2:n()) %>%  ## remove first empty row 
              select(postcode = POSTCODE, # changing names to be easier to use; and only keeping relevant vars
                     sa3_code = SA3_CODE_2011,
                     sa3      = SA3_NAME_2011,
                     percent  = PERCENTAGE) %>% 
              mutate(percent  = as.numeric(percent)) %>%  # convert to numeric for ease of use
              group_by(postcode) %>% 
              mutate(best_sa3 = if_else(percent == max(percent),
                                        TRUE, FALSE),
                     morethan1 = sum(best_sa3)) %>% # This identifies ONE perfect split, in Tasmania. Let's DROP South East Coast SA3 for ease; but we should come back and do this better later
              filter(sa3 != "South East Coast") %>% # Drop the one annoying SA3
              filter(best_sa3 == 1) %>% # Now, keep only the "best" SA3s for each postcode
              select(-morethan1, -best_sa3) %>%  # And remove any unnecessary variables
              ungroup() # This might not be necessary, as there is only one obversation per postcode

  
# Read in SA3 age data
sa3age <- read_csv("data/table.csv",
                       skip = 10) %>% 
          slice(2:n()) %>% 
          rename(sa3 = "AGE10P - Age in Ten Year Groups") %>% 
          rename(a2029 = "20-29 years") %>% 
          rename(a3039 = "30-39 years") %>% 
          rename(a4049 = "40-49 years") %>% 
          rename(a5059 = "50-59 years") %>% 
          rename(a6069 = "60-69 years")

# Now merge the two datasets
sa3 <- left_join(postcodes, sa3age, by = "sa3") 

edit <- sa3 %>% mutate(over50 = a5059 + a6069,
                       over50pc = over50 / Total,
                       vpop = (a3039/Total) )
          

