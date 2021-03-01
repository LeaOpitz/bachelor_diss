#Undergrad Dissertation
#Lea Opitz
#University of Edinburgh

## Diversity -----

#libraries
library(tidyverse)

#load data ----

#own data
hoch1 <- read.csv2("data/data_hoch.csv") #163-166 #spp ab 28, alle anderen ab 30
ruc_b <- read.csv2("data/data_ruc_brach.csv") #165-167
ruc_w <- read.csv2("data/data_ruc_weide.csv") #165-167
haus_b <- read.csv2("data/data_shaus_brach.csv") #delete row165-168 
haus_m <- read.csv2("data/data_shaus_mahd.csv") # delete 165-167, 180/181
      #alle 1-3, 6/7

#connys data


## data wrangling ----

#rename Tipp für die nächste Evaluierung: Poa mit Herbarbelgen differenzieren, ebenso Alchemilla -> sp

#longform gather()

#combine datasets full_join()
all_haus <- full_join(haus_b, haus_m, by = "sp")
all_ruc <- full_join(ruc_b, ruc_w, by = "sp")
all_weide <- full_join(all_ruc, hoch1, by = "sp")
all_data <- full_join(all_weide, all_haus, by = "sp") 
  
all_data <- all_data %>% mutate_all(na_if,"") %>%  #replace empty cells with NA
  drop_na(sp) %>% #remove empty columns
  filter(!(sp %in% c("Anzahl Arten", "Andere:", "Weitere:", "Anmerkung:", "Weitere")))

data_long <- gather(all_data, "Plot-ID", "sp")
