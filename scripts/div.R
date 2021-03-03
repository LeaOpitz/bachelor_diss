#Undergrad Dissertation
#Lea Opitz
#University of Edinburgh

## Diversity -----

#libraries
library(tidyverse)

#load data ----

#own data
hoch1 <- read.csv2("data/data_hoch.csv") #spp ab 28, alle anderen ab 30
ruc_b <- read.csv2("data/data_ruc_brach.csv") 
ruc_w <- read.csv2("data/data_ruc_weide.csv")
haus_b <- read.csv2("data/data_shaus_brach.csv") 
haus_m <- read.csv2("data/data_shaus_mahd.csv") 
      #alle 1-3, 

#connys data
ruc_c <- read.csv2("data/veg_ruc_2020.csv") #agr 11a-e, cal jehl 1-5 & 7-12
hoch_c <- read.csv2("data/veg_hoc_2020.csv") #agr 4a-d, nar 12, cal5ab?

## data wrangling ----

#combine datasets full_join()
all_haus <- full_join(haus_b, haus_m, by = "sp")
all_ruc <- full_join(ruc_b, ruc_w, by = "sp")
all_weide <- full_join(all_ruc, hoch1, by = "sp")
all_mine <- full_join(all_weide, all_haus, by = "sp") 

all_mine <- all_mine %>% mutate_all(na_if,"") %>%  #replace empty cells with NA
  drop_na(sp) %>% #remove empty columns
  filter(!(sp %in% c("Anzahl Arten", "Andere:", "Weitere:", "Anmerkung:", "Weitere", 
                     "Skala: Londo, alle Werte bereits in Deckungen, keine Transformation nÃ¶tig",
                     "[] = diese Art ist auÃŸerhalb des Plotes gewachsen")))

#select suitable plots
#ruc_c2 <- ruc_c %>%  select(Plot-ID %in% c("11a", "11b", "11c", "11d", "11e") )
ruc_c2 <- ruc_c %>%  select(X, sp, X.89:X.109)
hoch_c2 <- hoch_c %>% select (X, sp, X.29:X.39, X.76:X.85)


all_conny <- full_join(ruc_c2, hoch_c2, by = "sp") 
all_conny <- all_conny %>% mutate_all(na_if,"") %>%  #replace empty cells with NA
  drop_na(sp) %>% #remove empty columns
  filter(!(sp %in% c("Anzahl Arten", "Andere:", "Weitere:", "Anmerkung:", "Weitere", 
                     "Skala: Londo, alle Werte bereits in Deckungen, keine Transformation nötig",
                     "[] = diese Art ist außerhalb des Plotes gewachsen")))


str(all_mine)

all_data <- full_join(all_mine, all_conny, by = "sp") 





#longform gather()
data_long <- gather(all_data, "Plot-ID", "sp")
