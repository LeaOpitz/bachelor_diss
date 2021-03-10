#Undergrad Dissertation
#Lea Opitz
#University of Edinburgh

## Diversity -----

#libraries
library(tidyverse)
library(janitor)


#load data ----

#own data
hoch1 <- read.csv2("data/data_hoch.csv") #spp ab 28, alle anderen ab 30
ruc_b <- read.csv2("data/data_ruc_brach.csv") 
ruc_w <- read.csv2("data/data_ruc_weide.csv")
haus_b <- read.csv2("data/data_shaus_brach.csv") 
haus_m <- read.csv2("data/data_shaus_mahd.csv") 
      #alle 1-3, 

#connys data
ruc_c <- read.csv2("data/veg_ruc_2020.csv") %>%
      select(Type, sp, X, X.32, X.88:X.108)#agr 11a-e, cal jehl 1-5 & 7-12, cabi 1a&5a
hoch_c <- read.csv2("data/veg_hoc_2020.csv") %>% 
  select (Type, sp, X, X.28:X.38, X.75:X.84)#agr 4a-d, nar 12, cal5ab?

## data wrangling ----

#combine datasets full_join()

all_mine <- full_join(haus_b, haus_m, by = c("ï..Type", "sp")) %>% 
  full_join(hoch1, by = c("ï..Type", "sp")) %>% 
  full_join(ruc_b, by = c("ï..Type", "sp")) %>% 
  full_join(ruc_w, by = c("ï..Type", "sp")) %>% 
  mutate_all(na_if,"") %>%  #replace empty cells with NA
  drop_na(sp) %>% #remove empty columns
  filter(!(sp %in% c("Anzahl Arten", "Andere:", "Weitere:", "Anmerkung:", "Weitere", 
                     "Skala: Londo, alle Werte bereits in Deckungen, keine Transformation nÃ¶tig",
                     "[] = diese Art ist auÃŸerhalb des Plotes gewachsen")))

#select suitable plots

#ruc_c2 <- ruc_c %>%  select(Type, sp, X.88:X.108)
#hoch_c2 <- hoch_c %>% select (Type, sp, X.28:X.38, X.75:X.84)


all_conny <- full_join(ruc_c, hoch_c, by = c("Type", "sp")) %>% 
  mutate_all(na_if,"") %>%  #replace empty cells with NA
  drop_na(sp) %>% #remove empty columns
  filter(!(sp %in% c("Anzahl Arten", "Andere:", "Weitere:", "Anmerkung:", "Weitere", 
                     "Skala: Londo, alle Werte bereits in Deckungen, keine Transformation nötig",
                     "[] = diese Art ist außerhalb des Plotes gewachsen",
                     "Deckung Gefäßpflanzen (ab 2020)",
                     "2020 wurde lediglich das Mahdtransekt evaluiert (dieses hatte 2019 gefehlt)",
                     "Tipp für die nächste Evaluierung: Poa mit Herbarbelgen differenzieren, ebenso Alchemilla"))) %>% 
  select(-c(X.100, X.102, X.104, X.106, X.108, X.29, X.31, X.33, X.35, X.36, X.38,
            X.76, X.78, X.80, X.82, X.84)) #remove height columns



#str(all_conny)

all_data <- full_join(all_conny, all_mine, by = c("Type"="ï..Type", "sp")) 
  

#export full data set 
write.csv2(all_data, "C:/Users/Lea/Documents/R/git/bachelor_diss/data/full_list.csv", row.names = FALSE)

#get just species and treatments
sp_list <- all_data %>% slice(-c(4:8, 10:24, 162:166, 195:197)) %>% #remove unnecerssary rows
  janitor::row_to_names(1) #plot-ID as column name
  
#%>%  filter[-c(4:8, 10:24, 162:166, 195:197,)]
  #ChickWeight[-c(578),] 
  #filter(!(sp %in% c("DB_Jehl_Aufnahmeflaeche", "DB_Jehl_Aufnahme",
                                           # "Datum: ", "Bearbeiter:", "Area", 
                                            #"Offen", "Wasser"))) 
                                            #ends_with("(%)"), ends_with("(cm)"))))
  
select(sp_list,-ends_with("(%)"))


all_data3 <- all_data %>% rename("ID" = "Plot-ID")

all_data2 <- all_data %>%  select(!(ID %in% c("NA")))

all_data2 <- all_data %>% distinct()

all_data3 <- all_data %>%  rowid_to_column("Plot-ID")
all_data4 <- all_data %>%  janitor::row_to_names("Plot-ID")
all_data5 <- all_data %>% rownames_to_column( var = "Plot-ID") %>% as_tibble()
  #deal with when long form?
sp_list2 <- sp_list %>%
  janitor::row_to_names(1)
#sorbus <- all_data %>% filter(sp == "Sorbus aucuparia")

#longform gather() work in progress
data_long <- gather(all_data, "Plot-ID", "sp")

data_long <- pivot_longer()
df %>% pivot_longer(c(x, y, z), names_to = "key", values_to = "value")

head(hoch1)
str(hoch1)
