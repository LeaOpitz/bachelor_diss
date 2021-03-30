#Undergrad Dissertation
#Lea Opitz
#University of Edinburgh
#time series analysis

#### load libraries and data ----

library(tidyverse)
library(vegan)


data <- read.csv2("data/ruc_timeseries.csv") 

###data wrangling ----

colnames(data) <- data[10, ] 

data2 <- data %>% subset(var!="Höh") %>% 
  mutate_all(na_if,"") %>% 
  mutate_all(na_if,"-") %>%
  select(-c(var)) %>% 
  slice(-c(1:3, 6:8, 11:23, 25, 168:170))

#need to combine Aufbahmejahr: & Aufnahme-Nr.: to get distinct names

#colnames(data2) <- data2[2, ] 

sp <- data2 %>% slice(-c(1:20)) %>% 
  select(-c(type)) %>% 
  column_to_rownames(var = "ID")

meta <- data2 %>% slice(c(1:3, 5))%>% 
  select(-c(type)) %>% 
  column_to_rownames(var = "ID")

sp.long <- sp %>%
  pivot_longer(c(1:314),names_to = "plot", values_to = "cover") %>% 
  drop_na(cover) #%>% 
  #rename(c("sp" = "Plot-ID")) %>% 
  #group_by(plot)


meta.long <- data.frame(t(meta[-1]))
colnames(meta.long) <- meta[, 1]
meta.long <- meta.long %>% rownames_to_column(var = "plot")

meta_long2 <- meta_long %>% column_to_rownames(var = "plot")

timeseries.long <- full_join(sp.long, meta.long, by = c("plot"))

series.abundance <- timeseries.long %>% count(plot, name = "species") %>% 
  full_join(meta.long, by = c("plot"))
#1st row meta missing!! figure out later


#### isla's betadiv code ----


# loop to calculate biodiversity metrics ----
beta_Bray <- data.frame(matrix(ncol = 7, nrow = length(unique(data$SiteSubsite)))) 
names(beta_Bray) <- c("SiteSubsite", "duration", "richness", "richness_change", "Bbal", "Bgra", "Bbray") 

# beta_Jaccard <- data.frame(matrix(ncol = 7, nrow = length(unique(data$SiteSubsite)))) 
# names(beta_Jaccard) <- c("SiteSubsite", "duration", "richness", "richness_change", "Jbeta", "Jtu", "Jne")

i = 1

# for loop with betapart ----
for (i in 1:length(unique(data$SiteSubsite))) {
  SiteSubSiteName <- as.character(unique(data$SiteSubsite)[i])
  sub_bio_abundance <- filter(data, SiteSubsite == SiteSubSiteName)
  YearMin <- min(sub_bio_abundance$YEAR)
  YearMax <- max(sub_bio_abundance$YEAR)
  duration <- YearMax - YearMin
  # filters the dataframe for just the first and last observations per plot
  sub_bio_abundance_min <- filter(sub_bio_abundance, YEAR == YearMin)
  sub_bio_abundance_max <- filter(sub_bio_abundance, YEAR == YearMax)
  sub_bio_abundance <- rbind(sub_bio_abundance_min, sub_bio_abundance_max)
  richness <- length(unique(sub_bio_abundance$GENUS_SPECIES))
  # averages any species that have multiple records per plot and time point 
  sub_bio_abundance <- sub_bio_abundance %>% group_by(SiteSubsite, GENUS_SPECIES, YEAR) %>% summarise(Abundance = mean(Abundance)) %>% ungroup()
  # reshape to wide form
  sub_bio_abundance_wider <- pivot_wider(sub_bio_abundance, names_from = GENUS_SPECIES, 
                                         values_from = Abundance, 
                                         values_fill = list(Abundance = 0))
  # removes columns for beta.pair() function
  sub_bio_abundance_matrix <- dplyr::select(sub_bio_abundance_wider, -SiteSubsite, -YEAR) 
  # creates presence-absence matrix
  sub_bio_presence_matrix <- with(sub_bio_abundance_matrix, ifelse(sub_bio_abundance_matrix > 0,1,0))
  # calculates Jaccard overall, turnover and nestedness
  # J_components <- beta.pair(sub_bio_presence_matrix, index.family='jaccard')
  J_components <- beta.pair.abund(sub_bio_presence_matrix, index.family='bray')
  # saves biodiversity metrics
  richness_change <- rowSums(sub_bio_presence_matrix)[2] - rowSums(sub_bio_presence_matrix)[1]
  # Jbeta <- J_components$beta.jac
  # Jtu <- J_components$beta.jtu
  # Jne <- J_components$beta.jne
  Bbal <- J_components$beta.bray.bal
  Bgra <- J_components$beta.bray.gra
  Bbray <- J_components$beta.bray
  # beta_Jaccard[i,] <- c(SiteSubSiteName, duration, richness, richness_change, Jbeta, Jtu, Jne)
  beta_Bray[i,] <- c(SiteSubSiteName, duration, richness, richness_change, Bbal, Bgra, Bbray)
  i = i+1
}

beta_Bray
