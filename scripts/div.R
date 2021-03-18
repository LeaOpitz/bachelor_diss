#Undergrad Dissertation
#Lea Opitz
#University of Edinburgh

## Diversity -----

#libraries
library(tidyverse)
library(janitor)
library(vegan)

#load data ----

#own data
hoch1 <- read.csv2("data/data_hoch.csv") 
ruc_b <- read.csv2("data/data_ruc_brach.csv") 
ruc_w <- read.csv2("data/data_ruc_weide.csv")
haus_b <- read.csv2("data/data_shaus_brach.csv") 
haus_m <- read.csv2("data/data_shaus_mahd.csv") 
      

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


all_data <- full_join(all_conny, all_mine, by = c("Type"="ï..Type", "sp")) 
  

#export full data set 
write.csv2(all_data, "C:/Users/Lea/Documents/R/git/bachelor_diss/data/full_list.csv", row.names = FALSE)


#get just species and treatments
sp_list <- all_data %>% slice(-c(4:8, 10:24, 162:166, 195:197)) %>% #remove unnecerssary rows
  janitor::row_to_names(1) #plot-ID as column name


# conny combined columns, I added red list column
all_data2 <- read.csv2("data/2021_Grasslands_Nationalpark_RE.csv")

sp_list2 <- all_data2 %>% slice(-c(4:8, 10:32)) %>% #remove unnecerssary rows
  janitor::row_to_names(1) #plot-ID as column name

sp_list3 <- sp_list2 %>% arrange()

metadata <- sp_list2 %>% slice(c(1:3)) %>% 
  select(-c("Type", "red_list"))
just_sp <- sp_list2 %>% slice(-c(1:3))
  

#longform 

sp_long <- just_sp %>%
  pivot_longer(-c("Type","red_list", "Plot-ID"), names_to = "plot", values_to = "cover") %>% 
  drop_na(cover) %>% 
  rename("sp"="Plot-ID") %>% 
  group_by(plot)


meta_long <- data.frame(t(metadata[-1]))
colnames(meta_long) <- metadata[, 1]
meta_long <- meta_long %>% rownames_to_column(var = "plot")



#join longform
data_long <- full_join(sp_long, meta_long, by = c("plot"))

#NMDS data
species <- just_sp %>% select(-c("Type", "red_list")) #delete unnecessary columns

species <- data.frame(t(species)) #switch rows and columns

species2 <- species %>% janitor::row_to_names(1) #sp as column names 
species2<- species2[ , colSums(is.na(species2)) < nrow(species2)] #delete all NA rows
species2[is.na(species2)] <- 0 #NA <- 0
species2[] <- lapply(species2, as.numeric)


#sort by veg type
species3 <- species2 %>% rownames_to_column(var = "plot") %>% 
  left_join(meta_long, by = c("plot")) %>% 
  arrange(Vegetation_type) %>% 
  select(-c(155:158)) %>% 
  column_to_rownames(var = "plot")
  
  species3[is.na(species3)] <- 0 #NA <- 0
  species3[] <- lapply(species3, as.numeric)
  

#sort by management
  species4 <- species2 %>% rownames_to_column(var = "plot") %>% 
    left_join(meta_long, by = c("plot")) %>% 
    arrange(Management) %>% 
    select(-c(155:158)) %>% 
    column_to_rownames(var = "plot")
  
  species4[is.na(species3)] <- 0 #NA <- 0
  species4[] <- lapply(species3, as.numeric)
  
#sort by site
  species5 <- species2 %>% rownames_to_column(var = "plot") %>% 
    left_join(meta_long, by = c("plot")) %>% 
    arrange(Area) %>% 
    select(-c(155:158)) %>% 
    column_to_rownames(var = "plot")
  
  species5[is.na(species3)] <- 0 #NA <- 0
  species5[] <- lapply(species3, as.numeric)
  

## data analysis ----

abundance <- data_long %>% count(plot, name = "species")%>% 
  full_join(meta_long, by = c("plot"))  



plantlm1 <- lm(species~Management*Vegetation_type, data = abundance)
summary(plantlm1)
anova(plantlm1)

plantlm2 <- lm(species~Management*Area, data = abundance)
summary(plantlm2)
anova(plantlm2)


#Pielou’s evennessJ=H′/log(S) is easily found as: 
    #J <- H/log(specnumber(BCI))

#betadiv:
    #betadiver(x, method = NA, order = FALSE, help = FALSE, ...)
    #method: Bray-Curtis ((A+B-2*J)/(A+B)	"minimum"	Bray-Curtis)

betadiv <- data_long %>% betadiver(method = "bray")

designdist

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

#NMDS
#needs sp as column names


#code field trip ----
# Run the NMDS

set.seed(2)  # Because the final result depends on the initial random placement of the points, we`ll set a seed to make the results reproducible
# Veg type NMDS
NMDS3 <- metaMDS(species3, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")
NMDS3


# Check the stress
stressplot(NMDS3)


# Look at the results!
plot(NMDS3)

# nicer plot
ordiplot(NMDS3, type = "n")
orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", cex = 1.1, air = 0.01)

## sites
# Define a group variable (first 12 samples belong to group 1, last 12 samples to group 2)
#group = c(rep("Blackford", 28), rep("Craigmillar", 28))
#meta_long %>% count(Vegetation_type)
#meta_long %>% count(Management)
meta_long %>% count(Area)

group = c(rep("Caltion", 18), rep("Carex", 21),
          rep("Mountain Meadow", 29), rep("Nardetum", 30))
colours = c(rep("yellow", 18), rep("orange", 21),
            rep("red", 29), rep("purple", 30))
# Create a vector of color values with same length as the vector of group values
#colors = c(rep("orange", 28), rep("purple", 28))

# Plot convex hulls with colors based on the group identity
ordiplot(NMDS3, type = "n")

for(i in unique(group)) {
  ordihull(NMDS3$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", col = c(rep("yellow", 18), rep("orange", 16),
                                           rep("red", 29), rep("purple", 30)), air = 0.01, cex = 1.25)


# group by management
NMDS4 <- metaMDS(species4, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")


group = c(rep("brach", 43), rep("mahd", 26), rep("weide", 29))
colours = c(rep("brown", 43), rep("light green", 26), rep("dark green", 29))

ordiplot(NMDS4, type = "n")
orditorp(NMDS4, display = "sites", col = c(rep("brown", 43), rep("light green", 26), 
                                     rep("dark green", 29)), air = 0.01, cex = 1.25)

# group by site
NMDS5 <- metaMDS(species5, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")


group = c(rep("Hochschachten", 30), rep("Ruckowitzschachten", 38), rep("weide", 30))
colours = c(rep("dark blue", 30), rep("light blue", 38), rep("green", 30))

ordiplot(NMDS5, type = "n")
orditorp(NMDS5, display = "sites", col = c(rep("dark blue", 30), rep("light blue", 38), rep("green", 30))
         , air = 0.01, cex = 1.25)
#code cc tut ----

# Here we use bray-curtis distance, which is recommended for abundance data
dist <- vegdist(species2,  method = "bray")

# In this part, we define a function NMDS.scree() that automatically 
# performs a NMDS for 1-10 dimensions and plots the nr of dimensions vs the stress
NMDS.scree <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS stress plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}

# Use the function that we just defined to choose the optimal nr of dimensions
NMDS.scree(dist)
#3-4 D would meet stress criteria

#metaMDS(dist, autotransform = F, k = 1)

# Because the final result depends on the initial 
# random placement of the points 
# we`ll set a seed to make the results reproducible
set.seed(2)

# Here, we perform the final analysis and check the result
NMDS1 <- metaMDS(dist, k = 2, trymax = 100, trace = F)

stressplot(NMDS1)

plot(NMDS1, type = "t")

#giving metaMDS the original community matrix as input and specifying the distance measure
NMDS2 <- metaMDS(species2, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")
plot(NMDS2)
plot(NMDS2, display = "sites", type = "n")
points(NMDS2, display = "sites", col = "red", cex = 1.25)
text(NMDS2, display ="species")

# Alternatively, you can use the functions ordiplot and orditorp
ordiplot(NMDS2, type = "n")
orditorp(NMDS2, display = "species", col = "red", air = 0.01)
orditorp(NMDS2, display = "sites", cex = 1.1, air = 0.01)
