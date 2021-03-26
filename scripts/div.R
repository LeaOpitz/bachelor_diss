#Undergrad Dissertation
#Lea Opitz
#University of Edinburgh
#2020 data


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
  rename(c("sp" = "Plot-ID")) %>% 
  group_by(plot)


meta_long <- data.frame(t(metadata[-1]))
colnames(meta_long) <- metadata[, 1]
meta_long <- meta_long %>% rownames_to_column(var = "plot")

meta_long2 <- meta_long %>% column_to_rownames(var = "plot")

#join longform
data_long <- full_join(sp_long, meta_long, by = c("plot"))

#NMDS data
species <- just_sp %>% select(-c("Type", "red_list")) #delete unnecessary columns

species <- data.frame(t(species)) #switch rows and columns

species2 <- species %>% janitor::row_to_names(1) #sp as column names 
#species2<- species2[ , colSums(is.na(species2)) < nrow(species2)] #delete all NA rows
species2[is.na(species2)] <- 0 #NA <- 0
species2[] <- lapply(species2, as.numeric)


#sort by veg type
species3 <- species2 %>% rownames_to_column(var = "plot") %>% 
  left_join(meta_long, by = c("plot")) %>% 
  arrange(Vegetation_type) %>% 
  #select(-c(155:158)) %>% 
  select(-c(155:157)) %>% #delete meta data again
  #select(-c("Sphagnum sec.", "Climatium dendroides", "Plagiomnium undulatum")) %>%
  column_to_rownames(var = "plot")
  
  species3[is.na(species3)] <- 0 #NA <- 0
  species3[] <- lapply(species3, as.numeric)
  

#sort by management
  species4 <- species2 %>% rownames_to_column(var = "plot") %>% 
    left_join(meta_long, by = c("plot")) %>% 
    arrange(Management) %>% 
    select(-c(155:157)) %>% 
    column_to_rownames(var = "plot")
  
  species4[is.na(species4)] <- 0 #NA <- 0
  species4[] <- lapply(species4, as.numeric)
  
#sort by site
  species5 <- species2 %>% rownames_to_column(var = "plot") %>% 
    left_join(meta_long, by = c("plot")) %>% 
    arrange(Area) %>% 
    select(-c(155:157)) %>% 
    column_to_rownames(var = "plot")
  
  species5[is.na(species5)] <- 0 #NA <- 0
  species5[] <- lapply(species5, as.numeric)
  

## data analysis ----

  #get #rare species  
rare_sp <- data_long %>% group_by(plot) %>% 
    count(red_list, name = "rare") %>% 
    subset(red_list!="N")
  
  
abundance <- data_long %>% count(plot, name = "species")%>% 
  full_join(rare_sp, by = c("plot"))  %>% 
  full_join(meta_long, by = c("plot")) %>% 
    mutate(prop_rare = rare/ species) %>% 
    select(-c(red_list))

abundance[is.na(abundance)] <- 0 

abundance2 <- abundance %>% select(-c(rare, prop_rare))   
 
str(abundance) 
##### Mean of the species column by group 
mean1 <-  aggregate(x= abundance$species,
            by= list(abundance$Management),
            FUN=mean)

standard_error <- function(x) sd(x) / sqrt(length(x)) # Create own function

mean2 <- abundance %>%
  group_by(Management) %>%
  mutate(mean_by_group = mean(species)) %>% 
  mutate(standard_error(species))

mean3 <- abundance2 %>%
  group_by(Vegetation_type) %>%
  mutate(mean_by_group = mean(species)) %>% 
  mutate(standard_error(species))

mean4 <- abundance2 %>%
  group_by(Area) %>%
  mutate(mean_by_group = mean(species)) %>% 
  mutate(standard_error(species))

(box_man <- ggplot(data= abundance, aes(x= as.factor(Management), y = species, fill = Management))+
  geom_boxplot(size = 0.3) +
  theme_classic()+ 
  #scale_fill_manual(  #scale_fill_manual controls the colours of the 'fill' you specified in the 'ggplot' function.
   # values = c("#FEB96C", "#CC92C2"))+
  scale_x_discrete(name = "\ntypes of managemnt") +
  scale_y_continuous(name = "# species\n")+
  theme(text=element_text(size = 18), axis.line = element_line(size = 0.5), axis.ticks = element_line(size = 0.5)))

res <-  boxplot(species~Management, data = abundance) #medians   8.5   16   13
res


(box_veg <- ggplot(data= abundance, aes(x= as.factor(Vegetation_type), y = species, fill = Vegetation_type))+
  geom_boxplot(size = 0.3) +
  theme_classic()+ 
  #scale_fill_manual(  #scale_fill_manual controls the colours of the 'fill' you specified in the 'ggplot' function.
  # values = c("#FEB96C", "#CC92C2"))+
  scale_x_discrete(name = "\nvegetation types") +
  scale_y_continuous(name = "# species\n")+
  theme(text=element_text(size = 18), axis.line = element_line(size = 0.5), axis.ticks = element_line(size = 0.5))
)

(box_area <- ggplot(data= abundance, aes(x= as.factor(Area), y = species, fill = Area))+
  geom_boxplot(size = 0.3) +
  theme_classic()+ 
  #scale_fill_manual(  #scale_fill_manual controls the colours of the 'fill' you specified in the 'ggplot' function.
  # values = c("#FEB96C", "#CC92C2"))+
  scale_x_discrete(name = "\nvegetation types") +
  scale_y_continuous(name = "# species\n")+
  theme(text=element_text(size = 18), axis.line = element_line(size = 0.5), axis.ticks = element_line(size = 0.5)))

res2 <-  boxplot(species~Area, data = abundance)
res2

## Evenness
# Shannon index
H <- diversity(abundance$species) #4.4077
#Pielou’s evenness J=H′/log(S)
J <- H/log(specnumber(abundance$species)) #0.9613

#by subgroup? 
H <- abundance %>% group_by(Management) %>%
      diversity(abundance$species)

abundance$species <- as.numeric(as.character(abundance$species)) 
#betadiv:
    #betadiver(x, method = NA, order = FALSE, help = FALSE, ...)
    #method: Bray-Curtis ((A+B-2*J)/(A+B)	"minimum"	Bray-Curtis)

betadiv <- data_long %>% betadiver(method = "bray")

designdist


#### ordination ----
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
meta_long %>% count(Vegetation_type)
meta_long %>% count(Management)
meta_long %>% count(Area)

group2 = c(rep("Calthion", 18), rep("Carex", 21),
          rep("Mountain Meadow", 29), rep("Nardetum", 30))
colours = c(rep("yellow", 18), rep("orange", 21),
            rep("red", 29), rep("purple", 30))
# Create a vector of color values with same length as the vector of group values
#colors = c(rep("orange", 28), rep("purple", 28))

# Plot convex hulls with colors based on the group identity
ordiplot(NMDS3, type = "n")

for(i in unique(group2)) {
  ordihull(NMDS3$point[grep(i, group2),], draw="polygon",
           groups = group2[group2 == i],col = colours[grep(i,group2)],label=F) } 

#orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", col = c(rep("yellow", 18), rep("orange", 21),
                                           rep("red", 29), rep("purple", 30)), air = 0.01, cex = 1.25)


# group by management
NMDS4 <- metaMDS(species4, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")


#group = c(rep("brach", 48), rep("mahd", 21), rep("weide", 29))
#colours = c(rep("brown", 48), rep("light green", 21), rep("dark green", 29))

ordiplot(NMDS4, type = "n")
orditorp(NMDS4, display = "sites", col = c(rep("brown", 48), rep("light green", 21), 
                                     rep("dark green", 29)), air = 0.01, cex = 1.25)

# group by site
NMDS5 <- metaMDS(species5, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")


#group = c(rep("Hochschachten", 30), rep("Ruckowitzschachten", 38), rep("weide", 30))
#colours = c(rep("dark blue", 30), rep("light blue", 38), rep("green", 30))

ordiplot(NMDS5, type = "n")
orditorp(NMDS5, display = "sites", col = c(rep("dark blue", 30), rep("light blue", 38), rep("green", 30))
         , air = 0.01, cex = 1.25)
#code cc tut ----

# Here we use bray-curtis distance, which is recommended for abundance data
dist <- vegdist(species4,  method = "bray")

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
NMDS2 <- metaMDS(species4, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")
plot(NMDS2)
plot(NMDS2, display = "sites", type = "n")
points(NMDS2, display = "sites", col = "red", cex = 1.25)
text(NMDS2, display ="sites")

# Alternatively, you can use the functions ordiplot and orditorp
ordiplot(NMDS2, type = "n")
orditorp(NMDS2, display = "species", col = "red", air = 0.01)
orditorp(NMDS2, display = "sites", cex = 1.1, air = 0.01)

# Load the second dataset (envir variables)


# The function envfit will add the environmental variables as vectors to the ordination plot
ef <- envfit(NMDS2, meta_long2, permu = 999)
ef

# The two last columns are of interest: the squared correlation coefficient and the associated p-value
# Plot the vectors of the significant correlations and interpret the plot
plot(NMDS2, type = "t", display = "sites")
plot(ef, p.max = 0.05)


# Define a group variable (first 12 samples belong to group 1, last 12 samples to group 2)
group1 = c(rep("brach", 48), rep("mahd", 21), rep("weide", 29))

# Create a vector of color values with same length as the vector of group values
colors = c(rep("brown", 48), rep("light green", 21), rep("dark green", 29))

# Plot convex hulls with colors based on the group identity
ordiplot(NMDS2, type = "n")
for(i in unique(group1)) {
  ordihull(NMDS2$point[grep(i, group1),], draw="polygon",
           groups = group1[group1 == i],col = colors[grep(i,group1)],label=F) } 

#orditorp(NMDS2, display = "species", col = "red", air = 0.01)
orditorp(NMDS2, display = "sites", col = c(rep("brown", 48), 
                     rep("light green", 21), rep("dark green", 29)), air = 0.01, cex = 1.25)


# Define a group variable (first 12 samples belong to group 1, last 12 samples to group 2)
group3 = c(rep("Hochschachten", 30), rep("Ruckowitzschachten", 38), rep("weide", 30))

# Create a vector of color values with same length as the vector of group values
colors2 = c(rep("dark blue", 30), rep("light blue", 38), rep("green", 30))

ordiplot(NMDS5, type = "n")
for(i in unique(group2)) {
  ordihull(NMDS2$point[grep(i, group3),], draw="polygon",
           groups = group3[group3 == i],col = colors2[grep(i,group3)],label=F) } 

#orditorp(NMDS2, display = "species", col = "red", air = 0.01)
orditorp(NMDS5, display = "sites", col = c(rep("dark blue", 30), rep("light blue", 38), 
                                           rep("green", 30)), air = 0.01, cex = 1.25)

###PCA

pca1 <- prcomp(species4.2, center = TRUE,scale. = TRUE) #didn't work bc empty columns
species4.2 <- species4[,apply(species4, 2, var, na.rm=TRUE) != 0] #remove column with all 0?

pca1 <- prcomp(species4)
summary(pca1) #1+2 -> 78%, +3 -> 82% #after removing 0s waaay lower
str(pca1)

#library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)
library(gridExtra)
ggbiplot(pca1, labels=rownames(species4.2), groups = group1)

###adonis
adon.results<-adonis(species4 ~ group1, method="bray",perm=999)
print(adon.results)

## Bray-Curtis distances between samples
dis <- vegdist(species4)

## Calculate multivariate dispersions
mod <- betadisper(dis, group1)
mod

# diffr groups -> not v helpful
adon.veg <-adonis(species3 ~ group2, method="bray",perm=999)
print(adon.veg)

## Bray-Curtis distances between samples
dis2 <- vegdist(species3)

## Calculate multivariate dispersions
mod2 <- betadisper(dis2, group2)
mod

### check normality ----

Schacht <- subset(abundance, Area == "Schachtenhaus", 
                  select = c(plot, species, Vegetation_type, Management))
Ruck <- subset(abundance, Area == "Ruckowitzschachten", 
               select = c(plot, species, Vegetation_type, Management))
Hoch <- subset(abundance, Area == "Hochschachten",
               select = c(plot, species, Vegetation_type, Management))

#Normalverteilung: 
shapiro.test(Schacht$species) #p-value = 0.08003   #N-Verteilung ja nur bei v2$Arten net
shapiro.test(Ruck$species) #0.221 -> normal?
shapiro.test(Hoch$species) #p-value = 0.0006596 -> not normal?
#value is not less than .05, we can assume the sample data comes from a 
    #population that is normally distributed.

hist(Ruck$species, breaks = 15)

library(car)
leveneTest(abundance$species, abundance$Area, center=mean) 


### GLMs ----
plantlm1 <- lm(species~Management*Vegetation_type, data = abundance)
summary(plantlm1)
anova(plantlm1)

plantlm2 <- lm(species~Management*Area, data = abundance)
summary(plantlm2)
anova(plantlm2)

library(lme4)

hist(abundance$species) #-> poisson?
glmer(species ~ Management + (1|Vegetation_type), 
      data = abundance, family = poisson)
