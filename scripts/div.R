#Undergrad Dissertation
#Lea Opitz
#University of Edinburgh
#2020 data


## Diversity -----

#libraries
library(tidyverse)
library(janitor)
library(vegan)
library(wesanderson)

#load data OG----

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

### load revised data ----
# conny combined columns, I added red list column
all_data2 <- read.csv2("data/2021_Grasslands_Nationalpark_RE.csv")

sp_list2 <- all_data2 %>% slice(-c(4:8, 10:32)) %>% #remove unnecerssary rows
  janitor::row_to_names(1) #plot-ID as column name

#sp_list3 <- sp_list2 %>% arrange()

metadata <- sp_list2 %>% slice(c(1:3)) %>% 
  select(-c("Type", "red_list"))
just_sp <- sp_list2 %>% slice(-c(1:3))
  
cover <- all_data2 %>% slice(c(1, 13,15,17, 19,21,24)) %>% #remove unnecerssary rows
  janitor::row_to_names(1) %>% #plot-ID as column name
  rownames_to_column(var = "no") %>% 
  select(-c(Type, red_list, no)) %>% 
  column_to_rownames(var= "Plot-ID")

cover[is.na(cover)] <- 0 #NA <- 0
cover_long <- data.frame(t(cover)) %>% 
  rownames_to_column(var = "plot")
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

#NMDS data ----
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
#mean1 <-  aggregate(x= abundance$species,
  #          by= list(abundance$Management),
   #         FUN=mean)

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

### boxplots ----

# noe grazed, mowed, unmanaged; USED TO BE brach, mahd, weide
# > opposite order
(box_man <- ggplot(data= abundance, aes(x= as.factor(Management), y = species, fill = Management))+
  geom_boxplot(size = 0.3) +
  theme_classic()+ 
  scale_fill_manual(  #scale_fill_manual controls the colours of the 'fill' you specified in the 'ggplot' function.
    values = c("#228B22", "#7FFF00", "#EEB422"))+
  scale_x_discrete(name = "\nTypes of management") +
  scale_y_continuous(name = "Number of species\n")+
  theme(text=element_text(size = 18), axis.line = element_line(size = 0.5), axis.ticks = element_line(size = 0.5)))

res <-  boxplot(species~Management, data = abundance) #medians   13 15.0   10
res

res_rare <-  boxplot(prop_rare~Management, data = abundance) #medians all 0
res_rare

(split_plot <- ggplot(data= abundance, aes(x= as.factor(Management), y = species, fill = Management))+
  geom_boxplot(size = 0.3) +
  theme_minimal() +  
  facet_wrap(~ Vegetation_type) + # create a facet for each mountain range
    scale_fill_manual(  #scale_fill_manual controls the colours of the 'fill' you specified in the 'ggplot' function.
      values = c("#228B22", "#7FFF00", "#EEB422"))+
    scale_x_discrete(name = "\nTypes of management") +
    scale_y_continuous(name = "Number of species\n"))

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
res2 #medians: 12, 13, 13

# rare mean
rare_mean2 <- abundance %>%
  group_by(Management) %>%
  mutate(mean_by_group = mean(prop_rare)) %>% 
  mutate(standard_error(prop_rare))

(box_man_rare <- ggplot(data= abundance, aes(x= as.factor(Management), y = prop_rare, fill = Management))+
    geom_boxplot(size = 0.3) +
    theme_classic()+ 
    scale_fill_manual(  #scale_fill_manual controls the colours of the 'fill' you specified in the 'ggplot' function.
     values = c("#228B22", "#7FFF00", "#EEB422"))+
    scale_x_discrete(name = "\nTypes of management") +
    scale_y_continuous(name = "Proportion of red list species \n")+
    theme(text=element_text(size = 18), axis.line = element_line(size = 0.5), axis.ticks = element_line(size = 0.5)))

#plot from gergana
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

(funky_plot_abundance <- 
    ggplot(data = abundance, 
           aes(x = Management, y = species, fill = Management)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
    geom_point(aes(y = species, color = Management), 
               position = position_jitter(width = .15), alpha = 0.5)+#, size = .5, alpha = 0.2) +
    geom_boxplot(width = .3, outlier.shape = NA, alpha = 0.8) +
    labs(y = "Species richness\n", x = "\nTypes of Management") +
    #guides(fill = FALSE, color = FALSE) +
    theme_classic() +
    theme(legend.position = "none") +
    scale_colour_manual(values = c("#228B22", "#7FFF00", "#EEB422")) +
    scale_fill_manual(values = c("#228B22", "#7FFF00", "#EEB422"))) #+
    #geom_hline(yintercept = 0, colour = "grey30", linetype = "dashed") +
    #theme_bw() +
    #scale_y_continuous(limits = c(-0.24, 0.24),
      #                 breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
       #                labels = c("-0.2", "-0.1", "0", "0.1", "0.2")) +
    #raincloud_theme +
    #theme_LPI3())


(funky_plot_rare <- 
    ggplot(data = abundance, 
           aes(x = Management, y = prop_rare, fill = Management)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
    geom_point(aes(y = prop_rare, color = Management), 
               position = position_jitter(width = .15), alpha = 0.5)+#, size = .5, alpha = 0.2) +
    geom_boxplot(width = .3, outlier.shape = NA, alpha = 0.8) +
    labs(y = "Proportion of Red List species\n", x = "\nTypes of Management") +
    #guides(fill = FALSE, color = FALSE) +
    theme_classic() +
    theme(legend.position = "none") +
    scale_colour_manual(values = c("#228B22", "#7FFF00", "#EEB422")) +
    scale_fill_manual(values = c("#228B22", "#7FFF00", "#EEB422")))


#wes_palette("GrandBudapest1")
#c("#EEB422", "#7FFF00", "#228B22")


## Evenness ----####
# Shannon index
H <- diversity(abundance$species) #4.4077 -> the bigger the number
#Pielou’s evenness J=H′/log(S)
J <- H/log(specnumber(abundance$species)) #0.9613


#evenness per plot level or per managemnt type


#by subgroup? -> same result
sapply(abundance, class)    
abundance$species <- as.numeric(as.character(abundance$species))  # Convert one variable to numeric 

abundance3 <- abundance %>% group_by(Management)
H2 <-   diversity(abundance3$species)


#betadiv:
    #betadiver(x, method = NA, order = FALSE, help = FALSE, ...)
    #method: Bray-Curtis ((A+B-2*J)/(A+B)	"minimum"	Bray-Curtis)

betadiv <- data_long %>% betadiver(method = "bray")

designdist


#### ordination ----
#needs sp as column names


#code field trip (good veg type plot) ----
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
#meta_long %>% count(Area)

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
#code cc tut (good man plot)----

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
#df <- metaMDS(species4, distance = "bray", autotransform = FALSE) -> stress 0.17

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

#"#228B22", "#7FFF00", "#EEB422"

# Define a group variable (first 12 samples belong to group 1, last 12 samples to group 2)
#group1 = c(rep("brach", 48), rep("mahd", 21), rep("weide", 29))
group1 = c(rep("grazed", 29), rep("mowed", 21),rep("unmanaged", 48))
# Create a vector of color values with same length as the vector of group values
#colors = c(rep("brown", 48), rep("light green", 21), rep("dark green", 29))
colors = c(rep("#228B22", 29), rep("#7FFF00", 21),rep("#EEB422", 48))


# Plot convex hulls with colors based on the group identity
ordiplot(NMDS2, type = "n")
for(i in unique(group1)) {
  ordihull(NMDS2$point[grep(i, group1),], draw="polygon",
           groups = group1[group1 == i],col = colors[grep(i,group1)],label=F) } 

#orditorp(NMDS2, display = "species", col = "red", air = 0.01)
orditorp(NMDS2, display = "sites", col = c(rep("#228B22", 29), rep("#7FFF00", 21),
                                           rep("#EEB422", 48)), air = 0.01, cex = 1.25)


indic_fit3 <- envfit(NMDS2, indic_pca, permutations = 999)

plot(NMDS2, display = "sites", type = "p")
plot(indic_fit3)
colfactor <- factor(final_data$Management)
points(NMDS2, display = "sites", cex = 1, pch = 16, col = c("#228B22","#7FFF00","#EEB422")[colfactor])
#text(indic_nmds, display = "sites", cex = 1, col = c(1,2,3)[colfactor])
ordiellipse(NMDS2, final_data$Management, kind = "ehull",  col = c("#228B22","#7FFF00","#EEB422"), lwd = 3)


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

###PCA ----

species4.2 <- species4[,apply(species4, 2, var, na.rm=TRUE) != 0] #remove column with all 0?
pca1 <- prcomp(species4.2, center = TRUE,scale. = TRUE) #didn't work bc empty columns


pca1 <- prcomp(species4)
summary(pca1) #1+2 -> 78%, +3 -> 82% #after removing 0s waaay lower
str(pca1)

#library(devtools)
#install_github("vqv/ggbiplot")

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
qqnorm(abundance$species)
qqline(abundance$species, col = "blue")
shapiro.test(abundance$species) #0.0029 -> non-normal

hist(Ruck$species, breaks = 15)

library(car)
leveneTest(abundance$species, abundance$Area, center=mean) #-> 0.3437 -> over 0.05 -> equal variance
#less than our significance level of 0.05. Thus, we reject the null hypothesis and 
    #conclude that the variance among the three groups is not equal.

#Man-Whitney-U-Test:
wilcox.test(Schacht$species,Ruck$species) #p-value = 0.616
wilcox.test(Ruck$species,Hoch$species) #p-value = 0.2497
wilcox.test(Hoch$species,Schacht$species) #p-value = 0.4279

library(olsrr)
plantlm01 <- lm(species~Management, data = abundance)
ols_test_normality(plantlm01)
plantlm02 <- lm(species~Vegetation_type, data = abundance)
ols_test_normality(plantlm02)

plant
### GLMs ----
plantlm1 <- lm(species~Management*Vegetation_type, data = abundance)
summary(plantlm1)
anova(plantlm1)
ols_test_normality(plantlm1)
ols_plot_resid_hist(plantlm1)


plantlm2 <- lm(species~Management*Area, data = abundance)
summary(plantlm2)
anova(plantlm2)

plantlm3 <- lm(species~Area, data = abundance)
summary(plantlm3)
anova(plantlm3)

library(lme4)

#random effect using the syntax (1|variableName):
    #random effect to have at least five levels
# -> I have to used fixed effects

hist(abundance$species) #-> poisson?
hist(abundance$prop_rare)
glmm1 <- glmer(species ~ Management + (1|Vegetation_type), 
      data = abundance, family = poisson)
summary(glmm1)

plot(glmm1)
qqnorm(resid(glmm1))
qqline(resid(glmm1))


glmer(species ~ Management + (1|Area), 
                 data = abundance, family = poisson)

glmer(species ~ Management + (1|Vegetation_type) + (1|Area), 
      data = abundance, family = poisson)

glm2 <- glm(species ~ Management , 
              data = abundance, family = poisson)
summary(glm2)

glm2.2 <- glm(prop_rare ~ Management , 
            data = abundance, family = poisson)
summary(glm2.2)


glm3 <- glm(species ~ Vegetation_type, 
              data = abundance, family = poisson)
summary(glm3)

glm4 <- glm(species ~ Management + Vegetation_type, #veg fixed effect
            data = abundance, family = poisson)
summary(glm4) #0.064
qqnorm(resid(glm4))
qqline(resid(glm4))

glm4.2 <- glm(prop_rare ~ Management + Vegetation_type, 
            data = abundance, family = poisson)
summary(glm4.2)

glm5 <- glmer(species ~ Management + (1|Vegetation_type), #veg random effect
            data = abundance, family = poisson)
summary(glm5) #0.0677
qqnorm(resid(glm5))
qqline(resid(glm5))

glm5.2 <- glmer(prop_rare ~ Management + (1|Vegetation_type), 
              data = abundance, family = poisson)
summary(glm5.2)

glm6 <- glm(species ~ Management * Vegetation_type, #interaction with fixed effect
            data = abundance, family = poisson)
summary(glm6)
qqnorm(resid(glm6))
qqline(resid(glm6))

glm7 <- glm(species ~ Management + Vegetation_type + result_F + result_R, #fixed veg and idic
            data = final_data, family = poisson)
summary(glm7) #0.1287 unm

glm7.2 <- glm(prop_rare ~ Management + Vegetation_type + result_F + result_R, #fixed veg and idic
            data = final_data, family = poisson)


glm8 <- glm(species ~ Management + result_F + result_R , #fixed indic
            data = final_data, family = poisson)
summary(glm8) #0.0382 mowed

### plot GLM ----
predict1 <- ggpredict(glm7, terms = c("Management"), type = "fe") 
predict2 <- ggpredict(glm7.2, terms = c("Management"), type = "fe") 

(pr_abund <- ggplot()+
  geom_point(data = final_data, aes(x = Management, y= species, 
                                    colour = Vegetation_type, alpha = 0.5)) +
  geom_point(data = predict1, aes(x = x, y = predicted, size = 2)) +
  geom_errorbar(data= predict1, aes(x =x, ymax = conf.high, ymin= conf.low, width = 0.35))+
  labs(x = "\nManagement", y = "Species Richness\n") + 
  theme_classic()+
  theme(legend.position = "right"))

(pr_rare <- ggplot()+
    geom_point(data = final_data, aes(x = Management, y= prop_rare, colour = Vegetation_type, 
                                       alpha = 1)) +
    geom_point(data = predict2, aes(x = x, y = predicted, size = 2)) +
    geom_errorbar(data= predict2, aes(x =x, ymax = conf.high, ymin= conf.low, width = 0.35))+
    labs(x = "\nManagement", y = "Proportion of Red List Species\n") + 
    theme_classic()+
    theme(legend.position = "right"))

### Tukey ----
library(emmeans)

aov.plant1 <- aov(species ~ Management + Vegetation_type, data = abundance)

tukey_plants1 <- TukeyHSD(aov.plant1)
tukey_plants1

aov.plant2 <- aov(species ~ Management * Vegetation_type, data = abundance)

tukey_plants2 <- TukeyHSD(aov.plant2)
tukey_plants2

#install.packages("lsmeans")
library(lsmeans)

leastsquare1 = lsmeans(glm4,
                      pairwise ~ Management,
                      adjust="tukey")

confint(leastsquare1$contrasts)

leastsquare2 = lsmeans(glm5,
                       pairwise ~ Management,
                       adjust="tukey")

confint(leastsquare2$contrasts)

leastsquare3 = lsmeans(glm2,
                       pairwise ~ Management,
                       adjust="tukey")

confint(leastsquare3$contrasts)

leastsquare4 = lsmeans(glm4,
                       pairwise ~ Management:Vegetation_type,
                       adjust="tukey")

confint(leastsquare4$contrasts)

leastsquare5 = lsmeans(glm7,
                       pairwise ~ Management,
                       adjust="tukey")

confint(leastsquare5$contrasts)

#I think this tells me that mowed and grazed are more similar to each other than either to unmanaged
  #slightly bigger differece mowed-unmanaged

#install.packages("multcomp")
#library(multcomp)
#glm_model <- glm( y ~ categorical_variable, data = my_data)
#glht(glm2, mcp(Management = "Tukey")) #perform TukeyHSD

### indices ----
indices <- read.csv2("data/zeigerwerte.csv") %>% 
  mutate_all(na_if,"") %>% 
  mutate_all(na_if,"x")

#Ellenberg
ellenberg <- indices %>% select(-c(6:8)) 

ellenberg[is.na(ellenberg)] <- 0 #NA <- 0
ellenberg <-column_to_rownames(ellenberg, var = "sp")
ellenberg[] <- lapply(ellenberg, as.numeric)
ellenberg <- rownames_to_column(ellenberg, var = "sp")

ellen_long <- data_long %>% 
  left_join(ellenberg, by = c("sp")) %>% 
  select(-c("Type", "red_list")) #%>% 

## this isn't working bc 0.5 is read as 0,5 and therefore a character and 
    #can't be used in calculations 
sapply(ellen_long, class)    
ellen_long$cover <- as.numeric(as.character(ellen_long$cover))  # Convert one variable to numeric 


str(data_long)
ellenlong2 <- ellen_long %>% mutate(L = cover*L) %>%
  mutate(F = cover*F) %>%
  mutate(R = cover*R) %>%
  mutate(N = cover*N) %>% 
  select(-c(Vegetation_type, Management, Area)) #%>% 
 # group_by(plot)

L <- ellenlong2 %>% select(sp, plot, cover,L) %>%   
  mutate_at(vars(-group_cols()),na_if,"0") %>%  #replace empty cells with NA
  drop_na(L) #remove empty columns

L2 <- L %>% 
  group_by(plot) %>% 
  summarise(cover = sum(cover)) #%>%

L3 <- L %>% 
  group_by(plot) %>% 
  summarise(L = sum(L))

L4 <- L2 %>%  left_join(L3, by = "plot") %>%
  mutate(result_L = L/cover)


F2 <- ellenlong2 %>% select(sp, plot, cover,F) %>%   
  mutate_at(vars(-group_cols()),na_if, "0") %>%  #replace empty cells with NA
  drop_na(F) #remove empty columns
  #group_by(plot) %>%
  
  F3 <- F2 %>% 
    group_by(plot) %>% 
  summarise(cover = sum(cover)) #%>%
  
  F4 <- F2 %>% 
    group_by(plot) %>% 
  summarise(F = sum(F))

F5 <- F3 %>%  left_join(F4, by = "plot") %>%
  mutate(result_F = F/cover)


R <- ellenlong2 %>% select(sp, plot, cover,R) %>%   
  mutate_at(vars(-group_cols()),na_if,"0") %>%  #replace empty cells with NA
  drop_na(R) #remove empty columns

R2 <- R %>% 
  group_by(plot) %>% 
  summarise(cover = sum(cover)) #%>%

R3 <- R %>% 
  group_by(plot) %>% 
  summarise(R = sum(R))

R4 <- R2 %>%  left_join(R3, by = "plot") %>%
  mutate(result_R = R/cover)

N <- ellenlong2 %>% select(sp, plot, cover,N) %>%   
  mutate_at(vars(-group_cols()),na_if,"0") %>%  #replace empty cells with NA
  drop_na(N)

N2 <- N %>% 
  group_by(plot) %>% 
  summarise(cover = sum(cover)) #%>%

N3 <- N %>% 
  group_by(plot) %>% 
  summarise(N = sum(N))

N4 <- N2 %>%  left_join(N3, by = "plot") %>%
  mutate(result_N = N/cover)

ellenberg_results <- L4 %>% 
  left_join(F5, by = "plot") %>% 
  left_join(R4, by = "plot") %>% 
  left_join(N4, by = "plot") %>% 
  select(c(plot, result_L, result_F, result_R, result_N)) %>% 
  left_join(meta_long, by = "plot")

#Briemle
briemle <- indices %>% select(-c(2:5))

briemle[is.na(briemle)] <- 0 #NA <- 0
briemle <-column_to_rownames(briemle, var = "sp")
briemle[] <- lapply(briemle, as.numeric)
briemle <- rownames_to_column(briemle, var = "sp")

briemle_long <- data_long %>% 
  left_join(briemle, by = c("sp")) %>% 
  select(-c("Type", "red_list")) #%>% 

briemle_long$cover <- as.numeric(as.character(briemle_long$cover))

briemle_long2 <- briemle_long %>% mutate(mowing_tol = cover*mowing_tol) %>%
  mutate(grazing_tol = cover*grazing_tol) %>%
  mutate(foraging_val = cover*foraging_val) 

mow <- briemle_long2 %>% select(sp, plot, cover, mowing_tol) %>%   
  mutate_at(vars(-group_cols()),na_if,"0") %>%  #replace empty cells with NA
  drop_na(mowing_tol) #remove empty columns

mow2 <- mow %>% 
  group_by(plot) %>% 
  summarise(cover = sum(cover)) #%>%

mow3 <- mow %>% 
  group_by(plot) %>% 
  summarise(mowing_tol = sum(mowing_tol))

mow4 <- mow2 %>%  left_join(mow3, by = "plot") %>%
  mutate(result_mow = mowing_tol/cover)

graze <- briemle_long2 %>% select(sp, plot, cover, grazing_tol) %>%   
  mutate_at(vars(-group_cols()),na_if,"0") %>%  #replace empty cells with NA
  drop_na(grazing_tol) #remove empty columns

graze2 <- graze %>% 
  group_by(plot) %>% 
  summarise(cover = sum(cover)) #%>%

graze3 <- graze %>% 
  group_by(plot) %>% 
  summarise(grazing_tol = sum(grazing_tol))

graze4 <- graze2 %>%  left_join(graze3, by = "plot") %>%
  mutate(result_graze = grazing_tol/cover)

forage <- briemle_long2 %>% select(sp, plot, cover, foraging_val) %>%   
  mutate_at(vars(-group_cols()),na_if,"0") %>%  #replace empty cells with NA
  drop_na(foraging_val) #remove empty columns

forage2 <- forage %>% 
  group_by(plot) %>% 
  summarise(cover = sum(cover)) #%>%

forage3 <- forage %>% 
  group_by(plot) %>% 
  summarise(foraging_val = sum(foraging_val))

forage4 <- forage2 %>%  left_join(forage3, by = "plot") %>%
  mutate(result_forage = foraging_val/cover)

indicator_results <- L4 %>% 
  left_join(F5, by = "plot") %>% 
  left_join(R4, by = "plot") %>% 
  left_join(N4, by = "plot") %>% 
  left_join(mow4, by = "plot") %>% 
  left_join(graze4, by = "plot") %>%
  left_join(forage4, by = "plot") %>% 
  select(c(plot, result_L, result_F, result_R, result_N, 
           result_mow, result_graze, result_forage)) #%>% 



#PCA indic ----

final_data <- abundance %>% 
  left_join(indicator_results, by = "plot") %>% 
  left_join(cover_long, by = "plot") %>% 
  arrange(Management)

indic_pca <- final_data %>% 
  select(-c(3:6, 15:20)) %>% #take meta and cover out
  column_to_rownames(var = "plot")
indic_pca[] <- lapply(indic_pca, as.numeric)


pca2 <- prcomp(indic_pca)
summary(pca2)


#library(ggbiplot)
#library(gridExtra)
#ggbiplot(pca2, labels=rownames(indicator_results), groups = group1)

adon.results2 <-adonis(indic_pca ~ group1, method="bray",perm=999)
print(adon.results2)

## Bray-Curtis distances between samples
dis <- vegdist(indic_pca)

## Calculate multivariate dispersions
mod <- betadisper(dis, group1)
mod

set.seed(2) 
indic_nmds <- metaMDS(indic_pca, distance = "bray", autotransform = FALSE)

indic_fit <- envfit(indic_nmds, indic_pca, permutations = 999)

stressplot(indic_nmds)

head(indic_fit)

plot(indic_nmds, display = "sites", type = "p")
plot(indic_fit)
colfactor <- factor(final_data$Management)
points(indic_nmds, display = "sites", cex = 1, pch = 16, col = c("#228B22","#7FFF00","#EEB422")[colfactor])
#text(indic_nmds, display = "sites", cex = 1, col = c(1,2,3)[colfactor])
ordiellipse(indic_nmds, final_data$Management, kind = "ehull",  col = c("#228B22","#7FFF00","#EEB422"), lwd = 3)
#with(final_data, graphics::legend("topleft", levels(Management), pch = c("#228B22","#7FFF00","#EEB422",) title = "Management"))


ind_NMDS <- metaMDS(indic_pca, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")

ordiplot(ind_NMDS, type = "n")
orditorp(ind_NMDS, display = "sites", col = c(rep("brown", 48), rep("light green", 21), 
                                           rep("dark green", 29)), air = 0.01, cex = 1.25)

stressplot(ind_NMDS)

# Define a group variable (first 12 samples belong to group 1, last 12 samples to group 2)
group1 = c(rep("grazed", 29), rep("mowed", 21),rep("unmanaged", 48))
# Create a vector of color values with same length as the vector of group values
colors = c(rep("#228B22", 29), rep("#7FFF00", 21),rep("#EEB422", 48))

## with cover
indic_pca2 <- final_data %>% 
  select(-c(3:6)) %>% #take meta out
  column_to_rownames(var = "plot")
indic_pca2[] <- lapply(indic_pca2, as.numeric)


pca3 <- prcomp(indic_pca2)
summary(pca3)

adon.results3 <-adonis(indic_pca2 ~ group1, method="bray",perm=999)
print(adon.results3)

## Bray-Curtis distances between samples
dis2 <- vegdist(indic_pca2)

## Calculate multivariate dispersions
mod2 <- betadisper(dis2, group1)
mod2

set.seed(2) 
indic_nmds2 <- metaMDS(indic_pca2, distance = "bray", autotransform = FALSE)

indic_fit2 <- envfit(indic_nmds2, indic_pca2, permutations = 999)

stressplot(indic_nmds2)

head(indic_fit)

plot(indic_nmds2, display = "sites", type = "p")
plot(indic_fit2)
colfactor <- factor(final_data$Management)
points(indic_nmds2, display = "sites", cex = 1, pch = 16, col = c("#228B22","#7FFF00","#EEB422")[colfactor])
#text(indic_nmds, display = "sites", cex = 1, col = c(1,2,3)[colfactor])
ordiellipse(indic_nmds2, final_data$Management, kind = "ehull",  col = c("#228B22","#7FFF00","#EEB422"), lwd = 3)
#with(final_data, graphics::legend("topleft", levels(Management), pch = c("#228B22","#7FFF00","#EEB422",) title = "Management"))




# Plot convex hulls with colors based on the group identity
ordiplot(ind_NMDS, type = "n")
for(i in unique(group1)) {
  ordihull(ind_NMDS$point[grep(i, group1),], draw="polygon",
           groups = group1[group1 == i],col = colors[grep(i,group1)],label=F) } 

orditorp(ind_NMDS, display = "sites", col = c(rep("#228B22", 29), rep("#7FFF00", 21),
                                           rep("#EEB422", 48)), air = 0.01, cex = 1.25)
