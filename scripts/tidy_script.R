#actual tidy script a concept

### libraries ----
library(tidyverse)
library(janitor)
library(vegan)
library(lme4)
library(ggeffects)
library(viridis)
library(wesanderson)
library(emmeans)
library(lsmeans)
library(betapart)

library(dplyr)

### load 2020 data ----
data <- read.csv2("data/2021_Grasslands_Nationalpark_RE.csv")

sp_list <- data %>% slice(-c(4:8, 10:32)) %>% #remove unnecerssary rows
  janitor::row_to_names(1) #plot-ID as column name

#extract meta data
metadata <- sp_list %>% slice(c(1:3)) %>% 
  select(-c("Type", "red_list"))

#extract species abundances
just_sp <- sp_list %>% slice(-c(1:3))

#extract cover of layers
cover <- data %>% slice(c(1, 13,15,17, 19,21,24)) %>% #remove unnecerssary rows
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

#get rare species  
rare_sp <- data_long %>% group_by(plot) %>% 
  count(red_list, name = "rare") %>% 
  subset(red_list!="N")

#calculate number of species per plot
abundance <- data_long %>% count(plot, name = "species")%>% 
  full_join(rare_sp, by = c("plot"))  %>% 
  full_join(meta_long, by = c("plot")) %>% 
  mutate(prop_rare = rare/ species) %>% 
  select(-c(red_list))
abundance[is.na(abundance)] <- 0 
abundance2 <- abundance %>% select(-c(rare, prop_rare))   


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
           result_mow, result_graze, result_forage))

final_data <- abundance %>% 
  left_join(indicator_results, by = "plot") %>% 
  left_join(cover_long, by = "plot") %>% 
  arrange(Management)

#NMDS data ----
species <- just_sp %>% select(-c("Type", "red_list")) #delete unnecessary columns

species <- data.frame(t(species)) #switch rows and columns

species <- species %>% janitor::row_to_names(1) #sp as column names 
species[is.na(species)] <- 0 #NA <- 0
species[] <- lapply(species, as.numeric)

#sort by veg type
species3 <- species %>% rownames_to_column(var = "plot") %>% 
  left_join(meta_long, by = c("plot")) %>% 
  arrange(Vegetation_type) %>% 
  #select(-c(155:158)) %>% 
  select(-c(155:157)) %>% #delete meta data again
  #select(-c("Sphagnum sec.", "Climatium dendroides", "Plagiomnium undulatum")) %>%
  column_to_rownames(var = "plot")

species3[is.na(species3)] <- 0 #NA <- 0
species3[] <- lapply(species3, as.numeric)

#sort by management
species4 <- species %>% rownames_to_column(var = "plot") %>% 
  left_join(meta_long, by = c("plot")) %>% 
  arrange(Management) %>% 
  select(-c(155:157)) %>% 
  column_to_rownames(var = "plot")

species4[is.na(species4)] <- 0 #NA <- 0
species4[] <- lapply(species4, as.numeric)

#### Species richness and number of rare species ----
standard_error <- function(x) sd(x) / sqrt(length(x)) # Create own function

mean3 <- abundance2 %>%
  group_by(Vegetation_type) %>%
  mutate(mean_by_group = mean(species)) %>% 
  mutate(standard_error(species))

rare_mean2 <- abundance %>%
  group_by(Management) %>%
  mutate(mean_by_group = mean(prop_rare)) %>% 
  mutate(standard_error(prop_rare))

(split_plot <- ggplot(data= abundance, aes(x= as.factor(Management), y = species, fill = Management))+
    geom_boxplot(size = 0.3) +
    theme_minimal() +  
    facet_wrap(~ Vegetation_type) + # create a facet for each mountain range
    scale_fill_manual(  #scale_fill_manual controls the colours of the 'fill' you specified in the 'ggplot' function.
      values = c("#228B22", "#7FFF00", "#EEB422"))+
    scale_x_discrete(name = "\nTypes of management") +
    scale_y_continuous(name = "Number of species\n"))

#half violin plots
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

(funky_plot_abundance <- 
    ggplot(data = abundance, 
           aes(x = Management, y = species, fill = Management)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
    geom_point(aes(y = species, color = Management), 
               position = position_jitter(width = .15), alpha = 0.5)+
    geom_boxplot(width = .3, outlier.shape = NA, alpha = 0.8) +
    labs(y = "Species richness\n", x = "\nTypes of Management") +
    #guides(fill = FALSE, color = FALSE) +
    theme_classic() +
    theme(legend.position = "none") +
    scale_colour_manual(values = c("#02401B", "#81A88D", "#D8B70A")) +
    scale_fill_manual(values = c("#02401B", "#81A88D", "#D8B70A"))) 
#scale_fill_manual(values = c("#228B22", "#7FFF00", "#EEB422"))) 

#9C964A graze, #CDC08C mow, #FAD77B umn
#c("#9C964A", "#CDC08C", "#FAD77B")

(funky_plot_rare <- 
    ggplot(data = abundance, 
           aes(x = Management, y = prop_rare, fill = Management)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
    geom_point(aes(y = prop_rare, color = Management), 
               position = position_jitter(width = .15), alpha = 0.5)+
    geom_boxplot(width = .3, outlier.shape = NA, alpha = 0.8) +
    labs(y = "Proportion of Red List species\n", x = "\nTypes of Management") +
    #guides(fill = FALSE, color = FALSE) +
    theme_classic() +
    theme(legend.position = "none") +
    scale_colour_manual(values = c("#02401B", "#81A88D", "#D8B70A")) +
    scale_fill_manual(values = c("#02401B", "#81A88D", "#D8B70A")))

#D8B70A unm ,#02401B graze ,#81A88D mow OR #A2A475 unm or khaki3
#### ordination ----
set.seed(2)
NMDS <- metaMDS(species3, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")
NMDS2 <- metaMDS(species4, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")

stressplot(NMDS2)


group1 = c(rep("grazed", 29), rep("mowed", 21),rep("unmanaged", 48))
colors1 = c(rep("#228B22", 29), rep("#7FFF00", 21),rep("#EEB422", 48))

group2 = c(rep("Calthion", 18), rep("Carex", 21),
           rep("Mountain Meadow", 29), rep("Nardetum", 30))
colors2 = c(rep("#F1BB7B", 18), rep("#FD6467", 21),
            rep("#5B1A18", 29), rep("#D67236", 30))

colour_wes <- wes_palette("GrandBudapest1")
colour_wes2 <- wes_palette("Moonrise3") #9C964A graze, #CDC08C mow, #FAD77B umn
colour_wes3 <- wes_palette("Cavalcanti1") #D8B70A unm ,#02401B graze ,#81A88D mow

colour_wes3 #A2A475
indic_pca <- final_data %>% 
  select(-c(3:6, 15:20)) %>% #take meta and cover out
  column_to_rownames(var = "plot")
indic_pca[] <- lapply(indic_pca, as.numeric)

indic_pca2 <- final_data %>% 
  select(-c(3:6)) %>% #take out
  column_to_rownames(var = "plot")
indic_pca2[] <- lapply(indic_pca2, as.numeric)



indic_fit <- envfit(NMDS2, indic_pca2, permutations = 999)
#indic_fit2 <- envfit(NMDS, indic_pca, permutations = 999)
indic_fit

ord = metaMDS(species4)
env <- envfit(ord ~ species + prop_rare + result_R + result_N + result_mow + #keep r^2 > 0.3 
                result_forage + Vegetation_total + Grasses + Herbs, data = indic_pca2, perm = 999)


plot(ord, display = "sites", type = "p")
plot(env, col = "black")
colfactor <- factor(final_data$Vegetation_type)

with(final_data, points(ord, disp = "sites", pch = as.numeric(Vegetation_type)))
with(final_data, legend("bottomright", levels(Vegetation_type), pch = 1:4, title = "Vegetation type"))


points(ord, display = "sites", cex = 1, pch = 16, col = c("#228B22","#7FFF00","#EEB422")[colfactor])
ordiellipse(ord, final_data$Management, kind = "ehull",  col = c("#228B22","#7FFF00","#EEB422"), lwd = 3)

env15 <- envfit(ord ~  result_forage +  Agrostis.capillaris + Achillea.millefolium, 
                data=Environment3, perm=999)


#ordination management
plot(NMDS2, display = "sites", type = "p")
plot(env15, col = "black", labels = FALSE)
colfactor <- factor(final_data$Management)
points(NMDS2, display = "sites", cex = 1, pch = 16, 
       col = c("#02401B", "#81A88D", "#D8B70A")[colfactor])
ordiellipse(NMDS2, final_data$Management, kind = "ehull",  
            col = c("#02401B", "#81A88D", "#D8B70A"), lwd = 3)

colfactor <- factor(final_data$Vegetation_type)
points(ord, display = "sites", cex = 1, pch = 16, 
       col = c("#F1BB7B","#FD6467","#5B1A18", "#D67236" )[colfactor])
ordiellipse(ord, final_data$Vegetation_type, kind = "ehull",  
            col = c("#F1BB7B","#FD6467","#5B1A18", "#D67236" ), lwd = 3)


#ordination vegetation type
plot(NMDS, display = "sites", type = "p")
plot(indic_fit, col = "black")

for(i in unique(group2)) {
  ordiellipse(NMDS$point[grep(i, group2),], kind = "ehull", lwd = 3,
           groups = group2[group2 == i],col = colors2[grep(i,group2)],label=F) } 

points(NMDS, display = "sites",col = colors2, cex = 1, pch = 16)
with(final_data, legend("bottomright", levels(Vegetation_type), pch = 1:4, title = "Vegetation type"))


### GLMs ----

glm_abu <- glmer(species ~ Management + (1|Vegetation_type) + scale(result_F) + scale(result_N), #fixed veg and idic
              data = final_data, family = poisson)
summary(glm_abu)

glm7 <- glmer(species ~ Management + (1|Vegetation_type) + result_F + result_R, #fixed veg and idic
                 data = final_data, family = poisson)
summary(glm7)

glm72 <- lm(species ~ Management + Vegetation_type + result_F + result_R, #fixed veg and idic
              data = final_data)

glm_rare <- glm(prop_rare ~ Management + Vegetation_type + scale(result_F) + scale(result_N), #fixed veg and idic
                   data = final_data, family = quasipoisson)
summary(glm_rare)

glm_rare2 <- lm(prop_rare ~ Management + Vegetation_type + scale(result_F) + scale(result_N), #fixed veg and idic
                data = final_data) 

par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(glm_rare)

car::ncvTest(glm_rare2) # Chisquare = 0.08677912, Df = 1, p = 0.76831
car::ncvTest(glm72) #Chisquare = 1.780828, Df = 1, p = 0.18205


library(DHARMa)
summary(glm_abu)
simulationOutput <- simulateResiduals(fittedModel = glm08, n = 250)
plot(simulationOutput)
r <- simulationOutput$scaledResiduals
r1 <- simulationOutput$scaledResidualsNormal
plot(r)
plot(r1)
testResiduals(simulationOutput)
testUniformity(simulationOutput)
testOutliers(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
testGeneric(simulationOutput)
testTemporalAutocorrelation(simulationOutput)
testSpatialAutocorrelation(simulationOutput)


simulationOutput2 <- simulateResiduals(fittedModel = glm_rare, n = 250)
plot(simulationOutput2)
r <- simulationOutput2$scaledResiduals
r1 <- simulationOutput2$scaledResidualsNormal
plot(r)
plot(r1)
testResiduals(simulationOutput2)
testUniformity(simulationOutput2)
testOutliers(simulationOutput2)
testDispersion(simulationOutput2)
testZeroInflation(simulationOutput2)
testGeneric(simulationOutput2)
testTemporalAutocorrelation(simulationOutput2)
testSpatialAutocorrelation(simulationOutput2)

### plot models ----
predict1 <- ggpredict(glm_abu, terms = c("Management"), type = "fe") 
predict2 <- ggpredict(glm_rare, terms = c("Management"), type = "fe") 

cols <- c("Calthion" = "#F1BB7B", "Carex brizoides" = "#FD6467", "Mountain Meadow" = "#5B1A18", "Nardetum" = "#D67236")

(pr_abund <- ggplot()+
    geom_point(data = final_data, aes(x = Management, y= species,
                                      colour = Vegetation_type, alpha = 0.5),
               position = position_jitter(width = .15)) +
    guides(alpha = FALSE)+
    geom_point(data = predict1, aes(x = x, y = predicted, size = 2),show.legend = FALSE)  +
    geom_errorbar(data= predict1, aes(x =x, ymax = conf.high, ymin= conf.low, width = 0.35))+
    labs(x = "\nManagement", y = "Predicted Species Richness\n") + 
    theme_classic()+
    scale_colour_manual(values = cols, aesthetics = c("colour", "fill"), name = "Vegetation type")+
    theme(legend.position = "right"))

(pr_rare <- ggplot()+
    geom_point(data = final_data, aes(x = Management, y= prop_rare, colour = Vegetation_type, 
                                      alpha = 1), position = position_jitter(width = .15)) +
    guides(alpha = FALSE)+
    geom_point(data = predict2, aes(x = x, y = predicted, size = 2),show.legend = FALSE) +
    geom_errorbar(data= predict2, aes(x =x, ymax = conf.high, ymin= conf.low, width = 0.35))+
    labs(x = "\nManagement", y = "Predicted Proportion of Red List Species\n") + 
    theme_classic()+
    scale_colour_manual(values = cols, aesthetics = c("colour", "fill"), name = "Vegetation type")+
    theme(legend.position = "right"))


### Tukey ----
leastsquare5 = lsmeans(glm7,
                       pairwise ~ Management,
                       adjust="tukey")

confint(leastsquare5$contrasts)

leastsquare6 = lsmeans(glm7.2,
                       pairwise ~ Management,
                       adjust="tukey")

confint(leastsquare6$contrasts)


write.csv2(species4, "C:/Users/Lea/Documents/R/git/bachelor_diss/data/NMDS_species.csv", row.names = TRUE)
write.csv2(indic_pca, "C:/Users/Lea/Documents/R/git/bachelor_diss/data/NMDS_envir.csv", row.names = TRUE)
write.csv2(final_data, "C:/Users/Lea/Documents/R/git/bachelor_diss/data/final_data.csv", row.names = FALSE)

### load timeseries data ----
data <- read.csv2("data/ruc_timeseries.csv", dec = ",") 

###data wrangling ----

colnames(data) <- data[10, ] 

data2 <- data %>% subset(var!="Höh") %>% 
  mutate_all(na_if,"") %>% 
  mutate_all(na_if,"-") %>%
  select(-c(var)) %>% 
  slice(-c(1:3, 6,8, 11:23, 25, 168:170))

sp <- data2 %>% slice(-c(1:21))

sp2 <- data2 %>% slice(-c(1:21)) %>% 
  select(-c(type, red_list)) %>% 
  column_to_rownames(var = "ID")

meta <- data2 %>% slice(c(1:4, 6))%>% 
  select(-c(type, red_list))

meta2 <- data2 %>% slice(c(1:4, 6))%>% 
  select(-c(type, red_list)) %>% 
  column_to_rownames(var = "ID")

sp.long <- sp %>%
  pivot_longer(-c("type","red_list", "ID"), names_to = "plot", values_to = "cover") %>% 
  drop_na(cover)


meta.long <- data.frame(t(meta[]))
colnames(meta.long) <- meta[, 1]
meta.long <- meta.long %>% rownames_to_column(var = "plot") %>% 
  slice(-c(1))


#meta_long2 <- meta_long %>% column_to_rownames(var = "plot")

timeseries.long <- full_join(sp.long, meta.long, by = c("plot"))


#timeseries.long$year[timeseries.long$year  %in% c(“2020”)] <- “2019”
rare.sp <- timeseries.long %>% group_by(plot) %>% 
  count(red_list, name = "rare") %>% 
  subset(red_list!="N")

timeseries.long %>% count(red_list)
timeseries.long$cover <- as.numeric(as.character(timeseries.long$cover))

series.abundance <- timeseries.long %>% count(plot, name = "species")%>% 
  full_join(rare.sp, by = c("plot"))  %>% 
  full_join(meta.long, by = c("plot")) %>% 
  mutate(prop.rare = rare/ species) %>% 
  select(-c(red_list)) %>% arrange((Transect))

series.abundance[is.na(series.abundance)] <- 0 

mean5 <- series.abundance %>%
  group_by(year) %>%
  mutate(mean_by_group_sp = mean(species)) %>% 
  mutate(standard_error(species)) %>% 
  mutate(mean_by_group_rare = mean(prop.rare)) %>% 
  mutate(standard_error(prop.rare))

mean6 <- series.abundance %>% filter(!(Transect %in% c("J1", "J2"))) %>% 
  group_by(year) %>%
  mutate(mean_by_group_sp = mean(species)) %>% 
  mutate(standard_error(species)) %>% 
  mutate(mean_by_group_rare = mean(prop.rare)) %>% 
  mutate(standard_error(prop.rare))

### random plots ----

(time_abu <- ggplot() +
   geom_line(data = mean6, aes(x = year+2013, y = mean_by_group_sp),
             size = 1) +
   #geom_ribbon(data = pred.mm1, aes(ymin = conf.low, ymax = conf.high, x = x+2013), alpha = 0.3) +
   geom_point(data = mean6, aes(x = year + 2013, y = species, colour = Transect),
              alpha = 0.3, size = 2, position = position_jitter(width = .1)) +
   #annotate("text", x = 2017, y = 32, label = "Slope = 0.021, Std. error = 0.008") +  
   #scale_y_continuous(limits = c (-1, 1)) +
   theme_classic() +
   scale_colour_viridis(discrete = TRUE, option="plasma")+
   labs(x = "\nYear", y = "Species Richness\n"))

(time_rare <- ggplot() +
    geom_line(data = mean6, aes(x = year+2013, y = mean_by_group_rare), size = 1) +
    #geom_ribbon(data = pred.mm1, aes(ymin = conf.low, ymax = conf.high, x = x+2013), alpha = 0.3) +
    geom_point(data = mean6, aes(x = year + 2013, y = prop.rare, colour = Transect),
               alpha = 0.3, size = 2, position = position_jitter(width = .1)) +
    #annotate("text", x = 2017, y = 32, label = "Slope = 0.021, Std. error = 0.008") +  
    #scale_y_continuous(limits = c (-1, 1)) +
    theme_classic() +
    scale_colour_viridis(discrete = TRUE, option="plasma")+
    labs(x = "\nYear", y = "Species Richness\n"))



### GLMs ----
series.abundance$year <- parse_number(as.character(series.abundance$year))
series.abundance$year <- series.abundance$year - 2013

series.abundance2 <- series.abundance %>% 
  filter(!(rare %in% c("0"))) %>% 
  filter(!(year %in% c("7")))

hist(series.abundance2$prop.rare)

glm08 <- glmer(species ~ year + (1|Aufnahme_Nr), 
               data = series.abundance, family = poisson)


glm08.3 <- glm(prop.rare ~ year + Aufnahme_Nr, 
               data = series.abundance, family = quasipoisson)

summary(glm08.3)

glm08.4 <- glm(prop.rare ~ year + Aufnahme_Nr, 
               data = series.abundance2, family = quasipoisson)

summary(glm08.4)

### plot GLMs----

# Plot the predictions 
pred.mm1 <- ggpredict(glm08, terms = c("year")) 
pred.mm3 <- ggpredict(glm08.3, terms = c("year"))
pred.mm4 <- ggpredict(glm08.4, terms = c("year"))

(mixed_effects <- ggplot() +
   geom_line(data = pred.mm1, aes(x = x+2013, y = predicted),
             size = 1) +
   geom_line(data = mean5, aes(x = year+2013, y = mean_by_group_sp), 
             size = 2, colour = "#8B1A1A") +
   geom_ribbon(data = pred.mm1, aes(ymin = conf.low, ymax = conf.high, x = x+2013), alpha = 0.3) +
   geom_point(data = series.abundance, aes(x = year + 2013, y = species, colour = Transect),
              alpha = 0.3, size = 2, position = position_jitter(width = .1)) +
   annotate("text", x = 2017, y = 32, label = "Slope = 0.021, Std. error = 0.008") +  
   #scale_y_continuous(limits = c (-1, 1)) +
   theme_classic() +
   scale_colour_viridis(discrete = TRUE, option="plasma")+
   labs(x = "\nYear", y = "Species Richness\n"))

(mixed_effects2 <- ggplot() +
    geom_line(data = pred.mm3, aes(x = x+2013, y = predicted),
              size = 1) +
    geom_line(data = mean5, aes(x = year+2013, y = mean_by_group_rare),
              size = 2, colour = "#8B1A1A") +
    geom_ribbon(data = pred.mm3, aes(ymin = conf.low, ymax = conf.high, x = x+2013), alpha = 0.1) +
    geom_point(data = series.abundance, aes(x = year + 2013, y = prop.rare, colour = Transect),
               alpha = 0.3, size = 2, position = position_jitter(width = .2)) +
    annotate("text", x = 2017, y = 0.02, label = "Slope = -0.001, Std. error = 0.002") +  
    #scale_y_continuous(limits = c (-1, 1)) +
    theme_classic() +
    scale_colour_viridis(discrete = TRUE, option="plasma")+
    labs(x = "\nYear", y = "Proportion of Red List Species\n"))

### facetted plots
(series.p <- ggplot(series.abundance, aes(x = year + 2013, y = species, colour = veg_type)) +
    geom_point() +
    geom_smooth(method = glm, alpha = 0.3) + #colour = "#EEB422", fill = "#EEB422"
    facet_wrap(~Weide, nrow=1) +   # a panel for each transekt
    #scale_x_continuous(breaks = c(1975, 1980, 1985, 1990, 1995, 2000, 2005)) +
    theme_classic() +
    scale_colour_viridis(discrete = TRUE, option="plasma")+
    labs(x = "\nYear", y = " Predicted Species Richness\n"))

(series.p2 <- ggplot(series.abundance, aes(x = year + 2013, y = species, colour = Transect)) +
    geom_point() +
    geom_smooth(method = glm, alpha = 0.3) +
    facet_wrap(~Transekt, nrow=3) +   # a panel for each transekt
    #scale_x_continuous(breaks = c(1975, 1980, 1985, 1990, 1995, 2000, 2005)) +
    theme_classic() +
    scale_colour_viridis(discrete = TRUE, option="plasma")+
    labs(x = "\nYear", y = " Predicted Species Richness\n"))


(series.p3 <- ggplot(series.abundance, aes(x = year + 2013, y = prop.rare, colour = Transect)) +
    geom_point() +
    geom_smooth(method = glm, alpha = 0.3) + #colour = "#EEB422", fill = "#EEB422"
    facet_wrap(~Weide, nrow=1) +   # a panel for each transekt
    #scale_x_continuous(breaks = c(1975, 1980, 1985, 1990, 1995, 2000, 2005)) +
    theme_classic() +
    scale_colour_viridis(discrete = TRUE, option="plasma")+
    labs(x = "\nYear", y = " Predicted Proportion of Red List Species\n"))

### Bray Curtis ----

data <- series.abundance
str(data)
data$year <- as.numeric(as.character(data$year))  # Convert one variable to numeric 
data2 <- timeseries.long
data2$year <- as.numeric(as.character(data2$year))



# loop to calculate biodiversity metrics ----
beta_Bray <- data.frame(matrix(ncol = 7, nrow = length(unique(data$Aufnahme_Nr)))) 
names(beta_Bray) <- c("AreaName", "duration", "richness", "richness_change", "Bbal", "Bgra", "Bbray") 

# beta_Jaccard <- data.frame(matrix(ncol = 7, nrow = length(unique(data$SiteSubsite)))) 
# names(beta_Jaccard) <- c("SiteSubsite", "duration", "richness", "richness_change", "Jbeta", "Jtu", "Jne")

i = 1

# for loop with betapart ----
for (i in 1:length(unique(data2$Aufnahme_Nr))) {
  AreaName <- as.character(unique(data2$Aufnahme_Nr)[i])
  sub_bio_abundance <- filter(data2, Aufnahme_Nr == AreaName)
  YearMin <- min(sub_bio_abundance$year)
  YearMax <- max(sub_bio_abundance$year)
  duration <- YearMax - YearMin
  # filters the dataframe for just the first and last observations per plot
  sub_bio_abundance_min <- filter(sub_bio_abundance, year == YearMin)
  sub_bio_abundance_max <- filter(sub_bio_abundance, year == YearMax)
  sub_bio_abundance <- rbind(sub_bio_abundance_min, sub_bio_abundance_max)
  richness <- length(unique(sub_bio_abundance$ID))
  # averages any species that have multiple records per plot and time point 
  sub_bio_abundance <- sub_bio_abundance %>% group_by(Aufnahme_Nr, ID) %>% 
    filter(sum(cover) > 0) %>% 
    group_by(Aufnahme_Nr, ID, year) %>% 
    summarise(cover = mean(cover)) %>%
    ungroup() 
  #sub_bio_abundance <- sub_bio_abundance %>% group_by(SiteSubsite, GENUS_SPECIES, YEAR) %>% summarise(Abundance = mean(Abundance)) %>% ungroup()
  # reshape to wide form
  sub_bio_abundance_wider <- pivot_wider(sub_bio_abundance, names_from = ID, 
                                         values_from = cover, 
                                         values_fill = list(cover = 0))
  # removes columns for beta.pair() function
  sub_bio_abundance_matrix <- dplyr::select(sub_bio_abundance_wider, -Aufnahme_Nr, -year) 
  
  # creates presence-absence matrix
  sub_bio_presence_matrix <- with(sub_bio_abundance_matrix, ifelse(sub_bio_abundance_matrix > 0,1,0))
  
  # calculates Jaccard overall, turnover and nestedness
  # J_components <- beta.pair(sub_bio_presence_matrix, index.family='jaccard')
  B_components <- beta.pair.abund(sub_bio_abundance_matrix, index.family='bray')
  # saves biodiversity metrics
  richness_change <- rowSums(sub_bio_presence_matrix)[2] - rowSums(sub_bio_presence_matrix)[1]
  # Jbeta <- J_components$beta.jac
  # Jtu <- J_components$beta.jtu
  # Jne <- J_components$beta.jne
  Bbal <- B_components$beta.bray.bal
  Bgra <- B_components$beta.bray.gra
  Bbray <- B_components$beta.bray
  # beta_Jaccard[i,] <- c(SiteSubSiteName, duration, richness, richness_change, Jbeta, Jtu, Jne)
  beta_Bray[i,] <- c(AreaName, duration, richness, richness_change, Bbal, Bgra, Bbray)
  i = i+1
}

beta_Bray
beta_Bray <-column_to_rownames(beta_Bray, var = "AreaName")
beta_Bray[] <- lapply(beta_Bray, as.numeric)
beta_Bray <- rownames_to_column(beta_Bray, var = "AreaName")

bray_results <- beta_Bray %>% 
  left_join(meta.long,by = c("AreaName"="Aufnahme_Nr")) %>% 
  select(-c(year)) %>% 
  arrange(Transect)

mean5 %>% ungroup() %>% count(rare)
### glm bray results ----

hist(bray_results$Bbray)


glm_bray <- lm(Bbray ~ Transect , 
            data = bray_results)
summary(glm_bray)
plot(glm_bray)
pred.bray <- ggpredict(glm_bray, terms = c("Transect"))  # this gives overall predictions for the model
plot(pred.bray) + theme_classic() + 
  labs(x = "\n Transects", y = "Bray-Curtis dissimilarity index\n") 
summary(pred.bray)

