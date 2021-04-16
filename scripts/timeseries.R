#Undergrad Dissertation
#Lea Opitz
#University of Edinburgh
#time series analysis

#### load libraries and data ----

library(tidyverse)
library(vegan)
library(betapart)
library(lme4)



data <- read.csv2("data/ruc_timeseries.csv", dec = ",") 

###data wrangling ----

colnames(data) <- data[10, ] 

data2 <- data %>% subset(var!="Höh") %>% 
  mutate_all(na_if,"") %>% 
  mutate_all(na_if,"-") %>%
  select(-c(var)) %>% 
  slice(-c(1:3, 6:8, 11:23, 25, 168:170))


#need to combine Aufbahmejahr: & Aufnahme-Nr.: to get distinct names

#colnames(data2) <- data2[2, ] 
sp <- data2 %>% slice(-c(1:20))

sp2 <- data2 %>% slice(-c(1:20)) %>% 
  select(-c(type, red_list)) %>% 
  column_to_rownames(var = "ID")

meta <- data2 %>% slice(c(1:3, 5))%>% 
  select(-c(type, red_list))

meta2 <- data2 %>% slice(c(1:3, 5))%>% 
  select(-c(type, red_list)) %>% 
  column_to_rownames(var = "ID")

sp.long <- sp %>%
  pivot_longer(-c("type","red_list", "ID"), names_to = "plot", values_to = "cover") %>% 
  drop_na(cover) #%>% 
  #rename(c("sp" = "Plot-ID")) %>% 
  #group_by(plot)


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

series.abundance <- timeseries.long %>% count(plot, name = "species")%>% 
  full_join(rare.sp, by = c("plot"))  %>% 
  full_join(meta.long, by = c("plot")) %>% 
  mutate(prop.rare = rare/ species) %>% 
  select(-c(red_list))

series.abundance[is.na(series.abundance)] <- 0 
str(series.abundance)


timeseries.long$cover <- as.numeric(as.character(timeseries.long$cover))

#series.abundance <- timeseries.long %>% count(plot, name = "species") %>% 
 # full_join(meta.long, by = c("plot"))
#abun.transect <- series.abundance %>% group_by(Transekt.Nr..)

(colour_plot <- ggplot(series.abundance, aes(x = year, y = species, colour = Aufnahme_Nr)) +
    geom_point(size = 2) +
    facet_wrap(~ Transekt) +
    theme_classic() +
    theme(legend.position = "none"))
#dev.off()

### GLMs ----
hist(series.abundance$species)
glm02 <- glm(species ~ year , 
            data = series.abundance, family = poisson)
summary(glm02)

glm03 <- glm(species ~ year + Aufnahme_Nr, 
             data = series.abundance, family = poisson)
summary(glm03)

glm04 <- glm(species ~ year * Aufnahme_Nr, 
             data = series.abundance, family = poisson)
summary(glm04)
#(1|Area)

glm05 <- glmer(species ~ year + (1|Aufnahme_Nr), 
             data = series.abundance, family = poisson)
summary(glm05)


glm06 <- glmer(species ~ year + (1|Aufnahme_Nr) + (1|year), 
               data = series.abundance, family = poisson)
summary(glm06)

glm07 <- glmer(species ~ year + (1|Transekt) + (1|year), 
               data = series.abundance, family = poisson)
summary(glm07)

glm07.2 <- lmer(species ~ year + (1|Transekt) + (1|year), 
               data = series.abundance)
summary(glm07.2)

glm06_rare <- glmer(prop.rare ~ year + (1|Aufnahme_Nr) + (1|year), 
                    data = series.abundance, family = poisson)
summary(glm06_rare)

hist(series.abundance$species)
library(stargazer)
table_time_lmer <- stargazer(glm06, type = "html",
                        digits = 3,
                        star.cutoffs = c(0.05, 0.01, 0.001),
                        digit.separator = "",
                        title = "Summary Results of a Frequentist Hierarchical Model of Plant Species Richness on the Ruckowitzschachten from 2014 to 2020")
###plot glm results----

library(ggeffects)
# Extract the prediction data frame

glm07 <- glmer(species ~ year + (1|Transekt), 
               data = series.abundance, family = poisson)
pred.mm <- ggpredict(glm07, terms = c("year"))  # this gives overall predictions for the model
plot(pred.mm)
summary(pred.mm) # add ci.lvl = 0.95?


# Plot the predictions 
(mixed_effects <- ggplot() +
    geom_line(data = pred.mm, aes(x = x, y = predicted),
              size = 1) +
    geom_ribbon(data = pred.mm, aes(ymin = conf.low, ymax = conf.high, x = x), alpha = 0.1) +
    geom_point(data = series.abundance, aes(x = year, y = species),
               alpha = 0.1, size = 2) +
    #annotate("text", x = 13, y = 0.6, label = "Slope = 0.025, Std. error = 0.003") +  
    #scale_y_continuous(limits = c (-1, 1)) +
    theme_classic() +
    labs(x = "\nYear", y = "Species richness\n"))


ggsave("outputs/Mixed_effects_model.png", plot = mixed_effects, device = png)


library(viridis)
### facet plot w random slope
mixed.ranslope <- glmer(species ~ year + (1+year|Transekt), data = series.abundance, family = poisson)

summary(mixed.ranslope)

pred.mm <- ggpredict(glm07, terms = c("year"))
(mm_plot <- ggplot(series.abundance,  aes(x = year, y = species, colour = Aufnahme_Nr)) +
    facet_wrap(~Transekt, nrow=3) +   # a panel for each transekt
    geom_point(alpha = 0.5) +
    labs(x = "Year", y = "Species Richness") + 
    theme_classic() +
    geom_line(data = cbind(series.abundance, pred = predict(mixed.ranslope)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines")) + # adding space between panels
    scale_colour_viridis(discrete = TRUE, option="viridis") #choose different colour palette
)

ggsave("outputs/Mixed_effects_facet.png", plot = mm_plot, height = 40, units = "cm" ,width = 25, device = png)

ggpredict(glm07, terms = c("year"), type = "re") %>% 
  plot() +
  labs(x = "Year", y = "Species Richness") + 
  theme_minimal()+
  theme(legend.position = "none")


#ale code

glm07 <- glmer(species ~ year + (1|Transekt) + (1|year), 
               data = series.abundance, family = poisson)
#mixed.lmer3 <- lmer(scalepop ~ year_scaled + (1|Location) + (1|year), data = loggerhead_t)

#pred.lh3 <- ggpredict(mixed.lmer3, terms = c("year_scaled"))  #gives overall predictions for the model
pred.mm <- ggpredict(glm07, terms = c("year", "Transekt"))
predictions1 <- ggpredict(glm07, terms = c("year"))
#Model 3 prediction plot
(lh_predictions <- ggplot(pred.mm) +
    geom_point(data = series.abundance,                     
               aes(x = year, y = species, colour = Aufnahme_Nr)) + #adding the raw datapoints
    geom_line(aes(x = x, y = predicted, fill = "black", )) + #slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error),
                fill = "lightgrey", alpha = 0.4) +  #confidence interval
    #facet_wrap(~Transekt, nrow=3) +   # a panel for each transekt
    theme_classic() +
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines")) +
    #annotate("text", x = -2, y = 0.0, colour = "black",
    #         label = paste("7% yearly population increase"),
    #         size = 3.5, fontface = "bold") +  #Annotation
   # geom_curve(aes(x = -2, y = -0.035, xend = -1.5, yend = -0.2),
    #           arrow = arrow(length = unit(0.07, "inch")), size = 0.5,
     #          color = "black", curvature = 0.3) +
    labs(x = "Year", y = "species richness")+ #,
      #   caption = "\nFigure 2: Model predictions for the global population of Loggerhead turtle (Caretta caretta)
      #          with data points color coded by Location. Predicted increase of ~ 7%.") +
    scale_color_viridis_d())# +
  #  scale_fill_viridis_d())


#erics stuff 
Adelie_model1 <- glmer(species ~ year + (1|year) + (1|Location),
                      family = "poisson",  # Specifying distribution type
                      data = series.abundance)
predictions1 <- ggpredict(glm07, terms = c("year"))

(glm_results <- ggplot() +
    geom_line(data = predictions1, aes(x, predicted),
              size = 2) +
    geom_ribbon(data = predictions1, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
    geom_point(data = series.abundance, aes(x = year, y = species, colour = Transekt),
               alpha = 0.5, size = 2) +
    #annotate("text", x = -1.5, y = 20000, label = "Slope = 0.326 +/- 0.076") +  
    #scale_y_continuous(limits = c (0, 60000)) +
    scale_color_viridis_d(option = "inferno") + 
    #add_phylopic(penguin_logo, alpha = 1, x = -2, y = 50000, ysize = 10000) + 
    theme_classic() +
    labs(x = "\nYears (scaled)", y = "Penguin abundance\n"))#, 
        # title = "Adélie Penguin population size expected to increase", 
         #caption = "Fig.1 As model predicts, Adélie penguin population size is 
	       #expected to increase by a rate of 0.326 species (+/- 0.076) per year,
	       #slope = +0.326 +/- 0.076, sample size = 552, groups = Year = 44, Location = 5."))

#### isla's betadiv code ----
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
  left_join(meta.long,by = c("AreaName"="Aufnahme_Nr")) %>% #doesnt work 
  select(-c(year))

#abs(sub_bio_presence_matrix)

hist(bray_results$Bbray)


glm08 <- lm(Bbray ~ Transekt , 
               data = bray_results)
summary(glm08)
glm09 <- lmer(Bbray ~ Transekt + (1|Transekt), #lm better
            data = bray_results)
summary(glm09)


hist(bray_results$Bbray)
