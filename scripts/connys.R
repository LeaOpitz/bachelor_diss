library(vegan)
library(car)
library(MASS)
library(DHARMa)

species = read.csv2("data/NMDS_species.csv", header = T, row.names = 1, sep = ";")
str(species)

#Enviromental parameters
Enviroment = read.csv2("data/NMDS_envir.csv", header = T, row.names = 1, sep = ";")
str(Enviroment)     # Da fehlt die Kategorisierung
Enviroment2 = read.csv2("data/final_data.csv", header = T, row.names = 1, sep = ";")
str(Enviroment2)  

sp2 <- rownames_to_column(species, var = "plot")
envir2 <- rownames_to_column(Enviroment2, var = "plot")
Environment3 <- left_join(envir2, sp2, by = "plot")
Environment3 <- column_to_rownames(Environment3, var = "plot")

#NMDS
ord = metaMDS(species)   #Stress value=0.1682497
env <- envfit(ord ~ species + prop_rare + result_L + result_F + result_R + result_N + 
                result_mow + result_graze + result_forage, data=Enviroment2, perm=999)
env      #We rte anschauen und WEErte mit geringer Korrelation rauswerfen
#Korrelation of axis with species
envSpe <- envfit(ord ~ Agrostis.capillaris + Agrostis.canina + Anthoxanthum.odoratum +
                   Calamagrostis.villosa + Carex.brizoides. + Carex.echinata + 
                   Carex.flava.agg. + Carex.leporina + Carex.nigra, data=species, perm=999)
envSpe      #We rte anschauen und WEErte mit geringer Korrelation rauswerfen

env2 <- envfit(ord, Enviroment3, permutations = 999)
env2

env3 <- envfit(ord ~ species + result_F  + result_N +
                 result_mow + result_forage +  Agrostis.capillaris + Carex.brizoides. +
                 Carex.pilulifera + Deschampsia.cespitosa +
                 Festuca.rubra.agg. + Juncus.filiformis + Achillea.millefolium +
                 Alchemilla.spec. + Campanula.rotundifolia+ Galium.saxatile+ 
                 Hypericum.maculatum+ Lychnis.flos.cuculi+ Maianthemum.bifolium+ Prunella.vulgaris+ 
                 Trifolium.repens+ Valeriana.dioica+
                 Veronica.chamaedrys+ Veronica.officinalis+ Epilobium.angustifolium+
                 Epilobium.palustre+ Geranium.sylvaticum+ Stellaria.alsine +
                 Viola.reichenbachiana+ Leucanthemum.vulgare, data=Environment3, perm=999)

# Calamagrostis.villosa + Acer.pseudoplatanus + Carex.pallescens + Anthriscus.sylvestris+
  # Cardamine.amara+ Epilobium.palustre+ Geranium.sylvaticum+ Lysimachia.nemorum+
  #Rumex.acetosella+ Stellaria.alsine + Epilobium.obscurum + Sphagnum.sec.. 
env3

env12 <- envfit(ord ~ result_mow + result_forage +  Agrostis.capillaris +
                  Achillea.millefolium + Galium.saxatile+ Hypericum.maculatum, 
                data=Environment3, perm=999)

env15 <- envfit(ord ~  result_forage +  Agrostis.capillaris + Achillea.millefolium, 
                data=Environment3, perm=999)

write.csv2(Environment3, "C:/Users/Lea/Documents/R/git/bachelor_diss/data/NMDS.csv", row.names = TRUE)


plot(ord, display = "sites", type = "p")

plot(env15, col = "black", labels = FALSE) 
colfactor <- factor(Environment3$Vegetation_type)    #Braucht es glaube ich gar nicht
with(Enviroment3, points(ord, disp = "sites", pch = as.numeric(Vegetation_type)))
with(Enviroment3, legend("topleft", levels(Vegetation_type), pch = 1:4,
                         title = "Vegetation type"))

points(ord, display = "sites", cex = 1, pch = 16, col = c("#228B22","#7FFF00","#EEB422")[colfactor])
ordiellipse(ord, Environment3$Management, kind = "ehull",  col = c("#228B22","#7FFF00","#EEB422"), lwd = 3)

points(ord, display = "sites", cex = 1, pch = 16, col = c("#F1BB7B","#FD6467","#5B1A18", "#D67236" )[colfactor])
ordiellipse(ord, Environment3$Vegetation_type, kind = "ehull",  col = c("#F1BB7B","#FD6467","#5B1A18", "#D67236" ), lwd = 3)


plot(ord, display = "sites", type = "p")
plot(env, col = "black")
colfactor <- factor(Enviroment2$Management)
with(Enviroment2, points(ord, disp = "sites", pch = as.numeric(Management)))
with(Enviroment2, legend("topleft", levels(Management), pch = 1:4,
                         title = "Management"))

points(ord, display = "sites", cex = 1, pch = 16, col = c("#228B22","#7FFF00","#EEB422")[colfactor])
ordiellipse(ord, Enviroment3$Management, kind = "ehull",  col = c("#228B22","#7FFF00","#EEB422"), lwd = 3)


indic_nmds <- metaMDS(species, distance = "bray", autotransform = FALSE)
plot(indic_nmds, display = "sites", type = "p")
points(indic_nmds, display = "sites", cex = 1, pch = 16, col = c("#228B22","#7FFF00","#EEB422")[colfactor])
ordiellipse(indic_nmds, final_data$Management, kind = "ehull",  col = c("#228B22","#7FFF00","#EEB422"), lwd = 3)

#GLmer for prop rare species
Enviroment2 = read.csv2("C:/Users/co tu/Downloads/final_data.csv", header = T, row.names = 1, sep = ";")
str(Enviroment2)  
data=Enviroment2
#Verteilung Arten.Vasc formal passt nicht
hist(data$prop_rare)
plot(density(data$prop_rare))
shapiro.test(data$prop_rare)     #Keine Normalverteilung gegeben
#Homogeneity of variances
leveneTest(data$prop_rare, data$Management, center=mean) #Homogenität gerade so gegeben
boxplot(data$prop_rare ~ data$Management)
#Verteilung von Prop rare
#Wahrscheinlichkeitsverteilung = propability distribution
#REquired packages: car & MASS
data$prop_rare.t <- data$prop_rare + 1
qqp(data$prop_rare.t, "norm")            
# lnorm means lognormal -> okay
qqp(data$prop_rare.t, "lnorm")           
#neg binominal   Funktioniert irgendwie nicht, sollte aber gar nicht so schlecht sein, weil du viele Nullen hast
nbinom <- fitdistr(data$prop_rare.t, "negative binomial")
qqp(data$prop_rare.t, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])  
#Poisson
poisson <- fitdistr(data$prop_rare.t, "Poisson")
qqp(data$prop_rare.t, "pois", lambda = poisson$estimate) # Poisson distribution passt nicht
#gamma 
gamma <- fitdistr(data$prop_rare.t, "gamma")
qqp(data$prop_rare.t, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])   
#Theoretisch könnte an noch Autorkorrelationen testen (wenn correlation von zwei VAriablen über 0.8)
#Aber lass weg, kostet Zeit
#Folgerung: normale Piosson distribution ist nicht das richtige, entweder quasipoisson oder neg.binom
#Probieren wir einfach mal neg.binom
#For glmer.nb we need lme4 & for residual check DHARMa
r0 <- glmer.nb(prop_rare ~    Management + scale(result_L) + 
                 scale(result_F) +scale(result_R) +scale(result_mow)  +
                 scale(result_forage) + 
                 (1|Vegetation_type)  ,  data=data)
summary(r0) #Des rechnet er gar nicht???
r0 <- glmer(prop_rare ~    Management + (1|Vegetation_type) , family = "poisson" ,  data=data)
summary(r0)

glm7.4 <- glmer.nb(prop_rare.t ~ Management + (1|Vegetation_type) + result_F + result_R, #fixed veg and idic
                   data = data)
predict4 <- ggpredict(glm7.4, terms = c("Management"), type = "fe") 

#Sorry you have to find a good model on your own
#How good did  my model fit?
#Overdispersion mean<variance
mean(data$prop_rare)
var(data$prop_rare)
#Model validation according to Zuur et al. 2009
op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))         #4 Graphs wih a bit of space around
plot(r0, add.smooth = FALSE, which = 1) 
E <- resid(r0) 
hist(E, xlab = "Residuals", main = "") 
plot(data$prop_rare, E, xlab = "LNumber of species", ylab = "Residuals") 
plot(data$Alter, E, xlab = "Alter", ylab = "Residuals") 
par(op)
#Model validation with DHARMA
library(DHARMa)
summary(r0)
simulationOutput <- simulateResiduals(fittedModel = r0, n = 250)
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
#Das wäre noch ob die Variaben so benutzt werden dürfen oder ob eine TRansformation nötig wäre
#Kannst gerne weglassen, aber der Vollsständigkeit wegen
plotResiduals(data$Gras.., simulationOutput$scaledResiduals)
plotResiduals(data$Fest.Brom, simulationOutput$scaledResiduals)
plotResiduals(data$Alter, simulationOutput$scaledResiduals)
plotResiduals(data$P..mg.P.kg.Erde., simulationOutput$scaledResiduals)
plotResiduals(data$EIV.R, simulationOutput$scaledResiduals)
