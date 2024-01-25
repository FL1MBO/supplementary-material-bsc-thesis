# Sanne Bekkering - 7070985
# Bachelor's thesis Global Sustainability Science 
# Utrecht University

#**********************************************# 
####            PART 1 - Setup             ####
#**********************************************# 
# load all packages and install those missing
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readxl")) install.packages("readxl")
if (!require("ade4")) install.packages("ade4")
if (!require("factoextra")) install.packages("factoextra")
if (!require("randomForest")) install.packages("randomForest")
if (!require("broom")) install.packages("broom")

set.seed(23)

#**********************************************# 
####           PART 2 - Load data           ####
#**********************************************# 
# RQ 1 - Plant leaf traits 
data.plant <- read_excel("Thesis data.xlsx", sheet = "Plant")

# RQ 2 - PCAs of management practices and soil characteristics 
data.man <- read_excel("Thesis data.xlsx", sheet = "Management") # Managament practices
data.soil <- read_excel("Thesis data.xlsx", sheet = "Soil") # Soil characteristics 

# RQ 2 - Random forest management/soil variables per plant trait 
data.sla <- read_excel("Thesis data.xlsx", sheet = "SLA")
data.la <- read_excel("Thesis data.xlsx", sheet = "LA")
data.ldmc <- read_excel("Thesis data.xlsx", sheet = "LDMC")
data.ftp <- read_excel("Thesis data.xlsx", sheet = "FtP")
data.lt <- read_excel("Thesis data.xlsx", sheet = "LT")
data.chloro <- read_excel("Thesis data.xlsx", sheet = "Chloro")
data.n <- read_excel("Thesis data.xlsx", sheet = "N")
data.p <- read_excel("Thesis data.xlsx", sheet = "P")
data.k <- read_excel("Thesis data.xlsx", sheet = "K")
data.ca <- read_excel("Thesis data.xlsx", sheet = "Ca")

# RQ 3 - Random forest coffee leaf traits and production 
data.prod <- read_excel("Thesis data.xlsx", sheet = "Production")
str(data.prod)


#***********************************************************# 
#### PART 3 - RQ 1 & 2-  PCA plant, management, and soil ####
#***********************************************************# 
# Ade4 method
str(data.plant)

caract.pca.plant=dudi.pca(data.plant[,1:10], center=T, scale=T,scannf=T, nf=5)
## Make sure to enter the number 2 after running this line in the console. 
dudi.pca(df = data.plant[, 1:10], center = T, scale = T, scannf = FALSE, nf = 2)
(caract.pca.plant$eig)
cumsum(caract.pca.plant$eig) / sum(caract.pca.plant$eig)
caract.pca.plant$co
s.corcircle(caract.pca.plant$co, xax=1, yax=2 )
scatter(caract.pca.plant)
summary(caract.pca.plant)

# Factoextra method 
pca.plant<-fviz_pca_biplot(caract.pca.plant, repel = TRUE, 
                col.var = "black", # Variables colour
                title = "PCA - Leaf traits",
                xlab="PC 1 (23.8% of explained var.)", # % explained var. = from summary caract.pca
                ylab="PC 2 (22.5% of explained var.)", habillage=data.plant$Farm, 
                addEllipses=TRUE, ellipse.level=0.5, palette = c("#01a08a", "#f2ad00", "#ff2500"))+
  theme(text=element_text(size=14), # Title 
        axis.title=element_text(size=12), # axis titles and size 
        axis.text=element_text(size=12))
pca.plant 

# Management PCA 
caract.pca.man=dudi.pca(data.man, center=T, scale=T,scannf=T, nf=5)
## Make sure to enter the number 2 after running this line in the console. 
dudi.pca(df = data.man, center = T, scale = T, scannf = FALSE, nf = 2)
(caract.pca.man$eig)
cumsum(caract.pca.man$eig) / sum(caract.pca.man$eig)
caract.pca.man$co
s.corcircle(caract.pca.man$co, xax=1, yax=2 )
scatter(caract.pca.man)
summary(caract.pca.man)


pca.man<-fviz_pca_biplot(caract.pca.man, repel = TRUE, 
                           col.var = "black", # Variables colour
                           title = "PCA - Management practices",
                           xlab="PC 1 (57.9% of explained var.)", # % explained var. = from summary caract.pca
                           ylab="PC 2 (19.3% of explained var.)", habillage=data.plant$Farm,
                           addEllipses=TRUE, ellipse.level=0.5, palette = c("#01a08a", "#f2ad00", "#ff2500"))+
  theme(text=element_text(size=14), # Title 
        axis.title=element_text(size=12), # axis titles and size 
        axis.text=element_text(size=12))
pca.man

# Soil PCA
caract.pca.soil=dudi.pca(data.soil, center=T, scale=T,scannf=T, nf=5)
## Make sure to enter the number 2 after running this line in the console. 
dudi.pca(df = data.soil, center = T, scale = T, scannf = FALSE, nf = 2)
(caract.pca.soil$eig)
cumsum(caract.pca.soil$eig) / sum(caract.pca.soil$eig)
caract.pca.soil$co
s.corcircle(caract.pca.soil$co, xax=1, yax=2 )
scatter(caract.pca.soil)
summary(caract.pca.soil)


pca.soil<-fviz_pca_biplot(caract.pca.soil, repel = TRUE, 
                          col.var = "black", # Variables colour
                          title = "PCA - Soil characteristics",
                          xlab="PC 1 (35.0% of explained var.)", # % explained var. = from summary caract.pca
                          ylab="PC 2 (24.4% of explained var.)", habillage=data.plant$Farm, 
                          addEllipses=TRUE, ellipse.level=0.5, palette = c("#01a08a", "#f2ad00", "#ff2500"))+ 
  theme(text=element_text(size=14), # Title 
        axis.title=element_text(size=12), # axis titles and size 
        axis.text=element_text(size=12))
pca.soil

#**********************************************************# 
####  PART 4 - RQ 2 - Random forest management and soil ####
#**********************************************************# 
# LDMC 
rf.LDMC <- randomForest((data.ldmc$LDMC) ~ ., proximity = T, importance = T, keep.forest = T, ntree = 1000, data = data.ldmc, flagReg = 0)
rf.LDMC
rf.LDMC$importance
plot(rf.LDMC)
varImpPlot(rf.LDMC, type = 1, main = "Leaf dry matter content") # Type = 1 for the %incMSE, not IncNodePurity

# LA 
rf.LA <- randomForest((data.la$LA) ~ ., proximity = T, importance = T, keep.forest = T, ntree = 1000, data = data.la, flagReg = 0)
rf.LA
rf.LA$importance
plot(rf.LA)
varImpPlot(rf.LA, type = 1, main = "Leaf area")

# SLA 
rf.SLA <- randomForest((data.sla$SLA) ~ ., proximity = T, importance = T, keep.forest = T, ntree = 1000, data = data.sla, flagReg = 0)
rf.SLA
rf.SLA$importance
plot(rf.SLA)
varImpPlot(rf.SLA, type = 1, main = "Specific leaf area")

# FtP 
rf.FtP <- randomForest((data.ftp$FtP) ~ ., proximity = T, importance = T, keep.forest = T, ntree = 1000, data = data.ftp, flagReg = 0)
rf.FtP
rf.FtP$importance
plot(rf.FtP)
varImpPlot(rf.FtP, type = 1, main = "Force to punch")

# LT 
rf.LT <- randomForest((data.lt$LT) ~ ., proximity = T, importance = T, keep.forest = T, ntree = 1000, data = data.lt, flagReg = 0)
rf.LT
rf.LT$importance
plot(rf.LT)
varImpPlot(rf.LT, type = 1, main = "Leaf thickness")
# Chloro
rf.chloro <- randomForest((data.ftp$FtP) ~ ., proximity = T, importance = T, keep.forest = T, ntree = 1000, data = data.ftp, flagReg = 0)
rf.chloro
rf.chloro$importance
plot(rf.chloro)
varImpPlot(rf.chloro, type = 1, main = "Chlorophyll")
# Leaf N 
rf.N <- randomForest((data.n$Leaf_N) ~ ., proximity = T, importance = T, keep.forest = T, ntree = 1000, data = data.n, flagReg = 0)
rf.N
rf.N$importance
plot(rf.N)
varImpPlot(rf.N, type = 1, main = "Leaf nitrogen")
# Leaf P 
rf.P <- randomForest((data.p$Leaf_P) ~ ., proximity = T, importance = T, keep.forest = T, ntree = 1000, data = data.p, flagReg = 0)
rf.P
rf.P$importance
plot(rf.P)
varImpPlot(rf.P, type = 1, main = "Leaf phosphorous")
# Leaf K 
rf.K <- randomForest((data.k$Leaf_K) ~ ., proximity = T, importance = T, keep.forest = T, ntree = 1000, data = data.k, flagReg = 0)
rf.K
rf.K$importance
plot(rf.K)
varImpPlot(rf.K, type = 1, main = "Leaf potassium")
# Leaf Ca 
rf.Ca <- randomForest((data.n$Leaf_N) ~ ., proximity = T, importance = T, keep.forest = T, ntree = 1000, data = data.ca, flagReg = 0)
rf.Ca
rf.Ca$importance
plot(rf.Ca)
varImpPlot(rf.Ca, type = 1, main = "Leaf calcium")

#*************************************************************# 
####  PART 5 - RQ 3 - Random forest traits and production ####
#*************************************************************#
data.prod1 <- data.prod[,-1] # exclude variable "farm" since this is only a classification of the types of farms for later use 
rf.prod <- randomForest(prod~ .,
                       proximity = T,
                       importance = T,
                       keep.forest = T,
                       ntree = 1000,
                       data = data.prod1,
                       flagReg = 0)
rf.prod
rf.prod$importance
plot(rf.prod)
varImpPlot(rf.prod, type = 1, main = "Coffee productivity")

#******************************************************************# 
####  PART 6 - RQ 3 -  Linear regression traits and production ####
#*******************************************************************# 
LDMC.regression <- lm(data.prod$prod ~ data.prod$LDMC, data=data.prod)
summary(LDMC.regression)

## Nitrogen
N.regression <- lm(data.prod$prod ~ data.prod$Leaf_N, data=data.prod)
summary(N.regression)

## Calcium
Ca.regression <- lm(data.prod$prod ~ data.prod$Leaf_Ca, data=data.prod)
summary(Ca.regression)

## Potassium 
K.regression <- lm(data.prod$prod ~ data.prod$Leaf_K, data=data.prod)
summary(K.regression)

## Leaf phosphorous 
P.regression <- lm(data.prod$prod ~ data.prod$Leaf_P, data=data.prod)
summary(P.regression)

## Specific leaf area
SLA.regression <- lm(data.prod$prod ~ data.prod$SLA, data=data.prod)
summary(SLA.regression)

## Chlorophyll
Chloro.regression <- lm(data.prod$prod ~ data.prod$Chloro, data=data.prod)
summary(Chloro.regression)

## Leaf thickness 
LT.regression <- lm(data.prod$prod ~ data.prod$LT, data=data.prod)
summary(LT.regression)

## Force to punch 
FTP.regression <- lm(data.prod$prod ~ data.prod$FtP, data=data.prod)
summary(FTP.regression)

## Leaf area 
LA.regression <- lm(data.prod$prod ~ data.prod$LA, data=data.prod)
summary(LA.regression)

df <- data.prod[, c("LA", "prod", "Farm")]
df$Farm <- as.factor(df$Farm)
head(df)

model <- lm(prod~LA, data = df)
summary(model)
ggplot(df,aes(x=LA, y=prod)) + geom_point(aes(shape = Farm, color = Farm), size = 2) + 
  scale_shape_manual(values=c(19, 17, 15))+  
  scale_color_manual(values=c('#01a08a','#f2ad00', '#ff2500'))+ 
  geom_smooth(method='lm', se = TRUE, colour = "black")+ theme_minimal()+
  labs(x='Leaf area [cm2]', y='Coffee productivity [kg/ha/year]', title='Linear regression leaf area and coffee productivity') +
  theme(plot.title = element_text(hjust=1.4, size=16))

### Linear regression check assumptions 
model <- lm(data.prod$LA ~ data.prod$prod)
model
plot(model)

#**************************************************************# 
####  PART 7 - RQ 3 -  Coffee productivity among farm types ####
#**************************************************************# 
## Coffee productivity across all farm types 
farm <- as.factor(data.prod$Farm)

barplot(data.prod$prod, border = c("#01a08a", "#f2ad00", "#ff2500"), col = c("#E5F5F3", "#FEF7E5", "#FFE9E5"),
        names.arg=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), 
        xlab = "Farm observation number",
        ylab = "Coffee productivity [kg/ha/year]", 
        main = "Coffee productivity between farm types")
legend("topright", legend = levels(farm), col = c("#01a08a", "#f2ad00", "#ff2500"), pch = 16, cex = 0.9, bty = "y", inset = c(0.01,0))

