

#read in data
#setwd("~/Carnegie-SEM/data")
cc2015 <- read.csv("data/CC2015data.csv",header = TRUE)

########2015################
cc2015.full <- read.csv("data/CC2015data.csv", header = TRUE, as.is = TRUE)
#updated file
#cc2015.full <- read.csv("Updated2015.csv", header = TRUE)

cc2015 <- cc2015.full[(cc2015.full$BASIC2015>14&cc2015.full$BASIC2015<18),]
cc2015$BASIC2015 <- factor(cc2015$BASIC2015)


#function for ranking the data
minrank <- function(x){rank(x, ties.method = "min")}

#dataset that we want to use
cc2015Ps<-
  na.omit(cc2015[,c("NAME","BASIC2010","BASIC2015","FACNUM","HUM_RSD",
                    "OTHER_RSD","SOCSC_RSD","STEM_RSD","PDNFRSTAFF","S.ER.D","NONS.ER.D")])

#calculate the ranked data
cc2015.r <- data.frame(cc2015Ps[,1:3],sapply(cc2015Ps[,-c(1:3)],minrank)) 

cc2015percap <- cc2015Ps[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/cc2015Ps$FACNUM


colnames(cc2015percap) <- c("PDNRSTAFF_PC", "S.ER.D_PC", "NONS.ER.D_PC")
#for Corrplot, nothing else


cc2015percap.r<-data.frame(sapply(cc2015percap,minrank))
cc2015_r <- cbind(cc2015.r, cc2015percap.r)
#RUN THIS FOR CORRPLOT: (commented out otherwise)
names(cc2015_r) <- c("Name","2010 Basic","2015 Basic","Faculty Size", "Hum PhD",
                     "Other PhD","Soc. Sci PhD", "STEM PhD","Res. Staff", "STEM Exp", 
                     "NS Exp", "Res. Staff PC", "STEM Exp PC  ", "NS Exp PC")

cc2015_matrix2 <- as.matrix(cc2015_r[-c(1:3)])
corrmatrix <- Hmisc::rcorr(cc2015_matrix2)

par(mar = c(1,.8,.8,2))
corrplot::corrplot(corrmatrix$r, order="hclust", type = "upper", method = 'number')
text(-4,3, 'Correlation Plot', font = 2)
text(-4,2,'of Manifest Variables', font = 2)
rect(-7,1.3,-1,3.5)

model4 <- '
HUMANITIES=~HUM_RSD+OTHER_RSD+SOCSC_RSD+NONS.ER.D+FACNUM
STEM=~STEM_RSD+PDNFRSTAFF+S.ER.D+FACNUM
Aggregate=~HUMANITIES+STEM'

model4 <- '
non-STEM = ~ Hum PhD + Other PhD + Soc. Sci PhD + 
STEM
'


lavaan_sem_new <- lavaan::sem(model4, data=cc2015_r, std.lv=TRUE,
                              orthogonal=FALSE, se="robust.huber.white")
lavaan::summary(lavaan_sem_new, standardized=TRUE, fit.measures=TRUE)
CCScores <- as.data.frame(lavaan::predict(lavaan_sem_new))
rownames(CCScores) <- cc2015Ps$NAME

#checks the standardized factor loadings
inspect(lavaan_sem_new, what = 'std')$lambda

#path diagram

source('https://raw.githubusercontent.com/brandmaier/onyxR/master/tools/install.R')
library(onyxR)
onyx(lavaan_sem_new)

library(beanplot)
beanplot(CCScores$Aggregate,log="",col="yellow",method="jitter")

library(mclust)
mcres<-Mclust(CCScores$Aggregate)
summary(mcres)
par(mfrow=c(2,2))
plot(mcres,what="BIC")
plot(mcres,what="classification")
plot(mcres,what="uncertainty")
plot(mcres,what="density")

Classifications <- as.data.frame(mcres[14:15])
rownames(Classifications) <- cc2015Ps$NAME

table(mcres$classification, cc2015Ps$BASIC2015) ## pretty good!!

#Or more directly:
dens<-densityMclust(CCScores$Aggregate)
summary(dens)
par(mfrow=c(1,2))
plot(dens,what="diagnostic")

#Or clustering the STEM and HUMANITIES scores (bivariate version)
mcres2<-Mclust(CCScores[,1:2])
par(mfrow=c(2,2))
plot(mcres2,what="BIC")
plot(mcres2,what="classification")
plot(mcres2,what="uncertainty")
plot(mcres2,what="density")

