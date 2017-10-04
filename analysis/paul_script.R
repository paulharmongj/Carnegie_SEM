#read in data
setwd("C:/Users/paulh/Downloads")
cc2015 <- read.csv("CC2015data.csv",header = TRUE)

########2015################
cc2015.full <- read.csv("CC2015data.csv", header = TRUE, as.is = TRUE)
#updated file
#cc2015.full <- read.csv("Updated2015.csv", header = TRUE)

cc2015 <- cc2015.full[(cc2015.full$BASIC2015>14&cc2015.full$BASIC2015<18),]
cc2015$BASIC2015 <- factor(cc2015$BASIC2015)


#function for ranking the data
minrank <- function(x){rank(x, ties.method = "min")}

#dataset that we want to use
cc2015Ps<-
  na.omit(cc2015[,c("NAME","BASIC2010","BASIC2015","FACNUM","HUM_RSD","OTHER_RSD","SOCSC_RSD","STEM_RSD","PDNFRSTAFF","S.ER.D","NONS.ER.D")])

#calculate the ranked data
cc2015.r <- data.frame(cc2015Ps[,1:3],sapply(cc2015Ps[,-c(1:3)],minrank)) 

cc2015percap <- cc2015Ps[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/cc2015Ps$FACNUM
cc2015percap.r<-data.frame(sapply(cc2015percap,minrank))


###RUN THE sEM
#install.packages('lavaan')
library(lavaan); library(blavaan)

#to fit this model I'll need to have per-capita and full counts in the same dataset

carn_dat <- data.frame(cc2015Ps,cc2015percap)

model.carnegie <- '
#measurement model
ag_latent =~ S.ER.D + NONS.ER.D + STEM_RSD + SOCSC_RSD + OTHER_RSD + HUM_RSD
percap_latent =~ PDNFRSTAFF.1 + S.ER.D.1 + NONS.ER.D.1

# regressions
Overall = ~ ag_latent + percap_latent
'

#fit the SEM
library(semPlot)

sem.1 <- lavaan::sem(model.carnegie, data=carn_dat, std.lv=TRUE, se = "boot", test = "Bollen.Stine", orthogonal = FALSE)
semPlot::semPaths(sem.1)
#



a <- varTable(sem.1)
barplot(a$var, names.arg = a$name, las = '2')

#what if we scaled the raw data prior to fitting the sem?
carn_scale <- scale(carn_dat[,4:ncol(carn_dat)])

#re-fit the SEM
sem.scale <- sem(model.carnegie, data = carn_scale, std.lv = TRUE)
summary(sem.scale)


#could we do this based on the ranks?
names(cc2015percap.r) <- c("STAFFPC","SERDPC","NONSERDPC")
library(tibble)
ranked_dat <- as_tibble(cbind(cc2015.r, cc2015percap.r))

model.carnegie.ranked <- '
#measurement model
ag_latent =~ S.ER.D + NONS.ER.D + STEM_RSD + SOCSC_RSD + OTHER_RSD + HUM_RSD + PDNFRSTAFF
percap_latent =~ STAFFPC + SERDPC + NONSERDPC

# regressions
OVERALL =~ ag_latent + percap_latent

#Correlations
S.ER.D ~~ STEM_RSD 
S.ER.D ~~ PDNFRSTAFF
NONS.ER.D ~~ HUM_RSD
NONS.ER.D ~~ SOCSC_RSD
SOCSC_RSD ~~ HUM_RSD
STAFFPC ~~ SERDPC

#with per-capita stuff
S.ER.D ~~ SERDPC
NONS.ER.D ~~ NONSERDPC
PDNFRSTAFF ~~ STAFFPC
'


#sem.scale.rank <- sem(model.carnegie.ranked,data = ranked_dat, se = "bootstrap",std.lv = TRUE)
summary(sem.scale.rank)
#has trouble with standard errors normally, but produces results with bootstrapped SE's

#trying huber-white

#possibly mess with other SE measurments 

sem.scale.hw <- sem(model.carnegie.ranked, data = ranked_dat, estimators = "ML", se = "robust.huber.white", std.lv = TRUE)
summary(sem.scale.hw, fit.measures = TRUE)

inspect(sem.scale.hw, 'theta')

#still running into identifiability issues
#install.packages('semPlot')
library(semPlot)
semPlot::semPaths(sem.scale.hw,col = c('orange'))
title("SEM Path Diagram")




#let's try a Bayesian SEM
bSEM <- blavaan::bsem(model.carnegie.ranked, data = ranked_dat, std.lv = TRUE, n.chains = 4, burnin = 100, sample = 1000)


#lavaan.survey

svy.ob <- svydesign(fpc = ~1, data = ranked_dat, ids = rep(1,length(ranked_dat[,1])))
lavaan.survey(model.carnegie.ranked, survey.design = svy.ob)








