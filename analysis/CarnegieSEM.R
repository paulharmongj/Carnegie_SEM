#read in data
setwd("~/Carnegie-SEM/data")
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
colnames(cc2015percap) <- c("PDNRSTAFF_PC", "S.ER.D_PC", "NONS.ER.D_PC")
cc2015percap.r<-data.frame(sapply(cc2015percap,minrank))

#sem using raw data
cc2015_new <- cbind(cc2015Ps, cc2015percap)

model1 <- '
  Aggregate=~HUM_RSD+OTHER_RSD+SOCSC_RSD+STEM_RSD+PDNFRSTAFF+S.ER.D+NONS.ER.D
  PerCap=~PDNRSTAFF_PC+S.ER.D_PC+NONS.ER.D_PC

  Overall=~Aggregate+PerCap
'

lavaan_sem_b <- lavaan::sem(model1, data=cc2015_new, std.lv=TRUE,se="boot",test="Bollen.Stine",bootstrap=1000, orthogonal=FALSE)
lavaan::summary(lavaan_sem_b, fit.measures=TRUE)
lavaan::parameterEstimates(lavaan_sem_b,boot.ci.type="norm",standardized=T)

CCScores <- lavaan::predict(lavaan_sem_b)
car::Boxplot(CCScores[,3])
plot(density(CCScores[,3])) #very right-skewed
abline(v=-0.59, col="red")
abline(v=-0.14, col="red")

#sem using ranked data
cc2015_r <- cbind(cc2015.r, cc2015percap.r)


lavaan_sem_r <- lavaan::sem(model1, data=cc2015_r, std.lv=TRUE, orthogonal=FALSE, se="boot", test="Bollen.Stine", bootstrap=1000)
lavaan::summary(lavaan_sem_r, fit.measures=TRUE)

CCScores_r <- lavaan::predict(lavaan_sem_r)
plot(density(CCScores_r[,3])) 

#used a PCA because it made more sense in my head
PCScores <- prcomp(CCScores_r[,1:2])
summary(prcomp(CCScores_r[,1:2]))

PC <- as.data.frame(PCScores$x)
plot(PC$PC1, PC$PC2, ylim=c(-20,20))
boxplot(PC$PC1, horizontal = TRUE)
plot(density(PC$PC1))
abline(v=-6.76, col="red")
abline(v=6.41, col="red")


cc2015_matrix2 <- as.matrix(cc2015_r[-c(1:3)])
Hmisc::rcorr(cc2015_matrix2)
corrmatrix <- Hmisc::rcorr(cc2015_matrix2)
corrplot::corrplot(corrmatrix$r)
corrplot::corrplot(corrmatrix$r, order="hclust")





model2 <- '
Aggregate=~HUM_RSD+OTHER_RSD+SOCSC_RSD+STEM_RSD+PDNFRSTAFF+S.ER.D+NONS.ER.D
PerCap=~PDNRSTAFF_PC+S.ER.D_PC+NONS.ER.D_PC

Overall=~Aggregate+PerCap

S.ER.D~~S.ER.D_PC
NONS.ER.D~~NONS.ER.D_PC
PDNFRSTAFF~~PDNRSTAFF_PC
PDNFRSTAFF~~S.ER.D
HUM_RSD~~SOCSC_RSD
S.ER.D~~STEM_RSD
PDNRSTAFF_PC~~S.ER.D_PC
NONS.ER.D~~HUM_RSD
NONS.ER.D~~SOCSC_RSD
'

lavaan_sem_r_cov <- lavaan::sem(model2, data=cc2015_r, std.lev=TRUE, orthogonal=FALSE, se="robust.huber.white")
lavaan::summary(lavaan_sem_r_cov, fit.measures=TRUE)
CCScores_r_cov <- as.data.frame(lavaan::predict(lavaan_sem_r_cov))
range_scores <- max(CCScores_r_cov$Overall) - min(CCScores_r_cov$Overall)
CCScores_r_cov$rate_2015 <- ifelse(CCScores_r_cov$Overall < min(CCScores_r_cov$Overall)+((1/3)*range_scores), 'A', ifelse(CCScores_r_cov$Overall > max(CCScores_r_cov$Overall)-((1/3)*range_scores), 'C', 'B'))
CC_table <- as.data.frame(cbind(cc2015_new$NAME, cc2015_new$BASIC2015, CCScores_r_cov$rate_2015))
colnames(CC_table) <- c("name", "basic2015", "rate2015")
table(cc2015_new$BASIC2015, CC_table$rate2015)

require(blavaan)
bsem_norm <- blavaan::bsem(model2, data=cc2015_r, cp='srs', std.lev=TRUE, convergence='auto', dp=dpriors())
summary(bsem_norm)
bNORM <- blavaan::bsem(model2, data=cc2015_r, cp='srs', std.lv=TRUE, convergence='auto', dp=dpriors(lambda="dnorm(0,10000)"))
summary(bNORM) #lambda
bUNIF <- blavaan::bsem(model2, data=cc2015_r, cp='srs', std.lev=TRUE, convergence='auto', dp=dpriors(lambda="dunif(0, 250)"))
summary(bUNIF)

