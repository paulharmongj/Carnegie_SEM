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

#lavaan_sem_b <- lavaan::sem(model1, data=cc2015_new, std.lv=TRUE,se="boot",test="Bollen.Stine",bootstrap=1000, orthogonal=FALSE)
#lavaan::summary(lavaan_sem_b, fit.measures=TRUE)
#lavaan::parameterEstimates(lavaan_sem_b,boot.ci.type="norm",standardized=T)


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


#Generates the SEM
lavaan_sem_r_cov <- lavaan::sem(model2, data=cc2015_r, std.lv=TRUE, orthogonal=FALSE, se="robust.huber.white")
lavaan::summary(lavaan_sem_r_cov, fit.measures=TRUE)


#Generates PCA scores for comparison
PC_ag <- prcomp(cc2015.r[,-c(1,2,3,4)], scale = TRUE)
PC_percap <- prcomp(cc2015percap.r, scale = TRUE)

PC_ag_pred <- predict(PC_ag)[,1]
PC_percap_pred <- predict(PC_percap)[,1]



#alternative path diagrams
source('https://raw.githubusercontent.com/brandmaier/onyxR/master/tools/install.R')
library(onyxR)
onyx(lavaan_sem_r_cov)
onyx(lavaan_sem_b)
onyx(lavaan_sem_r)
PC_pred_scale <- apply(as.data.frame(cbind(PC_ag_pred,PC_percap_pred)),2,scale)


#predicts the scores
CCScores_r_cov <- as.data.frame(lavaan::predict(lavaan_sem_r_cov))
CCScores_r_cov_scale <- apply(CCScores_r_cov[,c(1,2)], 2, scale)
range_scores <- max(CCScores_r_cov$Overall) - min(CCScores_r_cov$Overall)
CCScores_r_cov$rate_2015 <- ifelse(CCScores_r_cov$Overall < min(CCScores_r_cov$Overall)+((1/3)*range_scores), 'A', ifelse(CCScores_r_cov$Overall > max(CCScores_r_cov$Overall)-((1/3)*range_scores), 'C', 'B'))
CC_table <- as.data.frame(cbind(cc2015_new$NAME, cc2015_new$BASIC2015, CCScores_r_cov$rate_2015))
colnames(CC_table) <- c("name", "basic2015", "rate2015")


#misclassification rate
table(cc2015_new$BASIC2015, CC_table$rate2015)
library(ggplot2)
a <- which(cc2015$new$NAME == "Montana State University")
ggplot(CCScores_r_cov) + geom_point(aes(x = Aggregate, y = PerCap, color = cc2015_new$BASIC2015)) + 
  ggtitle("Predicted vs Actual Classifications") #+ geom_point(aes(x = Aggregate,y = PerCap,size = 4))

#not sure I'm a big fan of this package, but it does create alternate path diagrams

###lets see if we can calculate distances with schools
Score_dist <- as.data.frame(cbind(CCScores_r_cov_scale, PC_pred_scale))
rownames(Score_dist) <- cc2015_new$NAME

#recreates the PC-based plot
ggplot(Score_dist) + geom_point(aes(x = PC_ag_pred, y = -PC_percap_pred, color = cc2015_new$BASIC2015)) + ggtitle("Carnegie Classification PC Plot")

ggplot(Score_dist) + geom_point(aes(x = Aggregate, y = PerCap, color = CCScores_r_cov$rate_2015))


#calculate distances 
get_Distance <- function(x1,x2,y1,y2){
  
 #calculates the euclidean distance between the points  
  dist <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
 distx <- x2 - x1
 disty <- y2 - y1
  #returns distances
  return(list( dist = dist, distx = distx, disty = disty ))
}

Score_dist$Distance <- c()

for (j in 1:nrow(Score_dist)){
  Score_dist$Distance[j] <- get_Distance(x1 = Score_dist[j,"Aggregate"],x2 = Score_dist[j,"PC_ag_pred"],y1 = Score_dist[j,"PerCap"],y2 = -Score_dist[j,"PC_percap_pred"])$dist

  }

#Let's look at distances
sorted <- Score_dist[order(Score_dist$Distance),]
head(sorted,10)
tail(sorted,10)

summary(Score_dist$Distance)

#show biggest movers on Carnegie Classifications Plot
ggplot(Score_dist) + geom_point(aes(x = PC_ag_pred, y = -PC_percap_pred, color = as.factor(floor(Distance)))) + ggtitle("Biggest Movers")


#look at x, y distances

distance_matrix <- matrix(0,nrow = nrow(Score_dist), ncol = 3)
for (j in 1:nrow(Score_dist)){
  distance_matrix[j,] <- get_Distance(x1 = Score_dist[j,"Aggregate"],x2 = Score_dist[j,"PC_ag_pred"],y1 = Score_dist[j,"PerCap"],y2 = -Score_dist[j,"PC_percap_pred"])
  
}
#look at squared distances in one direction


#more stuff with laura's suggestions
#idea: to smash everything into a single latent trait
model.carnegie.all <- '
#measurement model
overall =~ S.ER.D + NONS.ER.D + STEM_RSD + SOCSC_RSD + OTHER_RSD + HUM_RSD + PDNFRSTAFF + FACNUM

#Correlations
S.ER.D ~~ STEM_RSD 
S.ER.D ~~ PDNFRSTAFF
NONS.ER.D ~~ HUM_RSD
NONS.ER.D ~~ SOCSC_RSD
SOCSC_RSD ~~ HUM_RSD

'

#Generates the SEM
lavaan_sem_r_ALL <- lavaan::sem(model.carnegie.all, data=cc2015_r, std.lv=TRUE, orthogonal=FALSE, se="robust.huber.white")
lavaan::summary(lavaan_sem_r_ALL, fit.measures=TRUE)

#pathdiagram
semPaths(lavaan_sem_r_ALL)

#alternative parameterization: counts and funding as two different STEM/nonSTEM indices
model_alt <- '
STEM=~+STEM_RSD+PDNFRSTAFF+S.ER.D + FACNUM
HUM=~HUM_RSD + OTHER_RSD + SOCSC_RSD + NONS.ER.D + FACNUM

Overall=~STEM+HUM



NONS.ER.D ~~ HUM_RSD
NONS.ER.D ~~ SOCSC_RSD

'
lavaan_sem_r_alternate <- lavaan::sem(model_alt, data=cc2015_r, std.lv=TRUE, orthogonal=FALSE, se="robust.huber.white")
lavaan::summary(lavaan_sem_r_alternate, fit.measures=TRUE)

#path diagram
semPaths(lavaan_sem_r_alternate)


predict_alt <- lavaan::predict(lavaan_sem_r_alternate)
plot(-predict_alt[,1],-predict_alt[,2], col = cc2015_r$BASIC2015, pch = 11)
plot(density(-predict_alt[,3]))




#alternative model with added facnum as its own latent trait
#alternative parameterization: counts and funding as two different STEM/nonSTEM indices
model_alt_2 <- '
STEM =~+ STEM_RSD + S.ER.D
HUM =~ HUM_RSD + OTHER_RSD + SOCSC_RSD + NONS.ER.D
FACSIZE =~ PDNFRSTAFF + FACNUM

Overall=~STEM+HUM+FACSIZE


PDNFRSTAFF~~S.ER.D
HUM_RSD~~SOCSC_RSD
S.ER.D~~STEM_RSD
NONS.ER.D~~HUM_RSD
NONS.ER.D~~SOCSC_RSD

'
lavaan_sem_r_alternate_2 <- lavaan::sem(model_alt_2, data=cc2015_r, std.lv=TRUE, orthogonal=FALSE, se="robust.huber.white")
lavaan::summary(lavaan_sem_r_alternate_2, fit.measures=TRUE)

#path diagram
semPaths(lavaan_sem_r_alternate_2)

