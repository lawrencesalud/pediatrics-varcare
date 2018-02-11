# August 28, 2014
#
# Title: Data Mining of the Heritage Health Prize Data for Pediatric Healthcare
# Author: Lawrence H. Salud (c) 2014
# Provided for sumbission fulfillment of Final Paper,
# Data Mining Principles, MsCA Course #31008
# The University of Chicago
#
## The first and second iteration of this data mining process 
# occured in V1 and V2 revisions of this R script. All analysis however
# was included in this version V3
#
# Read in cleaned data file
#
# 
# Revisions:
#   Date      Author      Description
#   4/18/2015 L. Salud    Added pairs method to vizualize procedures, diagnosis and specialty
#
library("partykit")
library("caret")
library("party")

setwd("/Users/lawrencesalud/Documents/Software Projects/Data Mining Principles - An Example for Pediatric Healthcare")
kids <- read.csv("modeling_set1_kids_yngadlt.csv")
summary(kids)

special_vs_pg = subset(kids, select = c("no_Specialities","no_ProcedureGroups"))
pairs(special_vs_pg)

pcg_vs_pg = subset(kids, select = c("no_PrimaryConditionGroups","no_ProcedureGroups"))
pairs(pcg_vs_pg)

# Histogram and Boxplot origional data
hist(kids$no_ProcedureGroups)
boxplot(kids$no_ProcedureGroups)
mtext(side=1, "kids$no_ProcedureGroups")

sum(kids$no_ProcedureGroups <= 6)/sum(kids$no_ProcedureGroups)
# Get only kids with Acccute Resperatory (pcg3)
# A second good candidate with twice the proceedures is Cancer A (pcg9)
kids<-subset(kids, pcg3 !=0,)
# Get only age 0 to 25 
kids<-subset(kids, age_05 !=0 | age_15 != 0 | age_25 != 0,)

which( colnames(kids)=="pcg3" )

# Heatmap of all pcgs
pcg <- subset(kids, select = c(42:87))
pcg <- as.matrix(pcg)
heatmap(pcg, Colv = NA, Rowv = NA, labRow = NA)
# Heatmap of select pcgs
pcg <- subset(kids, select = c(pcg3, pcg20, pcg10, pcg1, pcg5))
pcg <- as.matrix(pcg)
heatmap(pcg, Colv = NA, Rowv = NA, labRow = NA)

# Try to do some KNN stuff to eliminate incompatable patients
pcg <- subset(kids, select = c(42:87))
pcg$pcg28 <- NULL
pcg$pcg42 <- NULL

summary(pcg)
pcg <- na.omit(pcg)
row.names(pcg)<-NULL

means <- apply(pcg,2,mean)
sds<- apply(pcg,2,sd)

pcg.scale <- scale(pcg,center=means,scale=sds)
# Try PCA only
pcg.pca <- prcomp(pcg, scale=TRUE)

# Try PCA with Kmeans
fit <- kmeans(pcg , 10)
aggregate(pcg,by=list(fit$cluster),FUN=mean)
k <- data.frame(pcg, fit$cluster)
kids$cluster <- fit$cluster

pcg.pca <- prcomp(pcg, scale=TRUE)
plot(pcg.pca)
mtext(side=1, "PCG/Diagnosis Principle Components")
summary(pcg.pca)
pred<- predict(pcg.pca)
summary(pred)
PCA1 <- pred[,1] # PCA1
PCA2 <- pred[,2] # PCA2
PCA10 <- pred[,10] # PCA10
PCA44 <- pred[,44] # PCA10

#This time Try PCA 9, etc.

plot(PCA1, PCA2, xlim=c(-1,1), ylim=c(-1,1), type="n" )
text(PCA1, PCA2, labels = fit$cluster, col=fit$cluster)
# Eliminate clusters 1

fit.pca<- kmeans(pred, 10)
aggregate(pcg,by=list(fit.pca$cluster),FUN=mean)
k.pca<- data.frame(pcg, fit.pca$cluster)

plot(PCA1, PCA2, xlim=c(-1,1), ylim=c(-1,1), type="n" )
text(PCA1, PCA2, labels = fit.pca$cluster, col=fit.pca$cluster)

# Plot sum of squares if no strong clusters are found
mydata <- pred
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:100) wss[i] <- sum(kmeans(mydata,
                                      centers=i)$withinss)
plot(1:100, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# add
kids$pca.cluster <- 0
kidsSimilar <- data.frame(kids, fit.pca$cluster)

# Eliminate clusters 2 or the most dissimailar cluster
kidsSimilar <- kidsSimilar[kidsSimilar$fit.pca.cluster != 2, ]

kids <- kidsSimilar
###
##
hist(kids$no_ProcedureGroups)
boxplot(kids$no_ProcedureGroups)
mtext(side=1, "kids$no_ProcedureGroups")

sum(kids$no_ProcedureGroups > 4)/sum(kids$no_ProcedureGroups)
count <- table(kids$no_ProcedureGroups > 4)
count <- table(kids$no_ProcedureGroups)
barplot(count, main="PG Number of Proceedures",
        xlab="PG") 

# table(pcg)
# pcg <- as.data.frame(pcg)

# How many pure pcg 3
# kids<-subset(kids, select = c(44:86) == 0)

kids$PG_VAR <- "INIT"
kids[kids$no_ProcedureGroups < 3, which( colnames(kids) =="PG_VAR" )] <- "LOW"
kids[kids$no_ProcedureGroups >= 3 & kids$no_ProcedureGroups <= 4, which( colnames(kids) =="PG_VAR" )] <- "MED"
kids[kids$no_ProcedureGroups > 4, which( colnames(kids) =="PG_VAR" )] <- "HI"

# Barchart this variation
counts <- table(kids$PG_VAR)
barplot(counts, main="PG Variations",
        xlab="Variation of Treatments/PG") 

hist(kids$CharlsonIndexI_ave)
counts <- table(kids$CharlsonIndexI_ave)
barplot(counts, main="CIndex Average Variations",
        xlab="Variation of CIndex") 

hist(kids$no_PrimaryConditionGroups)
hist(kids$no_PrimaryConditionGroups, breaks = 12, col = "lightblue", border = "pink")
counts <- table(kids$no_PrimaryConditionGroups)
barplot(counts, main="No. of Primary Condition Groups/Dx",
        xlab="Dx - Diagnosis (PCG)") 
boxplot(kids$no_PrimaryConditionGroups)

hist(kids$no_Specialities)
counts <- table(kids$no_Specialities)
barplot(counts, main="No. of Specialties",
        xlab="Specialties") 
boxplot(kids$no_Specialities)


# Do Decision Tree
assessKids <- data.frame(matrix(nrow = nrow(kids)))

# Xform age
kids$age <- "INIT"
kids[kids$age_05 == 1, which( colnames(kids) =="age" )] <- "ZeroToFive"
kids[kids$age_15 == 1, which( colnames(kids) =="age" )] <- "SixToFifteen"
kids[kids$age_25 == 1, which( colnames(kids) =="age" )] <- "SixteenToTwentyFive"

assessKids$age <- kids$age
assessKids$matrix.nrow...nrow.kids..<- NULL

kids$gender <- "INIT"
kids[kids$sexMALE == 1, which( colnames(kids) =="gender" )] <- "Male"
kids[kids$sexFEMALE == 1, which( colnames(kids) =="gender" )] <- "Female"
kids[kids$sexMISS == 1, which( colnames(kids) =="gender" )] <- "Missing"

assessKids$gender <- kids$gender
assessKids$PG_VAR <- kids$PG_VAR

kids$DX_VAR <- "INIT"
kids[kids$no_PrimaryConditionGroups < 3, which( colnames(kids) =="DX_VAR" )] <- "LOW"
kids[kids$no_PrimaryConditionGroups >= 3 & kids$no_PrimaryConditionGroups <= 4, which( colnames(kids) =="DX_VAR" )] <- "MED"
kids[kids$no_PrimaryConditionGroups > 4, which( colnames(kids) =="DX_VAR" )] <- "HI"

counts <- table(kids$DX_VAR)
barplot(counts, main="No. of DX",
        xlab="DX Diagnosis") 

assessKids$DX_VAR <- kids$DX_VAR

kids$SPECIALTY_VAR <- "INIT"
kids[kids$no_Specialities < 3, which( colnames(kids) =="SPECIALTY_VAR" )] <- "LOW"
kids[kids$no_Specialities >= 3 & kids$no_Specialities <= 4, which( colnames(kids) =="SPECIALTY_VAR" )] <- "MED"
kids[kids$no_Specialities > 4, which( colnames(kids) =="SPECIALTY_VAR" )] <- "HI"

assessKids$SPECIALTY_VAR <- kids$SPECIALTY_VAR

# Do this decision Tree

df <- assessKids
predictor <- PG_VAR~.
assessKids$DX_VAR <- as.factor(assessKids$DX_VAR)
assessKids$PG_VAR <- as.factor(assessKids$PG_VAR) # Target
assessKids$age <- as.factor(assessKids$age)
assessKids$gender <- as.factor(assessKids$gender)
assessKids$SPECIALTY_VAR <- as.factor(assessKids$SPECIALTY_VAR)

class(assessKids$SPECIALTY_VAR)
set.seed(1234)
modelsample <-sample(2, nrow(assessKids), replace=TRUE, prob=c(0.7, 0.3))
traindata   <-assessKids[modelsample==1, ]
testdata    <-assessKids[modelsample==2, ]

write.csv(kids, "resp_kids_yngadlt.csv")
write.csv(kids, "kids_traindata.csv")
df_ctree    <-ctree(predictor, data=assessKids)
#plot(df_ctree, type="simple")
#plot(df_ctree)

# USE THIS!!!
plot(df_ctree, gp = gpar(fontsize = 6),     # font size changed to 6
     inner_panel=node_inner,
     ip_args=list(
       abbreviate = FALSE, 
       id = FALSE)
)
table(predict(df_ctree), assessKids$PG_VAR)
confusionMatrix(predict(df_ctree), assessKids$PG_VAR)

##
##
## Now Do Decision Tree for DRG_VAR

# Histogram
# Drug Admin is count * no months
kids$drugsAdmin <- kids$drugCount_ave*kids$drugcount_months
hist(kids$drugsAdmin)
summary(kids$drugsAdmin)

# define NONE, LOW, MED, HIGH
kids$DRUGS_VAR <- "INIT"
kids[kids$drugsAdmin == 0, which( colnames(kids) =="DRUGS_VAR" )] <- "NONE"
kids[kids$drugsAdmin > 0 & kids$drugsAdmin < 2, which( colnames(kids) =="DRUGS_VAR" )] <- "LOW"
kids[kids$drugsAdmin >= 2 & kids$drugsAdmin <= 3, which( colnames(kids) =="DRUGS_VAR" )] <- "MED"
kids[kids$drugsAdmin > 3, which( colnames(kids) =="DRUGS_VAR" )] <- "HI"

#kids[kids$DRUGS_VAR == "INIT", which( colnames(kids) =="drugsAdmin" )] 

#Boxplot
boxplot(kids$drugsAdmin)
mtext(side=1, "kids$drugsAdmin")

# Boxplot
counts <- table(kids$DRUGS_VAR)
barplot(counts, main="Drugs Addministration Variations",
        xlab="Variation of Drugs Treatment") 

# Drop PG_VAR
assessKids$PG_VAR <- NULL 
# Add DRUGS_VAR
assessKids$DRUGS_VAR <- as.factor(kids$DRUGS_VAR) # Target

# Plot Tree
set.seed(1234)
modelsample <-sample(2, nrow(assessKids), replace=TRUE, prob=c(0.7, 0.3))
traindata   <-assessKids[modelsample==1, ]
testdata    <-assessKids[modelsample==2, ]
df_ctree    <-ctree(DRUGS_VAR~., data=assessKids)
plot(df_ctree, type="simple")
plot(df_ctree)

# USE THIS!!!
plot(df_ctree, gp = gpar(fontsize = 9),     # font size changed to 6
     inner_panel=node_inner,
     ip_args=list(
       abbreviate = FALSE, 
       id = FALSE)
)
table(predict(df_ctree), assessKids$DRUGS_VAR)

confusionMatrix(predict(df_ctree), assessKids$DRUGS_VAR)
##
##
## Now Do Decision Tree for LABS_VAR

# Histogram
# Drug Admin is count * no months
kids$labsAdmin <- kids$labCount_ave*kids$labcount_months
hist(kids$labsAdmin)
summary(kids$labsAdmin)

# define NONE, LOW, MED, HIGH
kids$LABS_VAR <- "INIT"
kids[kids$labsAdmin == 0, which( colnames(kids) =="LABS_VAR" )] <- "NONE"
kids[kids$labsAdmin > 0 & kids$labsAdmin < 3, which( colnames(kids) =="LABS_VAR" )] <- "LOW"
kids[kids$labsAdmin >= 3 & kids$labsAdmin <= 4, which( colnames(kids) =="LABS_VAR" )] <- "MED"
kids[kids$labsAdmin > 4, which( colnames(kids) =="LABS_VAR" )] <- "HI"

#Boxplot
boxplot(kids$labsAdmin)
mtext(side=1, "kids$labsAdmin")

# Boxplot
counts <- table(kids$LABS_VAR)
barplot(counts, main="Labs Addministration Variations",
        xlab="Variation of Labs Intervention") 

# Drop PG_VAR
assessKids$DRUGS_VAR <- NULL 
# Add DRUGS_VAR
assessKids$LABS_VAR <- as.factor(kids$LABS_VAR) # Target

# Plot Tree
set.seed(1234)
modelsample <-sample(2, nrow(assessKids), replace=TRUE, prob=c(0.7, 0.3))
traindata   <-assessKids[modelsample==1, ]
testdata    <-assessKids[modelsample==2, ]

df_ctree    <-ctree(LABS_VAR~., data=assessKids)
plot(df_ctree, type="simple")
plot(df_ctree)

# USE THIS!!!
plot(df_ctree, gp = gpar(fontsize = 11),     # font size changed to 6
     inner_panel=node_inner,
     ip_args=list(
       abbreviate = FALSE, 
       id = FALSE)
)
table(predict(df_ctree), assessKids$LABS_VAR)

confusionMatrix(predict(df_ctree), assessKids$LABS_VAR)
