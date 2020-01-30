#Load libraries
library(ComplexHeatmap)
library(circlize)
library(hopach)
library(dendextend)
library(cluster)
library(tidyverse)
library(factoextra)
library(NbClust)
library(plyr)

# http://www.bioconductor.org/packages/release/bioc/html/hopach.html

#Set Directory
setwd("/Users/files")

#Read .csv
mainteacherdata <- read.csv("file.csv", na.strings = c("", "NA"), 
                            check.names = FALSE, header = TRUE, row.names = 2)
dim (mainteacherdata)

##load in Columns and filter for set of observations
teacherdata <- mainteacherdata[mainteacherdata$SurveyFlagN35 == 1,
  c(
    "Useful: Own Assess.",
    "Useful: Own Observ.",
    "Useful: Own Gradebook",
    "Useful: Instr. Softw.",
    "Useful: State Standard",
    "Useful: IEP Info",
    "Useful: Past Grades", 
    "Useful: Inter. Test", 
    "Useful: Progr. Quiz", 
    "Useful: Comp. Adapt Test", 
    "Useful: Cog. Skills", 
    "Useful: State Test", 
    "Useful: Regent Test", 
    "Useful: State Test Predict.", 
    "Useful: Attend/Tard.", 
    "Useful: Discipline Inc.", 
    "Useful: Drop Out Pred.", 
    "Useful: Lexile"
  )]

##Change column names for heatmap
colnames(teacherdata) <- 
  c(
    "Own Assessments",
    "Own Observations",
    "Own Gradebook",
    "Instructional Software",
    "State Standards",
    "IEP Information",
    "Past Grades", 
    "Interim Testing", 
    "Progress Quizzes", 
    "Computer Adaptive Tests", 
    "Cognitive Skills", 
    "State Tests", 
    "Regents Tests", 
    "State Test Predictions", 
    "Attendance/Tardiness", 
    "Discipline Incidents.", 
    "Drop Out Predictions", 
    "Lexile Level"
  )

dim (teacherdata)
head (teacherdata)

data <- teacherdata

###Scale non-standardized data
data.scale <- scale(data)

dim(data.scale)
head(data.scale)

#Create Dissimilarity matrix
PERUSEDataTypes <- dist(data.scale, method = "euclidean")

# Cluster with method
hc1 <- hclust(PERUSEDataTypes, method = "ward.D2" )

# Cut tree into K groups
sub_grp <- cutree(hc1, k = 3)

# Find Number of members in each cluster
table(sub_grp)

##Looks like this
## sub_grp
##  1  2  3  4 
##  7 12 19 12

###Ways to dress up the matrix above
#data.scale %>%
#  mutate(cluster = sub_grp) %>%
#  head

##Should look like this
##   Murder Assault UrbanPop Rape cluster
## 1   13.2     236       58 21.2       1
## 2   10.0     263       48 44.5       2
## 3    8.1     294       80 31.0       2
## 4    8.8     190       50 19.5       3
## 5    9.0     276       91 40.6       2
## 6    7.9     204       78 38.7       2

##Plot the obtained dendrogram
plot(hc1, cex = 1, hang = -1, main = "HCA: Perceived Usefulness of Data Types")
rect.hclust(hc1, k = 3, border = 2:5)

###Use NbClust() to test for number of clusters with voting among methods
nb <- NbClust(data.scale, distance = "euclidean", min.nc = 2,
              max.nc = 7, method = "ward.D2", index ="alllong")

# Print the resultnb_clust
nb

#It's possible to visualize the result using the function fviz_nbclust() [in factoextra], as follow:
fviz_nbclust(nb) + theme_minimal()

#Additional Code using agnes() to do same tests for suggested number of clusters
##Cluster visualization of groups
###fviz_cluster(list(data = data.scale, cluster = sub_grp))

##Compute agglomerative coefficient with agnes and compare linkage messages
###hc1 <- agnes(data.scale, method = "ward")

##Agglomerative coefficient
###hc1$ac
##Looks like this
###[1] 0.8531583

##Methods for linkage calculations
#m <- c( "average", "single", "complete", "ward")
#names(m) <- c( "average", "single", "complete", "ward")

##function to compute coefficient
###ac <- function(x) {
###agnes(data.scale, method = x)$ac
###}

###map_dbl(m, ac)
##Looks like this
##   average    single  complete      ward 
## 0.7379371 0.6276128 0.8531583 0.9346210

##visualize dendrogram with Agnes
###hc3 <- agnes(data.scale, method = "ward")
###pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of Basic Usage Factors") 

##Elbow Method to determine optimal clusters
###hcut(data.scale,
###     hc_func = "hclust", 
###     hc_method = "ward.D2", 
###     hc_metric = "euclidean")

###fviz_nbclust(data.scale, hcut, 
###             method = "wss", k.max = 7) + geom_vline(xintercept = 3, linetype = 2)

##Average Silhouette Method to determine optimal clusters
###fviz_nbclust(data.scale, FUN = hcut, method = "silhouette", k.max = 5, hc_method = "ward.D2")

##Gap Statistic Method
###gap_stat <- clusGap(data.scale, FUN = hcut, K.max = 5, B = 40)
####fviz_gap_stat(gap_stat, maxSE = list(method = "firstSEmax", SE.factor = 1))

##NOTE on "ward" vs. "ward.D2"
#Two different algorithms are found in the literature for Ward clustering. 
#The one used by option "ward.D" 
#(equivalent to the only Ward option "ward" in R versions <= 3.0.3) 
#does not implement Ward's (1963) clustering criterion, 
#whereas option "ward.D2" implements that criterion (Murtagh and Legendre 2014). 
#With the latter, the dissimilarities are squared before cluster updating. 
#Note that agnes(*, method="ward") corresponds to hclust(*, "ward.D2").
