#Load libraries
library(ComplexHeatmap)
library(circlize)
library(hopach)
library(dendextend)
# http://www.bioconductor.org/packages/release/bioc/html/hopach.html

#Set Directory
setwd("/Users/files")

#Read .csv
mainteacherdata <- read.csv("file", na.strings = c("", "NA"), 
                            check.names = FALSE, header = TRUE, row.names = 2)
dim (mainteacherdata)

##Pull in specific columns
teacherdata <- mainteacherdata[mainteacherdata$SurveyFlag == 1, 
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

##Give columns different names for heatmap figure
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
data.scale

dim (data.scale)
head (data.scale)

##Plot heatmap
### Plot heatmap with row and column cluster 
ht<-Heatmap(data.scale, name="HCA Factors",
            col = colorRamp2(c(-2,-1, 0,1,2), 
                             c("blue","cornflowerblue", "grey","coral1", "red")),
            ##Titles for 3 clusters defined with split = 
            row_title = c("Clust1", "Clust2", "Clust3"),
            row_title_gp = gpar(fontsize = 9, fontface = "bold"),
            row_dend_width = unit(18, "mm"),
            column_title = "Perceived Usefulness of Data Types",
            column_title_gp = gpar(fontsize = 13, fontface = "bold"),
            cluster_rows = TRUE,
            ##clustering distance and linkage method -- many options
            clustering_distance_rows = "euclidean",
            clustering_method_rows = "ward.D2",
            cluster_columns = TRUE,
            column_dend_height = unit(18, "mm"),
            ##clustering distance and linkage method -- many options
            clustering_distance_columns = "euclidean",
            clustering_method_columns = "ward.D2",
            show_column_names = (TRUE),
            column_names_gp = gpar(fontsize = 12), 
            show_row_names = (TRUE), 
            row_names_gp = gpar(fontsize = 8),
            ##Split heatmap/dendrogram on height of dendrogram at 3 groups
            split = 3
)

##Other options
###row_dend_side = "right"
##Perform k-means split prior to clustering
###km = 1)

##add annotation data
###In later versions used same initial .csv to create annotations. Simpler this way for file maintenance
###but have to make n of both files exactly the same

##Read .csv
mainteacherdemo <- read.csv("annotationFile.csv", na.strings = c("", "NA"), check.names = FALSE, 
                            header = TRUE, row.names = 2)

##Filter to relevant set
teacherdemo <- mainteacherdemo[mainteacherdemo$SurveyFlag == 1,]

dim (teacherdemo)
head (teacherdemo)

##Create variables for annotations

###Convert Continuous annotations to dichotomous based on mean or median
mean_exp<-mean(teacherdemo[, 7])
experience<-ifelse(teacherdemo[,7]<mean_exp, 0,1)

mean_usage<-mean(teacherdemo[,20])
usage<-ifelse(teacherdemo[,20]<mean_usage, 0, 1)

###Create threshold for annotation
levuser<-ifelse(teacherdemo[,15]>2, 1, 0)

###Categorical annotations
middleschool<-ifelse(teacherdemo[,3]=="Middle", 1, 0)
highschool<-ifelse(teacherdemo[,3]=="High", 1, 0)
ela<-ifelse(teacherdemo[,5]=="ELA", 1, 0)
math<-ifelse(teacherdemo[,5]=="Math", 1, 0)
socialstudies<-ifelse(teacherdemo[,5]=="Social Studies", 1, 0)
science<-ifelse(teacherdemo[,5]=="Science", 1, 0)

##Begin Plotting heatmap with annotation

###Plot Usage Annotation
ht_usage <- Heatmap(usage, name="Total Duration of Use", 
                    clustering_distance_rows = function(m) 
                      as.dist(as.matrix(distancematrix(m, d="euclidean"))),
                    clustering_method_rows = "ward.D2", 
                    col = colorRamp2(c(0,1), c("white", "black")),
                    heatmap_legend_param = list(at = c(0,1), labels = c("Below Average", "Above Average")),
                    show_row_names = FALSE, width = unit(2, "mm"), 
                    show_column_names = TRUE, 
                    column_names_gp = gpar(fontsize = 12, fontface = "bold"))

#Plot School Level Annotation
ht_schoollevel <- Heatmap(highschool, name="School Level", 
                          clustering_distance_rows =function(m) 
                            as.dist(as.matrix(distancematrix(m, d="euclidean"))), 
                          clustering_method_rows = "ward.D2",
                          col = colorRamp2(c(0,1), c("red", "black")), 
                          heatmap_legend_param = list(at = c(0,1), labels = c("Middle School", "High School")),
                          show_row_names = FALSE, width = unit(2, "mm"), 
                          show_column_names = TRUE, column_names_gp = gpar(fontsize = 12, fontface = "bold"))

#Plot ELA Content Area
ht_ela <- Heatmap(ela, name="ELA", 
                  clustering_distance_rows =function(m)
                    as.dist(as.matrix(distancematrix(m, d="euclidean"))), 
                  clustering_method_rows = "ward.D2",
                  col = colorRamp2(c(0,1), c("white", "black")), 
                  heatmap_legend_param = list(at = c(0,1), labels = c("No", "Yes"), title = "Content Area"),
                  show_row_names = FALSE, width = unit(2, "mm"),
                  show_column_names = TRUE, column_names_gp = gpar(fontsize = 12, fontface = "bold"), show_heatmap_legend = TRUE)

#Plot Math Content Area
ht_math <- Heatmap(math, name="Math", 
                   clustering_distance_rows =function(m) 
                     as.dist(as.matrix(distancematrix(m, d="euclidean"))),
                   clustering_method_rows = "ward.D2",
                   col = colorRamp2(c(0,1), c("white", "black")), 
                   heatmap_legend_param = list(at = c(0,1), labels = c("No", "Yes")),
                   show_row_names = FALSE, width = unit(2, "mm"), 
                   show_column_names = TRUE, column_names_gp = gpar(fontsize = 12, fontface = "bold"), 
                   show_heatmap_legend = FALSE)

#Plot Science Content Area
ht_science <- Heatmap(science, name="Science", 
                      clustering_distance_rows =function(m) 
                        as.dist(as.matrix(distancematrix(m, d="euclidean"))), 
                      clustering_method_rows = "ward.D2",
                      col = colorRamp2(c(0,1), c("white", "black")), 
                      heatmap_legend_param = list(at = c(0,1), labels = c("No", "Yes")),
                      show_row_names = FALSE, width = unit(2, "mm"), 
                      show_column_names = TRUE, column_names_gp = gpar(fontsize = 12, fontface = "bold"), 
                      show_heatmap_legend = FALSE)

#Plot Social Studies Content Area
ht_socialstudies <- Heatmap(socialstudies, name="Social Studies", 
                            clustering_distance_rows =function(m) 
                              as.dist(as.matrix(distancematrix(m, d="euclidean"))),
                            clustering_method_rows = "ward.D2",
                            col = colorRamp2(c(0,1), c("white", "black")), 
                            heatmap_legend_param = list(at = c(0,1), labels = c("No", "Yes")),
                            show_row_names = FALSE, width = unit(2, "mm"),
                            show_column_names = TRUE, column_names_gp = gpar(fontsize = 12, fontface = "bold"), 
                            show_heatmap_legend = FALSE)

#Draw heatmap plus all annotations together
draw(ht + ht_schoollevel + ht_usage + ht_ela + ht_math + ht_science + ht_socialstudies) 