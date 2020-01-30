
#Load libraries
library(colorspace)
library (ggplot2)
library (dplyr)
library (tidyr)
library (reshape2)
library(ComplexHeatmap)
library(circlize)
library(readxl)
library(knitr)
library(kableExtra)
library(lubridate)
library(hexbin)
library(plotly)
library(hms)

#Set Directory
#setwd("/Users/Files")

#Examples of Scatterplots for Reference
##(Nothing fancy)

##Load file

##Pull Data (read_excel)
sessions <- read_excel("/Users/file.xlsx", sheet = "sheet1") 

##See what I've got
dim (sessions)

##Create variables for scatter plots (log file analysis example)

##Create standardized, natural log, and log variables for duration and actions

###standardized
sessions$zDuration <- (sessions$totMin - mean(sessions$totMin)) / sd(sessions$totMin)
sessions$zActions <- (sessions$totalAction - mean(sessions$totalAction)) / sd(sessions$totalAction)

###Natural Log
sessions$lnDurationMin <- log(sessions$totMin + 1)
sessions$lnActions <- log(sessions$totalAction + 1)

###Log
sessions$log10DurationMin <- log10(sessions$totMin + 1)
sessions$log10Actions <- log10(sessions$totalAction + 1)

##Set some specs to play with

###Set some specs to play with
cbPalette <- c("#0072B2", "#D55E00", "#999999")
shape1 <- 1
size1 <- 1.5
alpha1 <- 0.6
stroke1 <- 2

### Other colors in colorblind palette -- "#E69F00", "#56B4E9",   "#F0E442",  "#D55E00", "#CC79A7"

##Create Scatterplot of All Sessions, actual values
###X = Minutes of Use, Y = User Actions, Color = staff role

##Create scatterplot of all sessions
p <-  ggplot(sessions, aes(x=totMin, y=totalAction)) + 
      geom_point(shape=shape1, size = size1, alpha=alpha1, stroke=stroke1) +
      scale_color_manual(values=c(cbPalette)) +
      geom_smooth(method = lm) +
      theme_light() +
      theme(legend.title = element_text(size=16), 
            legend.text = element_text(size = 14), axis.text.x = element_text(size = 16), 
            axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 16),
            axis.title.y = element_text(size = 16)) +
      labs(title = "Figure TK: Scatterplot of Sessions by Duration and Number of Actions", 
       subtitle = "YEAR",
       x = "Length of Session (minutes)", 
       y = "Number of Actions in the Session",
       caption = "Note. TBA")
p

##Create Scatterplot of All Sessions, actual values, facet by staff role
###X = Minutes of Use, Y = User Actions, Color = staff role

##Create scatterplot of all sessions, facet by staff role
p <-  ggplot(sessions, aes(x=totMin, y=totalAction, color=StaffCodeName)) + 
      geom_point(shape=shape1, size = size1, alpha=alpha1, stroke=stroke1) +
      scale_color_manual(values=c(cbPalette)) +
      geom_smooth(method = lm) +
      theme_light() +
      theme(legend.title = element_text(size=16), legend.text = element_text(size = 14),
            axis.text.x = element_text(size = 16), 
            axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 16),
            axis.title.y = element_text(size = 16)) +
      labs(title = "Figure TK: Scatterplot of Sessions by Duration and Number of Actions", 
       subtitle = "YEAR",
       x = "Session Duration (minutes)", 
       y = "Number of Report Actions",
       caption = "Note. TBA")

p + facet_grid(StaffCodeName ~ .)

##Scatterplot of all Sessions, Natural Log, All Sessions, All Sessions with Facet by Staff role, Hex Binning
###X = Natural Log of Minutes of Use, Y = Natural Log User Actions, Color = staff role 

pln <-  ggplot(sessions, aes(x=lnDurationMin, y=lnActions, color=StaffCodeName)) + 
        geom_point(shape=shape1, size = size1, alpha=alpha1, stroke=stroke1) +
        scale_color_manual(values=c(cbPalette)) +
        geom_smooth(method = lm) +
        theme_light() +
        theme(legend.title = element_text(size=16), legend.text = element_text(size = 14),
              axis.text.x = element_text(size = 16), axis.title.x = element_text(size = 16),
              axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16)) +
        labs(title = "Scatterplot of Sessions by Natural Log of Duration and Nat. Log of Number of Actions",
             subtitle = "2016-17",
             x = "Natural log of Duration  (Minutes)",
             y = "Natural log of Report Actions",
             caption = "Note. TBA")

pln

pln + facet_grid(StaffCodeName ~ .)

bin <- hexbin(sessions$lnDurationMin, sessions$lnActions, xbins=50)
hexbin <- plot(bin, main="hexagonal binning")
hexbin

##Links/Sources
###http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

