#Load libraries
library(colorspace)
library (ggplot2)
library (dplyr)
library (tidyr)
library (reshape2)
library(readxl)
library(kableExtra)
library(lubridate)
library(plotly)
library(hms)

###Area Chart of Report Use for All Sessions over the Year


#Read .xlsx
data <- read_excel("/Users/file.xlsx", sheet = "sheet1") 
dim (data)

##Calculating Total Duration by School Week and Report Type 
###This version uses manual changes to the excel file to include values for weeks and report type combinations that are null)
###To Do: Incorporate complete() function to fill in missing week/report type combinations

###filter out actions where reportType = blank or Cognos Bug, 
dataSess <- data %>%
  filter(reportType != "", reportType != "Cognos Bug") %>%
  select(schoolWeek, reportType, actionHrs) %>%
  group_by(schoolWeek, reportType) %>%
  summarize(totReportWeek = sum(actionHrs))
dim (dataSess)

#Convert week from factor to num
dataSess$schoolWeek <- as.numeric(as.character(dataSess$schoolWeek))

#Create area plot

##Colorblind palette values (for some kinds of colorblind)
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p <- ggplot(dataSess, aes(x=schoolWeek, y=totReportWeek, fill=reportType)) +
      geom_area(position = "stack", stat = "identity") +
      scale_fill_manual(values=cbPalette) +
      theme_classic() +
      theme(legend.position="top", legend.title = element_text(size=14), 
            legend.text = element_text(size = 12), 
            axis.text.x = element_text(size = 12 , angle = 60, hjust = 1)) 

p + labs(title = "Weekly Usage by Report Type", 
         subtitle = "N = TK Sessions",
         x = "Week of the School Year", 
         y = "Hours of Use",
         caption = "TBA",
         fill = "Report Category") + 
  
  #Add manual scale, breaks and month names        
  scale_x_continuous(limits=c(1,52), breaks = 
                       c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
                         11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 
                         21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 
                         31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 
                         41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 
                         51, 52),
                     labels = c("8/1/2016", "8/8/2016","8/15/2016","8/22/2016","8/29/2016",
                                "9/5/2016","9/12/2016","9/19/2016","9/26/2016",
                                "10/3/2016","10/10/2016","10/17/2016","10/24/2016","10/31/2016",
                                "11/7/2016","11/14/2016","11/21/2016","11/28/2016", 
                                "12/5/2016","12/12/2016","12/19/2016","12/26/2016",
                                "1/2/2017","1/9/2017","1/16/2017","1/23/2017","1/30/2017",
                                "2/6/2017","2/13/2017","2/20/2017","2/27/2017",
                                "3/6/2017","3/13/2017","3/20/2017","3/27/2017",
                                "4/3/2017","4/10/2017","4/17/2017","4/24/2017",
                                "5/1/2017","5/8/2017","5/15/2017","5/22/2017","5/29/2017",
                                "6/5/2017","6/12/2017","6/19/2017","6/26/2017",
                                "7/3/2017","7/10/2017","7/17/2017","7/24/2017")) +
  
  ##Add verical lines and labels for important calendar events
  geom_vline(xintercept=1, linetype="dotted", color="red", size=1.5) +
  geom_text(aes(x=1, label="State Test Results Released (Middle School)", y=115), colour="black", angle=90, vjust = 1.2, text=element_text(size= 7)) +
  
  geom_vline(xintercept=3, linetype="dotted", color="red", size=1.5) +
  geom_text(aes(x=3, label="High School State Testing", y=115), colour="black", angle=90, vjust = 1.2, text=element_text(size= 7)) +
  
  geom_vline(xintercept=5, linetype="dotted", color="red", size=1.5) +
  geom_text(aes(x=5, label="State Test Results Released (High School)", y=115), colour="black", angle=90, vjust = 1.2, text=element_text(size=7)) +
  
  geom_vline(xintercept=6, linetype="dotted", color="red", size=1.5) +
  geom_text(aes(x=6, label="First Day of School", y=115), colour="black", angle=90, vjust = 1.2, text=element_text(size=7)) +
  
  geom_vline(xintercept=16, linetype="dotted", color="red", size=1.5) +
  geom_text(aes(x=16, label="Re-Rostered IDW Reports Released", y=115), colour="black", angle=90, vjust = 1.2, text=element_text(size=7)) +
  
  geom_vline(xintercept=22, linetype="dotted", color="red", size=1.5) +
  geom_text(aes(x=22, label="Holiday", y=115), colour="black", angle=90, vjust = 1.2, text=element_text(size=7)) +
  
  geom_vline(xintercept=26, linetype="dotted", color="red", size=1.5) +
  geom_text(aes(x=26, label="High School State Testing", y=115), colour="black", angle=90, vjust = 1.2, text=element_text(size=7)) +
  
  geom_vline(xintercept=29, linetype="dotted", color="red", size=1.5) +
  geom_text(aes(x=29, label="Training on College Reports", y=115), colour="black", angle=90, vjust = 1.2, text=element_text(size=7)) +
  
  geom_vline(xintercept=30, linetype="dotted", color="red", size=1.5) +
  geom_text(aes(x=30, label="Holiday", y=115), colour="black", angle=90, vjust = 1.2, text=element_text(size=7)) +
  
  geom_vline(xintercept=35, linetype="dotted", color="red", size=1.5) +
  geom_text(aes(x=35, label="ELA State Test", y=115), colour="black", angle=90, vjust = 1.2, text=element_text(size=7)) +
  
  geom_vline(xintercept=37, linetype="dotted", color="red", size=1.5) +
  geom_text(aes(x=37, label="Holiday", y=115), colour="black", angle=90, vjust = 1.2, text=element_text(size=7)) +
  
  geom_vline(xintercept=40, linetype="dotted", color="red", size=1.5) +
  geom_text(aes(x=40, label="Math State Test", y=115), colour="black", angle=90, vjust = 1.2, text=element_text(size=7)) +
  
  geom_vline(xintercept=40, linetype="dotted", color="red", size=1.5) +
  geom_text(aes(x=40, label="Math State Test", y=115), colour="black", angle=90, vjust = 1.2, text=element_text(size=7)) +
  
  geom_vline(xintercept=44, linetype="dotted", color="red", size=1.5) +
  geom_text(aes(x=44, label="Preliminary State Test Results Released (ELA 3rd-8th)", y=110), colour="black", angle=90, vjust = 1.2, text=element_text(size=7)) +
  
  geom_vline(xintercept=46, linetype="dotted", color="red", size=1.5) +
  geom_text(aes(x=46, label="High School State Testing / State Test Preliminary Results Released (Math 3rd-8th)", y=96), colour="black", angle=90, vjust = 1.2, text=element_text(size=7)) +
  
  geom_vline(xintercept=47, linetype="dotted", color="red", size=1.5) +
  geom_text(aes(x=47, label="Last Day of School", y=115), colour="black", angle=90, vjust = 1.2, text=element_text(size=7))

p

##Links/sources 
##FOR CREATING FILE WITH MISSING DATES TO ZERO
##https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-#exploratory-79f2a321e6b5
