#Load libraries
library(colorspace)
library (ggplot2)
library (dplyr)
library (tidyr)
library (reshape2)
library(circlize)
library(readxl)
library(kableExtra)
library(lubridate)
library(plotly)
library(hms)

## read file (read.excel)
data <- read_excel("/Users/file.xlsx", sheet = "sheet1") 

##Some filtering, selecting, grouping, summarizing, mutating options
sessions <- data %>%
  #select(Report_Type, elapsTimeSec) %>%
  filter(!is.na(COGIPF_SESSIONID)) %>%
  group_by(COGIPF_SESSIONID, StaffCodeName) %>%
  summarize(
    totalSeconds = sum(actionSeconds),
    totalAction = n()) %>%
  mutate(
    totMin = totalSeconds/60) %>%
  ungroup() %>%
  ##convert missing data to "0"
  complete(COGIPF_SESSIONID, fill = list(totalSeconds = 0, totalAction = 0, totMin = 0)
           
##Other Mutate options for creating variables
       #percActions = totalAction/4502*100,
       #totalhh_mm_ss = round_hms(as.hms(sum(elapsTimeSec)), 1),
       #percDuration = totalSeconds/321237*100,
       #aveDuration = round_hms(as.hms(mean(elapsTimeSec)), 1),
       #SDDuration = round_hms(as.hms(sd(elapsTimeSec)), 1),
       #minDuration = round_hms(as.hms(min(elapsTimeSec)), 1),
       #quart1Duration = round_hms(as.hms(quantile(elapsTimeSec, 0.25)), 1),
       #medDuration = round_hms(as.hms(median(elapsTimeSec)), 1),
       #quart3Duration = round_hms(as.hms(quantile(elapsTimeSec, 0.75)), 1),
       #maxDuration = round_hms(as.hms(max(elapsTimeSec)), 1)
       #durRepSum = as.duration(sum(elapsTimeSec))
       #Average = mean(elapsTimeSec),
       #SD = sd(elapsTimeSec),
       #Minimum = min(elapsTimeSec),
       #firstQuart = quantile(elapsTimeSec, 0.25),
       #Median = median(elapsTimeSec),
       #thirdQuart = quantile (elapsTimeSec, 0.75),
       #Maximum = max(elapsTimeSec)
       #durSumPer = seconds_to_period(repSum), Does not work with dplyr. Class 4 object
       #formDurSum = sprintf("%1.0fD %1.0fH %1.0fM %1.0fS", day(durSum), durSum@hour, minute(durSum), second(durSum)), 
  )

dim(sessions)

##Create histogram of all sessions
hist <- ggplot(sessions, aes(x=totMin)) + 
        geom_histogram(binwidth = 2) + 
  ##Add mean and median lines to graph
        geom_vline(aes(xintercept=mean(totMin)), color = "blue",linetype = "dotted", size = 1) +
        geom_vline(aes(xintercept=median(totMin)), color = "red",linetype = "dotted", size = 1) +
  ##label mean and median lines
        geom_text(aes(x=mean(totMin), label="Mean (14.6)", y=3000), colour="blue", angle=90, vjust = 1.2,
            text=element_text(size=9.5))+
        geom_text(aes(x=median(totMin), label="Median (3.0)", y=3000), colour="red", angle=90, vjust = -1,
            text=element_text(size=9.5)) +
        theme_light() +
  #geom_density(alpha = .2, fill = "red") +
        labs(title = "Figure 1: Histogram of Session Length", 
        subtitle = "N=TK",
        x = "Session Duration (In two-minute intervals)", 
        y = "Number of Sessions",
        caption = "TBA")

hist

##Links/Sources
#http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization