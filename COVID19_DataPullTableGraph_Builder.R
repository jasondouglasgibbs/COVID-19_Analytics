##COVID-19 Data Pull, Aggregation, and Plotting Code##
##Code written by Jason Gibbs using the below listed packages and data from the Johns Hopkins CSSE GitHub page.##
##https://github.com/jasondouglasgibbs or jasondouglasgibbs@gmail.com##
library(tidyverse)
library(tidyselect)
library(lubridate)
library(glue)
library(scales)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(orca)
library(processx)
##Set working directory for personal laptop##
##setwd("D:\\Users\\fight\\Documents\\COVID19 Code")##

##Set working directory for work laptop##
##setwd("C:\\Users\\jason.d.gibbs1\\Desktop\\COVID-19 R")##

##Set working directory for desktop computer##
setwd("C:\\Users\\fight\\Documents\\COVID-19 R File")

##Sets a variable for the working directory for use at the end of the script##
wd<-getwd()
##US Confirmed Cases##
##Pulls data from Johns Hopkins CSSE GitHub at the below link, turns into a TidyVerse tibble##
URL_Cases<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
COVID_US_Cases_Data<-read_csv(URL_Cases)
COVID_US_Cases_Data_Original<-COVID_US_Cases_Data

##US Deaths##
##Pulls data from Johns Hopkins CSSE GitHub at the below link, turns into a TidyVerse tibble##
URL_Deaths<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
COVID_US_Deaths_Data<-read_csv(URL_Deaths)
COVID_US_Deaths_Data_Original<-COVID_US_Deaths_Data


##Establish date for use is sprintf commands##
yesterday<-today()-1

##Summarize Cases##
COVID_US_Cases_Data$Total<-COVID_US_Cases_Data[,ncol(COVID_US_Cases_Data)]
StateCases<-COVID_US_Cases_Data %>% group_by(Province_State) %>% summarize(sum=sum(Total))
USTotalCases<-as.numeric(sum(COVID_US_Cases_Data$Total))
USTotalCasesString<-comma_format()(USTotalCases)


##Summarize Deaths##
COVID_US_Deaths_Data$Total<-COVID_US_Deaths_Data[,ncol(COVID_US_Deaths_Data)]
StateDeaths<-COVID_US_Deaths_Data %>% group_by(Province_State) %>% summarize(sum=sum(Total))
USTotalDeaths<-as.numeric(sum(COVID_US_Deaths_Data$Total))
USTotalDeathsString<-comma_format()(USTotalDeaths)


#########################By State Graphs and Tables##################################

##Group Cases by State##
CaseColumns<-ncol(COVID_US_Cases_Data_Original)
CaseAggregatedByState<-rowsum(COVID_US_Cases_Data_Original[,c(12:CaseColumns)],COVID_US_Cases_Data$Province_State)
TotalCasesAggregated<-sum(CaseAggregatedByState[ncol(CaseAggregatedByState)])
TotalCasesAggregated==USTotalCases

##Group Deaths by State##
DeathsColumns<-ncol(COVID_US_Deaths_Data_Original)
DeathAggregatedByState<-rowsum(COVID_US_Deaths_Data_Original[,c(13:DeathsColumns)],COVID_US_Deaths_Data$Province_State)
TotalDeathsAggregated<-sum(DeathAggregatedByState[ncol(DeathAggregatedByState)])
TotalDeathsAggregated==USTotalDeaths


##Create Cases by State Over Time Plot##
CaseAggregatedByStateDF<-as.data.frame(CaseAggregatedByState)
CaseStates<-row.names(CaseAggregatedByStateDF)
CaseAggregatedByStateDF$State<-CaseStates
##Gather command below changes data from wide to long format to enable using ggplot and plotly##
CaseAggregatedByStateDFLong<- CaseAggregatedByStateDF %>% gather(Date, Case,-State)
CaseAggregatedByStateDFLong$Dates<-as.Date(CaseAggregatedByStateDFLong$Date, format = "%m/%d/%y")
CasePlot<-ggplot(CaseAggregatedByStateDFLong, aes(x=Dates, y=Case, group=State, color=State))+geom_point()+labs(x="Month",y="Cummulative Cases", title="Total Number of COVID-19 Cases by State Over Time")+scale_y_continuous(labels=comma)
ggplotly(CasePlot)



##Create Deaths By State Over Time Plot##
DeathAggregatedByStateDF<-as.data.frame(DeathAggregatedByState)
DeathStates<-row.names(DeathAggregatedByStateDF)
DeathAggregatedByStateDF$State<-DeathStates
##Gather command below changes data from wide to long format to enable using ggplot and plotly##
DeathAggregatedByStateDFLong<- DeathAggregatedByStateDF %>% gather(Date, Death,-State)
DeathAggregatedByStateDFLong$Dates<-as.Date(DeathAggregatedByStateDFLong$Date, format = "%m/%d/%y")
DeathPlot<-ggplot(DeathAggregatedByStateDFLong, aes(x=Dates, y=Death, group=State, color=State))+geom_point()+labs(x="Month",y="Cummulative Deaths", title="Total Number of COVID-19 Deaths by State Over Time")+scale_y_continuous(labels=comma)
ggplotly(DeathPlot)


#######################New Cases/Deaths Per Day###############################

##This code uses two "for" loops to iterate through the cummulative COVID-19 data##
## from Johns Hopkins. "i" corresponds to rows (IE: looping through each state on##
## a given day). "j" corresponds to columns (IE: looping through each day). It is assumed##
## that column one's values constitute "new" values for purposes of this analysis, that is##
## why "j" starts at "2".##

##Cases##
NewCasesPerDay<-CaseAggregatedByState
CasePerDayCalculate<-CaseAggregatedByState
CasePerDayRows<-nrow(NewCasesPerDay)
CasePerDayColumns<-ncol(NewCasesPerDay)
for (j in 2:CasePerDayColumns){
  for (i in 1:CasePerDayRows){
    NewCasesPerDay[i,j]=abs(CasePerDayCalculate[i,j]-CasePerDayCalculate[i,j-1])
    next}
  next  
}

NewCasesPerDayOriginal<-NewCasesPerDay

USNewCases<-as.numeric(sum(NewCasesPerDay[,CasePerDayColumns]))
USNewCasesString<-comma_format()(USNewCases)


##Deaths##
NewDeathsPerDay<-DeathAggregatedByState
DeathPerDayCalculate<-DeathAggregatedByState
DeathPerDayRows<-nrow(NewDeathsPerDay)
DeathPerDayColumns<-ncol(NewDeathsPerDay)
for (j in 2:DeathPerDayColumns){
  for (i in 1:DeathPerDayRows){
    NewDeathsPerDay[i,j]=abs(DeathPerDayCalculate[i,j]-DeathPerDayCalculate[i,j-1])
    next}
  next  
}

NewDeathsPerDayOriginal<-NewDeathsPerDay

USNewDeaths<-as.numeric(sum(NewDeathsPerDay[,DeathPerDayColumns]))
USNewDeathsString<-comma_format()(USNewDeaths)

##New State Cases/Deaths Graphs##
##Cases##
CasePerDayStates<-row.names(NewCasesPerDay)
NewCasesPerDay$State<-CasePerDayStates
##Gather command below changes data from wide to long format to enable using ggplot and plotly##
NewCaseAggregatedByStateDFLong<- NewCasesPerDay %>% gather(Date, Case,-State)
NewCaseAggregatedByStateDFLong$Dates<-as.Date(NewCaseAggregatedByStateDFLong$Date, format = "%m/%d/%y")
NewCasePlot<-ggplot(NewCaseAggregatedByStateDFLong, aes(x=Dates, y=Case, group=State, color=State))+geom_point()+labs(x="Month",y="New Cases", title="Number of new cases of COVID-19 by State Over Time")+scale_y_continuous(labels=comma)
ggplotly(NewCasePlot)

##Deaths##
DeathPerDayStates<-row.names(NewDeathsPerDay)
NewDeathsPerDay$State<-DeathPerDayStates
##Gather command below changes data from wide to long format to enable using ggplot and plotly##
NewDeathAggregatedByStateDFLong<- NewDeathsPerDay %>% gather(Date, Death,-State)
NewDeathAggregatedByStateDFLong$Dates<-as.Date(NewDeathAggregatedByStateDFLong$Date, format = "%m/%d/%y")
NewDeathPlot<-ggplot(NewDeathAggregatedByStateDFLong, aes(x=Dates, y=Death, group=State, color=State))+geom_point()+labs(x="Month",y="New Deaths", title="Number of new COVID-19 related deaths by State Over Time")+scale_y_continuous(labels=comma)
ggplotly(NewDeathPlot)

#########################US Total Graphs#####################################

##Cases##
USTotalCases<-COVID_US_Cases_Data_Original
CaseRowsUS<-nrow(USTotalCases)
CaseColumnsUS<-ncol(USTotalCases)
USTotalCasesByDate<-colSums(USTotalCases[,12:CaseColumnsUS])
USTotalCasesByDateDF<-as.data.frame(USTotalCasesByDate)
USTotalCasesByDateDF$Dates<-row.names(USTotalCasesByDateDF)
USTotalCasesByDateDF$Dates<-as.Date(USTotalCasesByDateDF$Dates, format = "%m/%d/%y")
USCasePlot<-ggplot(USTotalCasesByDateDF, aes(x=Dates, y=USTotalCasesByDate))+geom_point()+labs(x="Month",y="Cummulative Cases", title="Total Number of US COVID-19 Cases Over Time")+scale_y_continuous(labels=comma)
ggplotly(USCasePlot)

##Deaths##
USTotalDeaths<-COVID_US_Deaths_Data_Original
DeathRowsUS<-nrow(USTotalDeaths)
DeathColumnsUS<-ncol(USTotalDeaths)
USDeaths<-colSums(USTotalDeaths[,13:DeathColumnsUS])
USTotalDeathsByDateDF<-as.data.frame(USDeaths)
USTotalDeathsByDateDF$Dates<-row.names(USTotalDeathsByDateDF)
USTotalDeathsByDateDF$Dates<-as.Date(USTotalDeathsByDateDF$Dates, format = "%m/%d/%y")
USDeathsPlot<-ggplot(USTotalDeathsByDateDF, aes(x=Dates, y=USDeaths))+geom_point()+labs(x="Month",y="Cummulative Deaths", title="Total Number of US COVID-19 Deaths Over Time")+scale_y_continuous(labels=comma)
ggplotly(USDeathsPlot)


#########################US New Graphs#####################################
##Cases##
NewUSCasesWorking<-NewCasesPerDayOriginal
NewUSCaseRows<-nrow(NewUSCasesWorking)
NewUSCaseColumns<-ncol(NewUSCasesWorking)
USTotalNewCasesByDate<-colSums(NewUSCasesWorking[,1:NewUSCaseColumns])
USTotalNewCasesByDateDF<-as.data.frame(USTotalNewCasesByDate)
USTotalNewCasesByDateDF$Dates<-row.names(USTotalNewCasesByDateDF)
USTotalNewCasesByDateDF$Dates<-as.Date(USTotalNewCasesByDateDF$Dates, format = "%m/%d/%y")
USNewCasePlot<-ggplot(USTotalNewCasesByDateDF, aes(x=Dates, y=USTotalNewCasesByDate))+geom_point()+labs(x="Month",y="New Cases", title="Number of New US COVID-19 Cases Over Time")+scale_y_continuous(labels=comma)
ggplotly(USNewCasePlot)


##Deaths##
NewUSDeathsWorking<-NewDeathsPerDayOriginal
NewUSDeathRows<-nrow(NewUSDeathsWorking)
NewUSDeathColumns<-ncol(NewUSDeathsWorking)
USTotalNewDeathsByDate<-colSums(NewUSDeathsWorking[,1:NewUSDeathColumns])
USTotalNewDeathsByDateDF<-as.data.frame(USTotalNewDeathsByDate)
USTotalNewDeathsByDateDF$Dates<-row.names(USTotalNewDeathsByDateDF)
USTotalNewDeathsByDateDF$Dates<-as.Date(USTotalNewDeathsByDateDF$Dates, format = "%m/%d/%y")
USNewDeathPlot<-ggplot(USTotalNewDeathsByDateDF, aes(x=Dates, y=USTotalNewDeathsByDate))+geom_point()+labs(x="Month",y="New Deaths", title="Number of New US COVID-19 Deaths Over Time")+scale_y_continuous(labels=comma)
ggplotly(USNewDeathPlot)


#################################Outputs###################################################
##Create a Directory for Today##
DirectoryChar<-as.character(today())
dir.create(DirectoryChar, showWarnings = FALSE)
OutputWD<-file.path(wd,DirectoryChar)
setwd(OutputWD)

##Saves a copy of the plots to a folder for today within your working directory. You must have Orca correctly installed, see: https://plotly.com/r/static-image-export/ ##
orca(USDeathsPlot, "USDeathsPlot.png")
orca(USCasePlot,"USCasePlot.png")
orca(USNewCasePlot, "USNewCasePlot.png")
orca(USNewDeathPlot, "USNewDeathPlot.png")

##Saves a copy of pertinent data and data frames as .csv files for today within your working directory##
write_csv(NewCaseAggregatedByStateDFLong, "NewCasesPerDayByState.csv")
write_csv(NewDeathAggregatedByStateDFLong, "NewDeathsPerDayByState.csv")
write_csv(USTotalNewCasesByDateDF, "USNewCasesPerDay.csv")
write_csv(USTotalNewDeathsByDateDF, "USNewDeathsPerDay.csv")
write_csv(COVID_US_Cases_Data_Original, "CasesOriginalData.csv")
write_csv(COVID_US_Deaths_Data_Original, "DeathsOriginalData.csv")


##Returns to original working directory##
setwd(wd)


###Prints a number of descriptive statistics to the console##
sprintf("The total number of COVID-19 cases in the United States from 2020-01-22 through %s is %s.", yesterday, USTotalCasesString)
sprintf("The total number of COVID-19 deaths in the United States from 2020-01-22 through %s is %s.", yesterday, USTotalDeathsString)
sprintf("The number of new COVID-19 cases reported in the United States on %s is %s.", yesterday, USNewCasesString)
sprintf("The number of new COVID-19 deaths reported in the United States on %s is %s.", yesterday, USNewDeathsString)