##COVID-19 Data Pull, Aggregation, and Plotting Code for Mobile##
##Designed to provide pertinent outputs on mobile devices. Requires an app like R Compiler on Android##
##Code written by Jason Gibbs using the below listed packages and data from the Johns Hopkins CSSE GitHub page.##
##https://github.com/jasondouglasgibbs or jasondouglasgibbs@gmail.com##

library(tidyverse)
library(tidyselect)
library(lubridate)
library(glue)
library(scales)
library(dplyr)
library(tidyr)

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

#########################By State Graphs##################################

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

##Create Long Formatted Cases by State Over Time##
CaseAggregatedByStateDF<-as.data.frame(CaseAggregatedByState)
CaseStates<-row.names(CaseAggregatedByStateDF)
CaseAggregatedByStateDF$State<-CaseStates
##Gather command below changes data from wide to long format##
CaseAggregatedByStateDFLong<- CaseAggregatedByStateDF %>% gather(Date, Case,-State)

##Create Long Formatted Deaths By State Over Time ##
DeathAggregatedByStateDF<-as.data.frame(DeathAggregatedByState)
DeathStates<-row.names(DeathAggregatedByStateDF)
DeathAggregatedByStateDF$State<-DeathStates
##Gather command below changes data from wide to long format to enable using ggplot and plotly##
DeathAggregatedByStateDFLong<- DeathAggregatedByStateDF %>% gather(Date, Death,-State)

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

##Prints tables with the cummulative number of cases and deaths by State##
print(StateCases, n=nrow(StateCases))
print(StateDeaths, n=nrow(StateDeaths))

###Prints a number of descriptive statistics to the console##
sprintf("The total number of COVID-19 cases in the United States from 2020-01-22 through %s is %s.", yesterday, USTotalCasesString)
sprintf("The total number of COVID-19 deaths in the United States from 2020-01-22 through %s is %s.", yesterday, USTotalDeathsString)
sprintf("The number of new COVID-19 cases reported in the United States on %s is %s.", yesterday, USNewCasesString)
sprintf("The number of new COVID-19 deaths reported in the United States on %s is %s.", yesterday, USNewDeathsString)