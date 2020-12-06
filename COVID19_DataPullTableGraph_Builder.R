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
#Set working directory for personal laptop#
#setwd("D:\\Users\\fight\\Documents\\COVID19 Code")

#Set working directory for work laptop#
#setwd("C:\\Users\\jason.d.gibbs1\\Desktop\\COVID-19 R")

#Set working directory for desktop computer#
setwd("C:\\Users\\fight\\Documents\\COVID-19 R File")

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
sprintf("The total number of COVID-19 cases in the United States from 2020-01-22 through %s is %s.", yesterday, USTotalCasesString)


##Summarize Deaths##
COVID_US_Deaths_Data$Total<-COVID_US_Deaths_Data[,ncol(COVID_US_Deaths_Data)]
StateDeaths<-COVID_US_Deaths_Data %>% group_by(Province_State) %>% summarize(sum=sum(Total))
USTotalDeaths<-as.numeric(sum(COVID_US_Deaths_Data$Total))
USTotalDeathsString<-comma_format()(USTotalDeaths)
sprintf("The total number of COVID-19 deaths in the United States from 2020-01-22 through %s is %s.", yesterday, USTotalDeathsString)


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


#########################US Total Graphs##################################

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


##Outputs##

#Saves a copy of the plots to your working directory. You must have Orca correctly installed, see: https://plotly.com/r/static-image-export/ ##

orca(USDeathsPlot, "USDeathsPlot.png")
orca(USCasePlot,"USCasePlot.png")
