##COVID-19 Data Pull, Aggregation, and Plotting Code##
##Code written by Jason Gibbs using the below listed packages and data from the Johns Hopkins CSSE GitHub page at https://github.com/CSSEGISandData/COVID-19 (COVID-19 cases and deaths data) and the CDC at https://covid.cdc.gov/covid-data-tracker/#vaccinations (vaccine data).##
##https://github.com/jasondouglasgibbs or jasondouglasgibbs@gmail.com##

##You must have Orca properly installed from https://github.com/plotly/orca ##
##You must have Java JDK installed from https://www.oracle.com/java/technologies/javase-jdk15-downloads.html ##
##Download Chrome Driver for version 87.0.4280.88 --> https://chromedriver.storage.googleapis.com/index.html?path=87.0.4280.88/ ##

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
library(RSelenium)
library(netstat)
library(data.table)

##Set working and default download directory for personal laptop.##
##Default download directory required to automatically pull CDC vaccine data.##
#setwd("D:\\Users\\fight\\Documents\\COVID19 Code")
#downloadwd<-"D:\\Users\\fight\\Downloads"


##Set working and default download directory for personal laptop.##
##Default download directory required to automatically pull CDC vaccine data.##
setwd("C:\\Users\\fight\\Documents\\COVID-19 R File")
downloadwd<-"C:\\Users\\fight\\Downloads"


##Sets a variable for the working directory for use at the end of the script##
wd<-getwd()

#########################Case and Death Data Pulling and Summarizing##################################

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


#################################Vaccines###################################################

##Tne below code from "options(warn=-1)" to remDr$close() is used to pull CDC vaccination data automatically from their website.##
##The website currently does not offer a direct link to download the CSV file, so the below code uses the RSelenium package to open##
##an instance of the Chrome web browser and automatically navigate to and download the CSV.##
##Ensure you have the correct version of Chrome driver installed (see top comments for link)##

##Creates a space (by deleting any previously downloaded copies) for the .csv file in case the user has previously downloaded it##
options(warn=-1)
fullpath<-file.path(downloadwd,'covid19_vaccinations_in_the_united_states.csv')
file.remove(fullpath)
options(warn=0)

##RSelenium code to open the Chrome browser.##
rD<-rsDriver(browser="chrome", chromever = "87.0.4280.88", port=netstat::free_port())
remDr <- rD$client
remDr$navigate("https://covid.cdc.gov/covid-data-tracker/#vaccinations")
##Sleep time to allow web page time to load.##
Sys.sleep(5)
##Expands the "COVID-19 Vaccinations in the United States tab.##
webElem<-remDr$findElement(using='id',value='vaccinations-table-header-icon')
webElem$highlightElement()
webElem$clickElement()
##Clicks the button to download the CDC's .csv file for vaccinations.##
webElem<-remDr$findElement(using='id', value='btnVaccinationsExport')
webElem$highlightElement()
webElem$clickElement()
Sys.sleep(10)

COVID_US_Vaccines_Data_Original<-read_csv(fullpath, skip=3)
COVID_US_Vaccines_Data_Working<-COVID_US_Vaccines_Data_Original[order(COVID_US_Vaccines_Data_Original$`State/Territory/Federal Entity`),]


Sys.sleep(5)
remDr$close()

##Summarize Total Vaccines Administered##
COVID_US_Vaccines_Data_Working[,4:5]<-sapply(COVID_US_Vaccines_Data_Working[,4:5], as.numeric)
COVID_US_Vaccines_Data_Working[is.na(COVID_US_Vaccines_Data_Working)] <- 0
USTotalVaccineAdmin<-as.numeric(sum(COVID_US_Vaccines_Data_Working$`Total Administered`))
USTotalVaccineAdminString<-comma_format()(USTotalVaccineAdmin)

##Percent of Population
COVID_US_Vaccines_Data_Working$Proportion<-COVID_US_Vaccines_Data_Working$`Administered per 100K`/100000
COVID_State_Vaccine_Proportion<-COVID_US_Vaccines_Data_Working[COVID_US_Vaccines_Data_Working$Proportion!=0,]

##Vaccine Plots##
##Doses Administered - Raw Numbers##
StateVaccineAdminPlot<-ggplot(COVID_US_Vaccines_Data_Working, aes(x=`State/Territory/Federal Entity`, y=`Total Administered`, color=`State/Territory/Federal Entity`, fill=`State/Territory/Federal Entity`))+geom_bar(stat='identity')+labs(y="Recipients", title="Number of Recipients of COVID-19 Vaccine")+scale_y_continuous(labels=comma)+theme(legend.position = "bottom")+theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
ggplotly(StateVaccineAdminPlot)

##Doses Administered - Percent of Total Population##
StateVaccineAdminPercentPlot<-ggplot(COVID_State_Vaccine_Proportion, aes(x=`State/Territory/Federal Entity`, y=Proportion, color=`State/Territory/Federal Entity`, fill=`State/Territory/Federal Entity`))+geom_bar(stat='identity')+labs(y="Percent Total Population Received Dose", title="Percent Population Administered COVID-19 Vaccine by State")+scale_y_continuous(labels = scales::percent)+theme(legend.position = "bottom")+theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
ggplotly(StateVaccineAdminPercentPlot)

################################Outputs###################################################
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
orca(StateVaccineAdminPlot, "VaccineAdmin.png")
orca(StateVaccineAdminPercentPlot, 'VaccineAdminPercent.png')

##Saves a copy of pertinent data and data frames as .csv files for today within your working directory##
write_csv(NewCaseAggregatedByStateDFLong, "NewCasesPerDayByState.csv")
write_csv(NewDeathAggregatedByStateDFLong, "NewDeathsPerDayByState.csv")
write_csv(USTotalNewCasesByDateDF, "USNewCasesPerDay.csv")
write_csv(USTotalNewDeathsByDateDF, "USNewDeathsPerDay.csv")
write_csv(COVID_US_Cases_Data_Original, "CasesOriginalData.csv")
write_csv(COVID_US_Deaths_Data_Original, "DeathsOriginalData.csv")
write_csv(COVID_US_Vaccines_Data_Original, "VaccineOriginalData.csv")


##Returns to original working directory##
setwd(wd)

##Table displaying information also captured in the sprintf statements below##
COVIDCaseString_Table<-"Cummulative COVID-19 Cases"
COVIDDeathString_Table<-"Cummulative COVID-19 Deaths"
COVIDVaccineString_Table<-"Vaccines Administered"
Values_Table<-matrix(c(TotalCasesAggregated, TotalDeathsAggregated, USTotalVaccineAdmin), ncol=3, byrow=TRUE)
colnames(Values_Table)<-c(COVIDCaseString_Table,COVIDDeathString_Table,COVIDVaccineString_Table)
Values_Table<-data.frame(Values_Table, check.names=FALSE)
Values_Table %>% mutate(`Cummulative COVID-19 Cases`=formatC(`Cummulative COVID-19 Cases`, format='f', big.mark=',', drop0trailing = TRUE), `Cummulative COVID-19 Deaths`=formatC(`Cummulative COVID-19 Deaths`, format='f', big.mark=',', drop0trailing = TRUE), `Vaccines Administered`=formatC(`Vaccines Administered`, format='f', big.mark=',', drop0trailing = TRUE)) %>% data.table()


##Prints a number of descriptive statistics to the console##
sprintf("The total number of COVID-19 cases in the United States from 2020-01-22 through %s is %s.", yesterday, USTotalCasesString)
sprintf("The total number of COVID-19 deaths in the United States from 2020-01-22 through %s is %s.", yesterday, USTotalDeathsString)
sprintf("The number of new COVID-19 cases reported in the United States on %s is %s.", yesterday, USNewCasesString)
sprintf("The number of new COVID-19 deaths reported in the United States on %s is %s.", yesterday, USNewDeathsString)
sprintf("The number of people in the US that have received the COVID-19 vaccine is %s.", USTotalVaccineAdminString)
