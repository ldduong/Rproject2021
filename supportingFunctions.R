###R Project, Biocomputing Fall 2021
###Loan Duong
###First script: a number of custom functions for data handling or summary tasks

##Convert all files in a directory with space- or tab-delimited data into csv
#files. The function name is CSV function and will accept the name of a
#directory that contains .txt files. It defaults to the CountryY directory.
CSVFunction<-function(TXTDir="countryY"){
TXTfiles <- list.files(TXTDir, pattern = ".txt")
TXTfilesIn <- paste(TXTDir, TXTfiles, sep="/") 
CSVfilesOut <- paste(TXTDir, paste0(sub(".txt","", TXTfiles),".csv"), sep="/")
for (i in 1:length(TXTfilesIn)) {
  temp <- (read.table(TXTfilesIn[i], header = TRUE, sep=" "))
  write.csv(temp, file = CSVfilesOut[i], row.names = FALSE)
  }
}

##Compile data from all csv files in a directory into a single csv file,
##maintaining original 10 columns as well as country & day of year columns. This
##function accepts a directory name and will create a compilation of all the csv
##files within that directory, creating a new csv within the same directory.
CompileCSVFunction<-function(dirToCompile){
  file.list<-list.files(dirToCompile, pattern = ".csv")
  initDataFrame<-data.frame()
  CSVtoRead <- paste(dirToCompile, file.list, sep="/")
  for (i in 1:length(file.list)){
    readIn<-read.csv(CSVtoRead[i],header=TRUE)
    readIn$country = substr(dirToCompile,8,8)
    readIn$day = substr(CSVtoRead[i],17,19)
    initDataFrame<-rbind(initDataFrame,readIn)
    write.csv(initDataFrame,
              file=paste(dirToCompile,
                         paste(dirToCompile,"compiled.csv"), sep="/"), row.names = FALSE)
  }
}
#Code to compile the compiled data from country's x and y into a single file, allData.csv
allDataFunction<-function(){
  X<-read.csv("countryX/countryX compiled.csv")
  Y<-read.csv("countryY/countryY compiled.csv")
  allData<-rbind(X, Y)
  write.csv(allData, file="allData.csv", row.names = FALSE)
}
#Allow the user to choose whether they want to remove rows with NA's in any
#columns, include NAs in compiled data, but be warned of their presence, or
#include NAs in compiled data without a warning
AllDataNA<-function(){
allData<-read.csv("allData.csv")
allData[allData==0]<-NA
NAsChoice<-readline(prompt="Remove rows with any NAs? Type Y or N. ")
if(NAsChoice=="N"){
  cat(paste("Rows with NAs have been preserved. See allData.csv."))
}else{
  allData_omit<-na.omit(allData)
  write.csv(allData_omit, file="allData_omit.csv", row.names=FALSE)
  cat(paste("Rows with NAs have been omitted. See allData_omit.csv."))
  }
}
##Summarize the compiled data set in terms of number of screens run, percent of
##patients screened that were infected, M vs F patients, and the age
##distribution of patients.
summary<-function(){
  allData<-read.csv("allData.csv")
  numScreens<-nrow(allData)
  positiveCasesOnly<-allData[rowSums(is.na(allData[,3:12]))!=10,]
  positiveCases<-nrow(positiveCasesOnly)
  percentInfected<-round((positiveCases/numScreens*100), digits=2)
  cat(paste("Total number of screens:"), numScreens)
  cat("\n")
  cat(paste("Percent positive cases:"), percentInfected, "%")
  cat("\n")
  MalePts<-sum(allData$gender=="male")
  FemalePts<-sum(allData$gender=="female")
  cat(paste("Number of Male Patients Screened:"), MalePts)
  cat("\n")
  cat(paste("Number of Female Patients Screened:"), FemalePts)
  cat("\n")
  options(warn=-1)
  library(ggplot2)
  ageDistribution1<-ggplot(allData,aes(x=age))+
    geom_histogram(bins=15, binwidth = 3, color="blue", fill="white")+
    ggtitle("Age Distribution of Screened Patients",)+
    ylab("Number of patients")+
    scale_x_continuous(breaks=seq(0,120,10), limits=c(0,120))+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5))
  ageDistribution2<-ggplot(positiveCasesOnly,aes(x=age))+
    geom_histogram(bins=15, binwidth = 3, color="red", fill="white")+
    ggtitle("Age Distribution of Patients Tested Positive",)+
    ylab("Number of patients")+
    scale_x_continuous(breaks=seq(0,120,10), limits=c(0,120))+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5))
  library(cowplot)
  ageDistribution<-plot_grid(ageDistribution1, ageDistribution2,
                             rel_widths = c(1, 0.85),
                             ncol = 2,
                             nrow = 1)
}
summary()
