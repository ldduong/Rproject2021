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
                         paste(dirToCompile, "compiled.csv"), sep="/"), row.names = FALSE)
  }
}
#

##Summarize the compiled data set in terms of number of screens run, percent of
##patients screened that were infected, M vs F patients, and the age
##distribution of patients.