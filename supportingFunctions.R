###R Project, Biocomputing Fall 2021
###Loan Duong
###First script: a number of custom functions for data handling or summary tasks

##Convert all files in a directory with space- or tab-delimited data into csv
#files. The function name is CSV function and will accept a directory name. It
#defaults to the CountryY directory.
CSVFunction<-function(directory="TXTDir"){
TXTDir <- "countryY"
dir.create("countryY/CSVs")
CSVs <- "countryY/CSVs"
TXTfiles <- list.files(TXTDir, pattern = ".txt")
TXTfilesIn <- paste(TXTDir, TXTfiles, sep="/") 
CSVfilesOut <- paste(CSVs, paste0(sub(".txt","", TXTfiles),".csv"), sep="/")
for (i in 1:length(TXTfilesIn)) {
  temp <- (read.table(TXTfilesIn[i], header = TRUE, sep=" "))
  write.csv(temp, file = CSVfilesOut[i], row.names = FALSE)
  }
}
#Running the function for each Country. 
CSVFunction()
CSVFunction(countryX)
##Compile data from all csv files in a directory into a single csv file,
##maintaining original 10 columns as well as country & day of year columns.
##Allow user to remove rows with NA's in any of the columns, include NA's in the
##compiled data but be warned of their presence, or include NA's in the data
##without a warning.

##Summarize the compiled data set in terms of number of screens run, percent of
##patients screened that were infected, M vs F patients, and the age
##distribution of patients.