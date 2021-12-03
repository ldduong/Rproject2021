###R Project, Biocomputing Fall 2021
###Loan Duong
###Second script: Uses the source function to load the functions defined in
###first script, compiles all data into a CSV file, process the data included in
###the entire data set to answer the questions, and provide graphical evidence.

##Load functions from supportingFunctions.R
source("supportingFunctions.R")

##Compiles allData.csv file
CSVFunction()
CompileCSVFunction("CountryX")
CompileCSVFunction("CountryY")
allDataFunction()

##Process the data
summary()

##Answer the questions
cat("Question 1. In what country did the outbreak likely begin?")
cat("\n")
cat("The disease likely began in Country X. Plotting positive tests over time shows that it first appeared in Country X on day 120, and steadily grew while it didn't appear until day 139 in Country Y.")
cat("\n")
cat("This is visualized in Figure 1.")
XvYPlot()
