
#working Directory
working.Directory <- "C:\\Users\\singh\\Documents\\advanceschema\\telco-customer-churn-v1"

authenticationType <- "Windows" 
serverName <- "LAPTOP-3F541P5H"
schema <- "telcoedw"


	
#Source function scripts

source(file.path(working.Directory, "R", "setUp.R"))
source(file.path(working.Directory, "R", "evaluate.R"))
source(file.path(working.Directory, "R", "dataExploration.R"))
source(file.path(working.Directory, "R", "dataPreparation.R"))
source(file.path(working.Directory, "R", "trainModel.R"))

if (authenticationType == "Windows") {
    sqlConnString <- paste("Driver=SQL Server;Server=", serverName, ";schema=", schema, ";trusted_connection=true", sep = "")
} else if (authenticationType == "SQL") { sqlConnString <- paste("Driver=SQL Server;Server=", serverName, ";schema=", schema, ";Uid=", user, ";Pworking.Directory=", password, sep = "") }

sqlCompute <- RxInSqlServer(connectionString = sqlConnString)

sqlSettings <- vector("list")
sqlSettings$connString <- sqlConnString

## Load data into SQL tables

rxSetComputeContext('local')
cdrTable <- "edw_cdr"

cdrFile <- RxTextData(file.path(working.Directory, "Data", "edw_cdr.csv"))

cdrColInfo <- list(age = list(type = "integer"),
                 annualincome = list(type = "integer"),
                 calldroprate = list(type = "numeric"),
                 callfailurerate = list(type = "numeric"),
                 callingnum = list(type = "numeric"),
                 customerid = list(type = "integer"),
                 customersuspended = list(type = "factor", levels = c("No", "Yes")),
                 education = list(type = "factor", levels = c("Bachelor or equivalent", "High School or below", "Master or equivalent", "PhD or equivalent")),
                 gender = list(type = "factor", levels = c("Female", "Male")),
                 homeowner = list(type = "factor", levels = c("No", "Yes")),
                 maritalstatus = list(type = "factor", levels = c("Married", "Single")),
                 monthlybilledamount = list(type = "integer"),
                 noadditionallines = list(type = "factor", levels = c("\\N")),
                 numberofcomplaints = list(type = "factor", levels = as.character(0:3)),
                 numberofmonthunpaid = list(type = "factor", levels = as.character(0:7)),
                 numdayscontractequipmentplanexpiring = list(type = "integer"),
                 occupation = list(type = "factor", levels = c("Non-technology Related Job", "Others", "Technology Related Job")),
                 penaltytoswitch = list(type = "integer"),
                 state = list(type = "factor"),
                 totalminsusedinlastmonth = list(type = "integer"),
                 unpaidbalance = list(type = "integer"),
                 usesinternetservice = list(type = "factor", levels = c("No", "Yes")),
                 usesvoiceservice = list(type = "factor", levels = c("No", "Yes")),
                 percentagecalloutsidenetwork = list(type = "numeric"),
                 totalcallduration = list(type = "integer"),
                 avgcallduration = list(type = "integer"),
                 churn = list(type = "factor", levels = as.character(0:1)),
                 year = list(type = "factor", levels = as.character(2015)),
                 month = list(type = "factor", levels = as.character(1:3)))

cdrSQL <- RxSqlServerData(table = cdrTable,
                          connectionString = sqlConnString,
                          colInfo = cdrColInfo)

rxDataStep(inData = cdrFile, outFile = cdrSQL, overwrite = TRUE)

inputTable <- cdrTable
trainTable <- "edw_cdr_train"
testTable <- "edw_cdr_test"
predTable <- "edw_cdr_pred"

system.time({
    dataPreparation(sqlSettings, trainTable, testTable)
})

trainingDataTable <- RxSqlServerData(connectionString = sqlConnString,
                                   table = trainTable,
                                   colInfo = cdrColInfo)
testingDataTable <- RxSqlServerData(connectionString = sqlConnString,
                                   table = testTable,
                                   colInfo = cdrColInfo)
rxGetInfo(trainingDataTable, getVarInfo = T)
rxGetInfo(testingDataTable, getVarInfo = T)
rxSummary( ~ churn, data = trainingDataTable)
rxSummary( ~ churn, data = testingDataTable)


rxSetComputeContext(sqlCompute)

system.time({
    trainModel(sqlSettings, trainTable)
})


#Data exploration and visualization

shinyApp(ui, server)





