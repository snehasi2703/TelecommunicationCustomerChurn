
library(unbalanced)
dataPreparation <- function(sqlSettings, trainTable, testTable) {
    sqlConnectionString <- sqlSettings$connString


    dataVars <- rxGetVarNames(cdrSQL)
    dataVars <- dataVars[!dataVars %in% c("year", "month")]
    dataVars <- paste(dataVars, collapse = ", ")
    dataQuery <- paste("select", dataVars, "from", inputTable)
    
    #Create tables
    inputDataSQL = RxSqlServerData(sqlQuery = dataQuery, 
                                   connectionString = sqlConnectionString, 
                                   colInfo = cdrColInfo)
    trainDataSQL <- RxSqlServerData(connectionString = sqlConnectionString,
                                   table = trainTable,
                                   colInfo = cdrColInfo)
    testDataSQL <- RxSqlServerData(connectionString = sqlConnectionString,
                                   table = testTable,
                                   colInfo = cdrColInfo)

    rxExec(preProcess, inData = inputDataSQL, outData1 = trainDataSQL, outData2 = testDataSQL)
}

preProcess <- function(inData, outData1, outData2) {
    customerDetailsDf <- rxDataStep(inData = inData,
                        removeMissings = TRUE,
                        overwrite = TRUE)
    customerDetailsDf <- customerDetailsDf[!duplicated(customerDetailsDf),]

    # Splitting data into test and training set
    set.seed(1234)
    splitFile <- rxSplit(inData = customerDetailsDf,
                         outFilesBase = "trainTestData",
                         splitByFactor = "ind",
                         transforms = list(ind = factor(sample(0:1, size = .rxNumRows, replace = TRUE, prob = c(0.3, 0.7)),
                                                       levels = 0:1,
                                                       labels = c("Test", "Train"))),
                         overwrite = TRUE)
    trainFile <- splitFile[[2]]
    testFile <- splitFile[[1]]
    
    trainDF <- rxDataStep(inData = trainFile, varsToDrop = c("ind"))
    testDF <- rxDataStep(inData = testFile, varsToDrop = c("ind"))

    
    trainVars <- names(trainDF)
    trainVarsInd <- trainVars %in% c("churn")
    smotetrain <- ubSMOTE(X = trainDF[!trainVarsInd], Y = trainDF$churn,
                      perc.over = 200, perc.under = 500,
                      k = 3, verbose = TRUE)
    smotetrainDF <- cbind(smotetrain$X, smotetrain$Y)
    names(smotetrainDF)[names(smotetrainDF) == "smotetrain$Y"] <- "churn"
    trainDF <- smotetrainDF
  

    rxDataStep(inData = trainDF, outFile = outData1, overwrite = TRUE)
    rxDataStep(inData = testDF, outFile = outData2, overwrite = TRUE)
}
