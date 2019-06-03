--Table to store Model
drop table if exists models;
go
create table models(
	model_name varchar(30) not null default('primary model') primary key,
	model varbinary(max) not null
);
go

--Building the predictive model
drop procedure if exists generate_random_forest;
go
create procedure generate_random_forest
as
begin
	execute sp_execute_external_script
	  @language = N'R'
	, @script = N'
		require("RevoScaleR");
        labelVar = "churn"
        trainVars <- rxGetVarNames(edw_cdr_train)
        trainVars <- trainVars[!trainVars %in% c(labelVar)]
        temp <- paste(c(labelVar, paste(trainVars, collapse = "+")), collapse = "~")
        formula <- as.formula(temp)
        rx_forest_model <- rxDForest(formula = formula,
                            data = edw_cdr_train,
                            nTree = 8,
                            maxDepth = 32,
                            mTry = 2,
                            minBucket=1,
                            replace = TRUE,
                            importance = TRUE,
                            seed=8,
                            parms=list(loss=c(0,4,1,0)))
		rxDForest_model <- data.frame(payload = as.raw(serialize(rx_forest_model, connection=NULL)));
'
	, @input_data_1 = N'select * from edw_cdr_train'
	, @input_data_1_name = N'edw_cdr_train'
	, @output_data_1_name = N'rxDForest_model'
	with result sets ((model varbinary(max)));
end;
go

--Update model
insert into models (model)
exec generate_random_forest;
update models set model_name = 'rxDForest' where model_name = 'default model';
select * from models;
go

-- Sp to make predictions
drop procedure if exists predict_cdr_churn_rx_forest;
go
create procedure predict_cdr_churn_rx_forest (@model varchar(100))
as
begin
	declare @rx_model varbinary(max) = (select model from models where model_name = @model);

	exec sp_execute_external_script 
					@language = N'R'
				  , @script = N'
    require("RevoScaleR");
    cdr_model<-unserialize(rx_model);
    predictions <- rxPredict(modelObject = cdr_model,
                             data = edw_cdr_test,
						     type="prob",
                             overwrite = TRUE)
    predictions$X0_prob <- NULL
    predictions$churn_Pred <- NULL
    names(predictions) <- c("probability")
    predictions$prediction <- ifelse(predictions$probability > 0.5, 1, 0)
    predictions$prediction<- factor(predictions$prediction, levels = c(1, 0))
    edw_cdr_pred <- cbind(edw_cdr_test[,c("customerid","churn")],predictions)
    edw_cdr_pred<-as.data.frame(edw_cdr_pred);
'
	, @input_data_1 = N'
	select * from edw_cdr_test'
	, @input_data_1_name = N'edw_cdr_test'
	, @output_data_1_name=N'edw_cdr_pred'
	, @params = N'@rx_model varbinary(max)'
	, @rx_model = @rx_model
	with result sets ( ("customerid" int, "churn" varchar(255), "probability" float, "prediction" float)
			  );
end;
go

--Execute scoring procedure
drop table if exists edw_cdr_pred;
go
create table edw_cdr_pred(
customerid int,
churn varchar(255),
probability float,
prediction float
)
insert into edw_cdr_pred
exec predict_cdr_churn_rx_forest 'rxDForest';
go
select * from edw_cdr_pred

--SP to Evaluate model performance
drop procedure if exists model_evaluate;
go
create procedure model_evaluate
as
begin
	execute sp_execute_external_script
	  @language = N'R'
	, @script = N'
      evaluateModel <- function(data, observed, predicted) 
      {
      confusion <- table(data[[observed]], data[[predicted]])
      print(confusion)
      tp <- confusion[rownames(confusion) == 1, colnames(confusion) == 1]
      fn <- confusion[rownames(confusion) == 1, colnames(confusion) == 0]
      fp <- confusion[rownames(confusion) == 0, colnames(confusion) == 1]
      tn <- confusion[rownames(confusion) == 0, colnames(confusion) == 0]
      accuracy <- (tp + tn) / (tp + fn + fp + tn)
      precision <- tp / (tp + fp)
      recall <- tp / (tp + fn)
      fscore <- 2 * (precision * recall) / (precision + recall)
      metrics <- c("Accuracy" = accuracy,
                   "Precision" = precision,
                   "Recall" = recall,
                   "F-Score" = fscore)
      return(metrics)
      }

      metrics <- evaluateModel(data = edw_cdr_pred,
                               observed = "churn",
                               predicted = "prediction")
      print(metrics)
      metrics<-matrix(metrics,ncol=4)
      metrics<-as.data.frame(metrics);
'
	, @input_data_1 = N'
	select * from edw_cdr_pred'
	, @input_data_1_name = N'edw_cdr_pred'
	, @output_data_1_name = N'metrics'
	with result sets ( ("Accuracy" float, "Precision" float, "Recall" float, "F-Score" float)
			  );
end;
go

--Execute evaluating procedure
exec model_evaluate
go

--Create a stored procedure to generate roc curve 
drop procedure if exists model_roccurve;
go
create procedure model_roccurve
as
begin
	execute sp_execute_external_script
	  @language = N'R'
	, @script = N'
      require("RevoScaleR");
      rxrocCurve <- function(data, observed, predicted) 
	  {
      data <- data[, c(observed, predicted)]
      data[[observed]] <- as.numeric(as.character(data[[observed]]))
      rxRocCurve(actualVarName = observed,
                 predVarNames = predicted,
                 data = data)
      }

     # Open a jpeg file and output plot in that file.
     image_file = tempfile();
     jpeg(filename=image_file, width=800, height = 550);
     print(
     rxrocCurve(data = edw_cdr_pred,
                observed = "churn",
                predicted = "probability")
     );
     dev.off();
     OutputDataSet <- data.frame(data=readBin(file(image_file, "rb"), what=raw(), n=1e6));
' 
	, @input_data_1 = N'
	select * from edw_cdr_pred'
	, @input_data_1_name = N'edw_cdr_pred'
	with result sets ((plot varbinary(max)));
end;
go

exec model_roccurve
go


--Generate plots for visualization
--Create a SP to generate pie chart 
drop procedure if exists pie;
go
create procedure pie
as
begin
exec sp_execute_external_script
      @language = N'R', 
	  @script = N'
      # Set output directory for files
      # Prior to plotting ensure there are no files with same file names as the out files below in the above directory.
      # Calculate counts of churned/non-churned customers with RevoScaleR
      require("RevoScaleR");
      tmp <- rxCube( ~ churn, edw_cdr_pred, means = FALSE)
      resultsDF <- rxResultsDF(tmp)
      library(dplyr)
      library(ggplot2)
      # Open a jpeg file and output plot in that file.
      image_file = tempfile();
      jpeg(filename=image_file, width=800, height = 550);
      print(
      resultsDF %>%
      ggplot(aes(x = factor(1), y=Counts, fill=factor(churn))) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y") +
	  theme_minimal()
      );
      dev.off();
      OutputDataSet <- data.frame(data=readBin(file(image_file, "rb"), what=raw(), n=1e6));
' 
   , @input_data_1 = N'select * from edw_cdr_pred'
   , @input_data_1_name = N'edw_cdr_pred'
	with result sets ((plot varbinary(max)));
end;
go

--Execute pie procedure
exec pie
go

--Create a stored procedure to plot stackedbar chart for visualizing churn vs predicted churn
drop procedure if exists stackedbar;
go
create procedure stackedbar
as
begin
exec sp_execute_external_script
      @language = N'R', 
	  @script = N'
      # Set output directory for files
      # Prior to plotting ensure there are no files with same file names as the out files below in the above directory.
      # Calculate counts of customers by churn and predicted churn with RevoScaleR
      require("RevoScaleR");
      tmp <- rxCube( ~ churn:F(prediction), data = edw_cdr_pred, mean = FALSE)
      resultsDF <- rxResultsDF(tmp)
	  print(resultsDF)
      library(dplyr)
      library(ggplot2)
      # Open a jpeg file and output plot in that file.
      image_file = tempfile();
      jpeg(filename=image_file, width=800, height = 550);
      print(
      resultsDF %>%
      ggplot(aes(x = churn, y = Counts,
             group = prediction, fill = prediction)) +
      geom_bar(stat = "identity") +
      labs(x = "churn", y = "Counts of customer") +
     theme_minimal()
      );
      dev.off();
      OutputDataSet <- data.frame(data=readBin(file(image_file, "rb"), what=raw(), n=1e6));
' 
   , @input_data_1 = N'select * from edw_cdr_pred'
   , @input_data_1_name = N'edw_cdr_pred'
	with result sets ((plot varbinary(max)));
end;
go

--Generate stacked bar
exec stackedbar
go

