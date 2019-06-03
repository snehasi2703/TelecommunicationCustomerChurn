-- Generate Model
exec generate_random_forest;
-- Evaluate Model
exec model_evaluate
-- Predictions
exec predict_cdr_churn_rx_forest 'rxDForest';





