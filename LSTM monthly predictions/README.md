# LSTM Model for Hurricane Forecasting

This Python script implements an LSTM model for hurricane forecasting using historical data. The code is designed for Google Colab and requires data stored on Google Drive.

```python
#!pip install GPy
#!pip install emukit

"""
Updated for Monthly Data and Three Models
"""

from google.colab import drive
import pandas as pd

# Step 1: Mount Google Drive
drive.mount('/content/drive')

# Step 2: Define the path to the folder and files
# Make sure to have the right path (this is my personal one)
folder_path = '/content/drive/My Drive/Hurricane_Forecasting_LSTM/'  # Adjust path if necessary
D = folder_path + 'datasetfinale.csv'

# Step 3: Load the CSV file
D = pd.read_csv(D)

# Display the first 5 rows of the dataset
print(D.head())

# -*- coding: utf-8 -*-
import numpy as np
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, Dense
from sklearn.metrics import mean_absolute_error

# Initialize storage for 100 runs
runs = 40
predictions_all_runs = []
mae_all_runs = []
da_all_runs = []

# Define the testing years
start_year = 1981
end_year = 2022
test_years = range(2011, end_year)

# Function to compute directional accuracy
def directional_accuracy(forecast, actual):
    forecast_diff = np.diff(forecast)
    actual_diff = np.diff(actual)
    correct_directions = sum(np.sign(forecast_diff) == np.sign(actual_diff))
    return correct_directions / (len(actual) - 1)

# LSTM model training and prediction for 100 runs
for run in range(runs):
    run_predictions = []
    test_targets = []

    for test_year in test_years:
        # Training and testing subsets
        train_data = D[(D['Year'] >= start_year) & (D['Year'] < test_year)]
        test_data = D[D['Year'] == test_year]

        y_train = train_data['yt'].values
        y_test = test_data['yt'].values

        # Model predictors (Model 3 as an example: Combine lagged months and time series)
        X_train = np.hstack((train_data.iloc[:, 5:17].values,
                             train_data[['time_series1.nina1', 'time_series2.z500_std']].values))
        X_test = np.hstack((test_data.iloc[:, 5:17].values,
                            test_data[['time_series1.nina1', 'time_series2.z500_std']].values))

        # Reshape for LSTM
        X_train = np.reshape(X_train, (X_train.shape[0], X_train.shape[1], 1))
        X_test = np.reshape(X_test, (X_test.shape[0], X_test.shape[1], 1))

        # Define and train LSTM model
        model = Sequential([
            LSTM(100, input_shape=(X_train.shape[1], X_train.shape[2])),
            Dense(1)
        ])
        model.compile(optimizer='adam', loss='mean_squared_error')
        model.fit(X_train, y_train, epochs=50, batch_size=32, verbose=0)

        # Predict and aggregate for the test year
        y_pred = model.predict(X_test).sum()  # Sum monthly predictions
        run_predictions.append(y_pred)
        test_targets.append(y_test.sum())

    # Store predictions for this run
    predictions_all_runs.append(run_predictions)
    mae_all_runs.append(mean_absolute_error(test_targets, run_predictions))
    da_all_runs.append(directional_accuracy(run_predictions, test_targets))

# Convert predictions and metrics to arrays
predictions_all_runs = np.array(predictions_all_runs)  # Shape: (runs, len(test_years))
test_targets = np.array(test_targets)  # For comparison

# Compute average predictions and confidence intervals
average_predictions = predictions_all_runs.mean(axis=0)
std_predictions = predictions_all_runs.std(axis=0)
lower_bound = average_predictions - 1.96 * std_predictions
upper_bound = average_predictions + 1.96 * std_predictions

# Save results to CSV
predictions_df = pd.DataFrame(predictions_all_runs, columns=test_years)
predictions_df.to_csv('/content/drive/MyDrive/LSTM_predictions_all_runs60_100.csv', index=False)

metrics_df = pd.DataFrame({
    'Test Year': test_years,
    'Average Prediction': average_predictions,
    'Lower CI': lower_bound,
    'Upper CI': upper_bound,
    'Target': test_targets
})
metrics_df.to_csv('/content/drive/MyDrive/LSTM_metrics60_100.csv', index=False)

# Save metrics summary
mae_df = pd.DataFrame({'Run': range(1, runs + 1), 'MAE': mae_all_runs, 'DA': da_all_runs})
mae_df.to_csv('/content/drive/MyDrive/LSTM_mae_da_summary60_100.csv', index=False)
