
## Descriptive Analysis of Hurricane Forecasting Data

This repository contains code and results for a descriptive analysis of hurricane forecasting data.
The project explores historical patterns, statistical measures, and visual representations to gain insights into hurricane prediction accuracy and trends.

---
## Table of Contents
1. [Dataset](#dataset)
2. [Descriptive Analysis](#Descriptive-Analysis)
3. [GraphicalPlot_VariablesSelection](#GraphicalPlots_VariablesSelection)

---

---

## Dataset
The dataset used for this analysis is stored in a CSV file (`Monthly-data.csv`) and includes:
- **Years**: Historical time span of data (e.g., 1981–2022).
- **Predictors**: See Table.
- **Targets**: Observed hurricane counts.
---

The dataset contains the following variables:

| **Variable**                     | **Description**                                                                 |
|-----------------------------------|---------------------------------------------------------------------------------|
| Year                              | The year of observation.                                                        |
| Month                             | The month of observation.                                                       |
| Hurricane Count                   | The number of hurricanes observed in the given year and month.                  |
| time_series1.enso.ts.1mn          | Monthly ENSO (El Niño-Southern Oscillation) time series data.                   |
| time_series1.tni.had              | Trans-Niño Index (TNI) values from the Hadley dataset.                          |
| time_series1.nina1                | Time series of Niño 1 climate index values.                                     |
| time_series1.nina3.anom           | Anomalies in Niño 3 index values.                                               |
| time_series1.pna                  | Pacific-North American index, reflecting atmospheric pressure patterns.         |
| time_series1.soi                  | Southern Oscillation Index, measuring atmospheric pressure differences.         |
| time_series2.z500_avg             | Average geopotential height at 500 hPa.                                         |
| time_series2.z500_std             | Standard deviation of geopotential height at 500 hPa.                           |
| time_series2.zzwnd200_ori         | Original zonal wind at 200 hPa.                                                 |
| time_series2.zwdn_200_an          | Anomaly in wind deviation at 200 hPa.                                           |
| time_series2.zwdn_500_an          | Anomaly in wind deviation at 500 hPa.                                           |
| time_series2.zwnd200_st           | Standard deviation of zonal wind at 200 hPa.                                    |
| time_series2.OLR                  | Outgoing Longwave Radiation (OLR) values.                                       |
| time_series2.Oni                  | Oceanic Niño Index (ONI) values.                                                |
| MM                                | Monthly mean values for climate indices.                                        |
| ESPI                              | ENSO Precipitation Index values.                                                |
| EI                                | Energy Index values.                                                            |
| LI                                | Landfall Index values.                                                          |
| MON                               | Month of the year (numerical).                                                  |
| X130E.80W                         | Climate data for the region between 130°E and 80°W longitude.                   |
| X160E.80W                         | Climate data for the region between 160°E and 80°W longitude.                   |
| X180W.100W                        | Climate data for the region between 180°W and 100°W longitude.                  |

---

### Notes:
- These variables represent a mix of atmospheric and oceanic predictors as well as the target variable (hurricane count or intensity).
- Time series variables like Niño indices, OLR, and zonal winds are key features used in hurricane prediction models.
- Regional climate variables (e.g., `X130E.80W`) provide spatially focused data for specific longitude ranges.



## Descriptive Analysis
Please download the DescriptiveAnalysis3.pdf  

1. **Correlation Plots**:  correlation between various indexes and hurricane
counts.
2. **Exploratory Plots**: This section includes plots related to specific variables and approaches used in the exploratory
analysis.
3. **Cluster Analysis**: This section presents the results of a cluster analysis performed on the data.
4. **Principal Component Analysis**:


---

## Graphical Variables selection

The document analyzes the relationships between variables and hurricanes counts with multiple scatterplots. 
We provided a comment for each one the plots.






