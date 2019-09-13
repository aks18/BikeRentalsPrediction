# BikeRentalsPrediction
Using features of a given day to predict the number of rentals a bike rental company will get.

1. For Python (4 files are mandatory to run successfully)
The folder should & must have the following files, which are mandatory for the code to successfully
run
  * BikeRentals - Python.ipynb (Python Notebook having code)
  * day.csv (training dataset provided)
  * sample_input.csv (sample_input file)
  * best_tuned_model.pkl (best tuned model saved to disk)
All these files should be present at the same file path (in the same folder) to avoid file path errors
while reading.
All the necessary libraries are coded into the Jupyter notebook and will be installed as the code runs.
Built on **Python 3**
2. For R (4 files are mandatory to run successfully)
The folder should & must have the following files, which are mandatory for the code to successfully
run
  * BikeRentals - R.R (R Script)
  * day.csv (training dataset provided)
  * sample_input.csv (sample_input file)
  * final_tuned_model.rds (best tuned model saved to disk)
All the necessary libraries are coded into the R script and will be installed as the code runs. Set the
working directory correctly before running the script.
Built on **R 3.4.3**

There should be no errors as the code has been tested at least 5 times. If any error arises, it has to be
because of incorrect file paths or version difference in library loading.

Thank you. Hope you like this work.

# Final Result of Regression

![Predicted Values vs. Actual Values Expected](https://github.com/aks18/BikeRentalsPrediction/blob/master/Plots/21_ScatterLinePlotConfInterval_pred_vs_real.png)


### Some Interesting Insights from the project

## Correlation Plot of number of observations with Seasons
![Correlation Plot with Season](https://github.com/aks18/BikeRentalsPrediction/blob/master/Plots/15_CorrelationPlotWithSeasons.png)

## Correlation Plot of number of observations with Year
![Correlation Plot with Season](https://github.com/aks18/BikeRentalsPrediction/blob/master/Plots/20_CorrelPlotYear.png)
