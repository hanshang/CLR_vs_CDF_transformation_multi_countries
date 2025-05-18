# CLR_vs_CDF_transformation_multi_countries
Multi-country comparison between the centered log-ratio and cumulative distribution function transformations

1. load_package.R: load R packages
2. transformation.R: CLR and CDF transformations
3. data.zip: unzip the data folder containing the age distribution of death counts from the 24 countries in the Human Mortality Database
4. read_data.R: read data files
5. point_forecast_compar.R: point forecast comparison based on the Kullback-Leibler and Jensen-Shannon divergences
6. interval_forecast_compar_PI_80.R: interval forecast comparison based on the coverage probability difference between the nominal and empirical coverage probabilities and mean interval score at the 80% nominal coverage probability.
7. interval_forecast_compar_PI_95.R: interval forecast comparison based on the coverage probability difference between the nominal and empirical coverage probabilities and mean interval score at the 95% nominal coverage probability.
8. annuity_calculation.R: based on the 50-years-ahead out-of-sample point forecasts, we determine the annuity prices for various entry ages and maturities.
9. annuity_calculation_interval_PI_80.R: based on the 50-years-ahead out-of-sample interval forecasts at the 80% nominal coverage probability, we determine the lower and upper bounds of the annuity prices for various entry ages and maturities.
10. annuity_calculation_interval_PI_95.R: based on the 50-years-ahead out-of-sample interval forecasts at the 95% nominal coverage probability, we determine the lower and upper bounds of the annuity prices for various entry ages and maturities.
11. save_function.R: R function for saving figures
