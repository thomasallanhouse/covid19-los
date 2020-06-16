# covid19-los

Code For COVID-19 Hospital Length of Stay Work.

## Accelerated Failure Time (AFT) model on CHESS data

CHESS_AFT_STATA.do - A STATA do file.

R_code_JAGS_Comparison.R - R code for JAGS.

## Multistate (MS) model on NHS Trust Data

Ward_COVID.sql - The SQL query run on MFT data. This will not execute independently of MFT, but is provided to help with reproducibility.

Simulate_Surge.ipynb - This will simulate data in the form extracted from the SQL query without any input from confidential data.

Process_Data_Table.ipynb - This processes the simulated data (easily adapted for real data) into the format suggested for multi-state survival analysis in the flexsurv R package. Data are bootstrapped at the individual level.

Fit_And_Predict.ipynb - This fits a multi-state survival model, compares to a census, and produces confidence and prediction intervals for lengths of stay.

Indicative_planning.ipynb - This produces indivative scenarios for healthcare planning from the different fitted model outputs.

## Truncation corrected (TC) model on CHESS data

Truncation_corrected_LoS.m and Truncation_corrected_LoS.py provide the main algorithm. See also https://github.com/thomasallanhouse/covid19-growth and associated papers at https://arxiv.org/abs/2004.00117 and https://arxiv.org/abs/2005.04937
