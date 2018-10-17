# Signal Adaptive Variable Selector for the horseshoe prior
R codes for implementing SAVS algorithm for variable selection.

Description of the associated R script files:
1. savs_algo.R file contains the function that implements SAVS algorithm (Algorithm 1 of the paper). This function takes design matrix and posterior mean as inputs and produces a sparse estimate as output.
2. gendata10.R is a function that generates data on response variable, design matrix of specific structure and true regression coefficient vectors with 10 non-zero elements. One can find more details in the script file.
3. measure_function.R contains the 'meas.fun' function that computes Matthew's Correlation Coefficient (MCC), True Positive Rate (TPR) or sensitivity and True Negative Rate (TNR) or specificity. It takes true regresion coefficient vector and estimated regression coefficient vector as inputs and returns MCC, TPR and TNR values.
4. code-script.R implements SAVS, S5, Adaptive LASSO, SCAD and MCP under a particular setting (mentioned in the paper) and produces the MCC, TPR and TNR values for all the 5 methods.

