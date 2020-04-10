
Order of scripts:

2.) master_anaylsis.R, which calls #3 below (doesn’t exist yet)
1.) make_imputations_all.R
3.) analysis_general_setup.R, which calls #4 below (this file replaces master_marriage.R, etc.)
4.) That file calls analysis_general.R and then merge_results.R


Imputation approach:
- Before imputing, keep only those with known mars89 (this is an inclusion criterion). Note this means that imputed datasets always have same dimension within each restriction type. 