# Neuropsychology_related
Convert normative scores and compare scores of two groups

"dg_PR_Zscore_Tscore_converter.m" Converts Percentile ranks (PR) to Z-scores or T-scores, as indicated. 
Written according to Iverson, G.L. (2011). Z Scores. In Kreutzer, J.S., DeLuca, J., & Caplan, B. (Eds.), Encyclopedia of Clinical Neuropsychology.(pp. 2739-2740). Springer. https://doi.org/10.1007/978-0-387-79948-3

"dg_compare_cognitive.m" Compares Z-scores or PRs of two groups of subjects (i.e., patients vs. controls) using the Wilcoxon rank test (equivalent to the Mann-Whitney-U test). Additionally, the script applies an FDR correction (https://www.mathworks.com/matlabcentral/fileexchange/27418-fdr_bh) for multiple comparisons. An implementation of t-tests for bigger samples can be added if required. Still to add: selection between Wilcoxon rank test or t-test.

"dg_dprime_2afc.m" is a function intended to be used within a for loop. Given a <<cond_data>> table with a HIT variable/column, it will count correct responses coded as 1s and incorrect responses or omissions coded as 0s to determine percent correct (PC), accuracy, bias ratio, and D prime (d') after a 2-alternative forced choice (2AFC) design. Every iteration of the for loop corresponds to a condition of the task. Coded based on Stanislaw and Todorov (1999) https://psycnet.apa.org/doi/10.3758/BF03207704.
