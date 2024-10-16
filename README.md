# Community_analysis
Clustering and supplementary analyses

# Main scripts:
*dg_walktrap_ER.R* Takes trimmed data from RedCap and computes Walktrap clustering analysis to determine clusters in the data. Optimizes the modularity result by selecting the number of random steps that yields the highest modularity. Lastly, plots and saves results.
<br />
    Based on Mackenzie's Walktrap_ExampleCode_MEM.R
<br />
    Needs *dg_determine_groups.R* in the same directory.
<br />
<br />
*dg_itemlevel_data_converter_ER.R* Given an Excel with labels document downloaded from RedCap, it processes the data, and recodes all the items of several subscores by finding the least common multiple (LCM). The script can handle any range of item-level data and number of subscores.

# Supplementary scripts:
*dg_trim_clinicaldata.R* The script cleans and organizes the data collected with 3 surveys used in the BrainMAP study.
<br />
<br />
*dg_determine_groups.R* The script reads the frequently updated "Demographics Form.csv" BrainMAP document and provides a list of participants regarded as with ADHD or as TD.
<br />
<br />
*dg_MWUtest_afterWalktrap.R* The script compares 2 clusters of BrainMAP participants (either ADHD only or ADHD plus TD) using a Wilcoxon rank sum test, equivalent to Mann-Whitney U test. Used for preliminary results.
<br />
Needs *dg_determine_groups.R* and *dg_trim_clinicaldata.R*. ¡¡It also needs the converted/rescaled data with *dg_itemlevel_data_converter_ER.R*!!
