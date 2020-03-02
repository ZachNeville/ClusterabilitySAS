# Clusterability SAS Macro
The **clusterability** SAS macro can be used to test for the cluster tendency of a dataset. This preprocessing step can inform whether clustering algorithms are appropriate.

## Installation
**Requirements:** SAS software and base R software must be installed on the local machine. The **diptest** and **splines** R packages must be installed or the machine must be connected to the internet and have the capability to install R packages. On first use, the macro will attempt to install these packages if they are not present.

1.    Download the *macroDefinitions, dip, silverman,* and *clusterability_examples* files from the main folder in this repository to your machine. These should be saved in a permanent location.
2.    The RLANG system option must be enabled, which is typically done by editing the SASV9.CFG file. This option may not be available for users of browser-based SAS software. [Documentation](http://documentation.sas.com/?docsetId=imlug&docsetTarget=imlug_r_sect003.htm&docsetVersion=15.1&locale=en) on how to enable this option can be found in the [SAS/IML User Guide](http://documentation.sas.com/?docsetId=imlug&docsetTarget=titlepage.htm&docsetVersion=15.1&locale=en).
3.    Open the *macroDefinitions* file in SAS software and execute the entire file to initialize the **clusterability** macro. 
4.    Open the *clusterability_examples* file in SAS software and update the path specified in the filename statement at the top of the file. The path should point to the folder where the *dip* and *silverman* files are located.
5.    The examples in *clusterability_examples* can now be run.

## Example
After completing the installation instructions, we can perform a test of clusterability to determine if the numeric variables in the [*iris*](http://documentation.sas.com/?docsetId=statug&docsetVersion=15.1&docsetTarget=statug_sashelp_sect014.htm&locale=en) dataset have a natural cluster tendency.

## Input
``` SAS
/* This needs to be updated to reflect the folder on your local machine */
filename clust "C:\Documents\Clusterability\";

data iris;
	set sashelp.iris;
	drop Species;
run;

%clusterability(data = iris, test = dip, reduction = PCA, folder = clust);
```

## Output
``` SAS
Clusterability Test: iris 

----------------------                                                                                
Clusterability Test                                                                                   
----------------------                         

Data set name: iris                                                                                   
Your data set has 150 observation(s) and 4 variable(s). For more information, please run PROC         
CONTENTS.                                                                                             
There were no missing values. Your data set is complete.                                              

Data Reduced Using: PCA. Variables were centered. Variables were scaled.                              

-----------------------------------------                                                             
Results: Dip Test of Unimodality                                                                      
-----------------------------------------                                                             

Null Hypothesis: number of modes = 1                                                                  
Alternative Hypothesis: number of modes > 1                                                           
p-value: 0                                                                                            
Dip statistic: 0.107841006841301                                                                      

---------------------                                                                                 
Test Options Used                                                                                     
---------------------                                                                                 

Default values for the optional parameters were used. To learn more about customizing the behavior of 
the %clusterability macro, please see the accompanying paper.                                         
```

## Required Parameters
##### data
A SAS dataset containing numeric data to be used in the clusterability macro.
##### test
The multimodality test to be performed. Available choices are `dip` for the Dip Test and `silverman` for Silverman's Test. 
##### folder
A SAS libref associated with the folder where the *dip* and *silverman* files are located. If using the *clusterability_examples* file, `clust` is the libref used.
## Optional Parameters
##### reduction
The dimension reduction technique to be used. The default is `pca`, which uses Principal Component Analysis to reduce the dataset to one dimension. The value `distance` can be used to reduce the dimension by computing pairwise distances, and `none` can be used to prevent any dimension reduction at all.  
##### pca_center
Applicable only if PCA is used for dimension reduction. If `TRUE`, which is the default, then the variables are centered to have mean 0 before computing principal components. If `FALSE`, this centering is not performed.
##### pca_scale
Applicable only if PCA is used for dimension reduction. If `TRUE`, the correlation matrix is used to compute principal components. If `FALSE`, the covariance matrix is used instead. The default is `TRUE`.
##### distance_metric
Only applicable if pairwise distances is used for dimension reduction. This is the distance metric to be used when computing the distances. The default is `euclid`, which uses the Euclidean distance. Other metrics are given in the accompanying paper.
##### distance_standardize
Only applicable if pairwise distances is used for dimension reduction. This is the type of standardization, if any, to be used before computing the distances. The default is `STD`, which standardizes each variable to have mean 0 and standard deviation 1.
##### is_dist_matrix
Whether the **data** dataset is a distance matrix. If `TRUE`, then only the lower triangular elements of the dataset are used in the analysis. The default is `FALSE`.
##### complete_case
Whether to perform a complete case analysis when performing the test. The default is `FALSE`. If `TRUE`, then any partially or fully missing observations are removed.
##### out
The name of the dataset that the macro will create, which will contain statistics and results from the clusterability test. Additional information about the contents of the **out** dataset is provided in the accompanying paper.
## Additional Parameters and Details
There are additional parameters to control behavior of the Dip Test - which are prefixed with *d_* - and the Silverman Test - which are prefixed with *s_*. Documentation of these parameters, along with additional details of the parameters discussed above, is found in the accompanying paper.
## Supplemental Files
##### clusterability_examples.sas
Contains examples of how to use the **clusterability** macro on various datasets. These are the same examples in the accompanying paper.
##### clusterability_timings.sas
Used to compare the relative computational performance of the various test and dimension reduction combinations.
##### plots.sas
Contains code to reproduce the plots found in the accompanying paper.

