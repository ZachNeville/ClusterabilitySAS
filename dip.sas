/* ============================================================================================ 
NOTE: Do NOT execute this file. It is only meant to be executed by the DIP macro.
This file contains all necessary R code (plus necessary setup in SAS) to perform the Dip Test. 
============================================================================================= */

/* Copyright (C) 2020  Zachariah Neville, Naomi Brownstein, Andreas Adolfsson, Margareta Ackerman

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA. */



proc iml;
call ExportDataSetToR("dipData2", "dipdata");

/* Since macro variables don't work well in a SUBMIT /R statement. */
dspv = "&simulatePvalue";
dreps = &reps.;
dreduction = "&reduction";
dmetric = "&distance_metric";
clust_nvars = &clust_nvars.;
clust_nobs = &clust_nobs.;
nmiss = &nmiss.;
dist_std = "&distance_standardize";
dsname = "&data";
out1 = &out1.;
pcacenter = "&pca_center";
pcascale = "&pca_scale";

	submit dspv dreps dreduction dmetric clust_nvars clust_nobs nmiss dist_std dsname out1 pcacenter pcascale / R;

	if(!is.numeric(as.matrix(dipdata))) {
		cat("ERROR: Your data was input in SAS as numeric, but R did not recognize it as numeric. This may be due to the use of SAS formats, such as dates, times, or custom formats. If you decide that removing a format is appropriate, then documentation with directions is available at the following link:\n http://support.sas.com/documentation/cdl/en/lrdict/64316/HTML/default/viewer.htm#a000178212.htm")
	} else {
		
		# Load necessary packages. Using code from: https://stackoverflow.com/questions/15155814/check-if-r-package-is-installed-then-load-library
		# Modified from the source since we are only installing one package.  
		# if package is installed locally, load
		if("diptest" %in% rownames(installed.packages())) {
			do.call("library", list("diptest"))
		} else { # if package is not installed locally, download, then load
			cat("Installing diptest package.\n\n")
			install.packages("diptest")
			do.call("library", list("diptest"))
		}

		# Perform dip test
		dipresult <- dip.test(as.matrix(dipdata), &dspv, &dreps)
		

		############ Print the data set information ###########
		# Printing output. Use cat() rather than print() to avoid line numbers in output
		# Title
		cat("----------------------\n")
		cat("Clusterability Test\n")
		cat("----------------------\n\n")


			
		# Data set information
		cat(paste("Data set name: ", "&dsname", "\n", sep = ""))
		cat(paste("Your data set has", &clust_nobs, "observation(s) and", &clust_nvars, "variable(s). For more information, please run PROC CONTENTS.\n", sep = " "))

		if(&nmiss > 0){
			cat(paste('WARNING: Of these,', &nmiss, 'observation(s) is/are missing at least one variable value.\n\n', sep = ' '))
		} else {
			cat('There were no missing values. Your data set is complete.\n\n')
		}
		
		
		# Print reduction method, distance metric, and standardization method (if applicable)
		# Prefer identical() over == for comparison inside if()
		if(identical(toupper('&dreduction'), 'DISTANCE')) {
			cat('Data Reduced Using: Pairwise Distances\n')
			cat(paste('Distance Metric:', '&dmetric', '\n', sep = ' '))
			if(!identical(toupper('&dist_std'), 'NONE')) {
				cat(paste('Standardization Method Used:', '&dist_std', '\n', sep = ' '))
				
				std_description <- switch(toupper('&dist_std'),
					"STD" = "Data is standardized so that each variable has mean 0 and standard deviation 1.",
					"MEAN" = "Each variable is centered around its mean. In other words, the mean - after standardization - is 0, and the standard deviation is unchanged.",
					"MEDIAN" = "Each variable is centered around its median. The median - after standardization - is 0, and the standard deviation is unchanged.",
					"RANGE" = "Each variable is centered and scaled so its minimum value is 0 and maximum value is 1. (The location is shifted by the minimum value so its new minimum is 0, and the shifted variable is then divided by the range of the variable).",
					"MIDRANGE" = "Each variable is centered and scaled so its minimum value is -1 and maximum value is 1. (The location is shifted by the midrange and divided by half the range of the variable, so that the new minimum is -1 and new maximum is 1).",
					"IQR" = "Each variable is centered about its median (so the new median is 0) and scaled so its new interquartile range (IQR) is 1 (that is, the shifted variable is divided by its IQR).",
					"MAD" = "Each variable is centered about its median (so the new median is 0) and scaled by dividing each observation by the median absolute deviation from the median.",
					"For L(p), p is a numeric constant (not necessarily an integer) greater than or equal to 1 that specifies the power to which differences are to be raised in computing an L(p) norm or Minkowski metric"
				)


				cat(paste("\t", std_description, sep = ' ')) # description
			}
			cat('\n')
		} else if(identical(toupper('&dreduction'), 'PCA')) {
 			cat('Data Reduced Using: PCA. ')

			if(&pcacenter) {
				cat('Variables were centered. ')
			} else {
				cat('Variables were not centered. ')
			}

			if(&pcascale) {
				cat('Variables were scaled.\n')
			} else {
				cat('Variables were not scaled.\n')
			}
		}


		############ Print the test results ###########
		cat("\n-----------------------------------------\n")
		cat("Results: Dip Test of Unimodality\n")
		cat("-----------------------------------------\n\n")
		cat("Null Hypothesis: number of modes = 1\n")
		cat("Alternative Hypothesis: number of modes > 1\n")
		cat(paste("p-value:", dipresult$p.value, "\n", sep=" "))
		cat(paste("Dip statistic:", unname(dipresult$statistic), "\n\n", sep=" "))

		############ Print the options used to generate that particular output ###########
		cat("---------------------\n")
		cat("Test Options Used\n")
		cat("---------------------\n\n")

		# Boolean indicating whether any options were printed. If no options printed, we need to print a separate message
		nooptions <- TRUE

		# If p values were simulated using Monte Carlo simulation
		if(&dspv) {
			cat(paste("p values obtained from a Monte Carlo simulation with", &dreps, "replicates\n", sep = " "))
			nooptions <- FALSE
		}

		# If out was specified, then indicate that we stored the results of the test into the ___ data set
		if(!is.null(&out1)){
			cat(paste("Results from test stored in SAS data set:", "&out1", "\n", sep = " "))
			nooptions <- FALSE
		}

		# If no options were specified, then we will indicate that
		if(nooptions) {
			cat("Default values for the optional parameters were used. To learn more about customizing the behavior of the %clusterability macro, please see the accompanying paper.")
		}

		
		######### Store results ##########
		# Create data frame from results to send back to SAS if the out parameter was specified
		if(!is.null(&out1)){
			# Fill in the values that are applicable for all Dip tests
			dipdf <- data.frame(DataSet = "&dsname", Test = "Dip", 
								NullHypothesis = "number of modes = 1",
 								AlternativeHypothesis = "number of modes > 1",
								DipStatistic = unname(dipresult$statistic), 
								Pvalue = dipresult$p.value)
			

			# If values simulated via MC, we add the other values
			if(&dspv) {
				dipdf$reps <- &dreps
				dipdf$SimulatedPvalues <- 1
			} else {
				dipdf$SimulatedPvalues <- 0
			}

			# reduction
			dipdf$Reduction <- "&dreduction"

			# distance_metric and distance_standardize
			if(identical(toupper('&dreduction'), 'DISTANCE')) {
				dipdf$Distance_Metric <- "&dmetric"
				dipdf$Distance_Standardize <- "&dist_std"
			}
		}
	}
	endsubmit;

	/* If out argument was specified, then import dip results back into SAS  */
	if compare(&out1, "NULL") ^= 0 then call ImportDataSetFromR(&out1, "dipdf");
quit;
