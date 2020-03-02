/* =============================================================================================== 
NOTE: Do NOT execute this file. It is only meant to be executed by the SILVERMAN macro.
This file contains all necessary R code (plus necessary setup in SAS) to perform the Silverman Test. 
=============================================================================================== */

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
call ExportDataSetToR("silvData2", "silvData");

/* Bring all macro variables into IML and do some necessary quoting before the SUBMIT statement */
k = &k.;
M = &M.;
adjust = "&adjust";
digits = &digits.;
setSeed = &setSeed.;
out1 = &out1.;
sreduction = "&reduction";
smetric = "&distance_metric";
clust_nvars = &clust_nvars.;
clust_nobs = &clust_nobs.;
nmiss = &nmiss.;
dist_std = "&distance_standardize";
dsname = "&data";
pcacenter = "&pca_center";
pcascale = "&pca_scale";

	submit k M adjust digits setSeed out1 sreduction smetric clust_nvars clust_nobs nmiss dist_std dsname pcacenter pcascale / R;
		# Verify data is numeric
		if(!is.numeric(as.matrix(silvData))) {
			cat("ERROR: Your data was input in SAS as numeric, but R did not recognize it as numeric. This may be due to the use of SAS formats, such as dates, times, or custom formats. If you decide that removing a format is appropriate, then documentation with directions is available at the following link:\n http://support.sas.com/documentation/cdl/en/lrdict/64316/HTML/default/viewer.htm#a000178212.htm")
		} else {
			# From the silvermantest package cited in the paper, we concatenated the three main functions necessary to perform
			# the test. We also modified so that k = 1 is the default, and the returned
			# value of the silverman.test is a list containing the p-value and saved seed.
			# The method used in the bootstrap is the one described in Silverman 1981, which is slightly different
			# from the method used in the original silvermantest package
			# Originally obtained from: https://www.mathematik.uni-marburg.de/~stochastik/R_packages/

			silverman.test <-
			  function(x,k=1,M=999,adjust=FALSE,digits=6,seed=NULL){
			    # x: data
			    # k: number of modes to be tested. For clusterability purposes, this should be 1.
			    # M: number of bootstrap replications
			   
			    #check if seed is available (as done in boot package)
			    #if so save it
			    seedAvailable = exists(x=".Random.seed",envir=.GlobalEnv,inherits=FALSE)
			    if(seedAvailable)
			      saved_seed = .Random.seed 
			    else{
			      rnorm(1)
			      saved_seed = .Random.seed
			    }

				if(!is.null(seed)) {
					set.seed(seed)
				}
			    
			    # temp function for bootstrapping
			    y.obs <- function(x,h,sig=sd(x)){
					#mean(x) + (x - mean(x) + h*rnorm(length(x),0,1))/((1 + h^2/sig^2)^(1/2))
					(x+h*rnorm(length(x),0,1))/((1+h^2/sig^2)^(1/2))
			    }
			    
			    # temp function for density calculation
			    nor.kernel <- function(x,h){
			      density(x,bw=h,kernel ="gaussian")$y
			    }
			    
			    #start of the test
			    h0 <- h.crit(x, k)
			    n <- 0
			    

			    for (i in 1:M) {
			      x.boot <- sort(y.obs(sample(x, replace=TRUE),h0))
			      mod.temp <- nr.modes(nor.kernel(x.boot,h0))
			      if (mod.temp > k){
			        n <- n+1
			      }
			    }
			    p <- n/M
			    ptemp=p
			    
			    if(adjust){
			      if(k==1){
			        #asymptotic levels of silvermantest by Hall/York
			        x=c(0,0.005,0.010,0.020,0.030,0.040,0.050,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.2,0.25,0.30,0.35,0.40,0.50)
			        y=c(0,0,0,0.002,0.004,0.006,0.010,0.012,0.016,0.021,0.025,0.032,0.038,0.043,0.050,0.057,0.062,0.07,0.079,0.088,0.094,0.102,0.149,0.202,0.252,0.308,0.423)
					sp = splines::interpSpline(x,y)
					#adjusting the p-value
			        #if(p<0.005)
			         # p=0
			        #else{
			          p = predict(sp,p)$y
			          p = round(p,digits)

					  # in certain cases, spline interpolation gives a negative p-value.
			          if(p < 0){
			            p <- 0
			          }
			        #}
			      }
			    }
			    return(list(saved_seed = saved_seed, p_value = p, k = k, hcrit = h0))
			  }

			nr.modes <-
			  function(y){
			    
			    d1 <- diff(y)
			    signs <- diff(d1/abs(d1))
			    length(signs[signs==-2])
			    
			  }

			h.crit <-
			  function(x,k,prec=6){
			    
			    #temp function
			    nor.kernel <- function(x,h){
			      density(x,bw=h,kernel ="gaussian")$y
			    }
			    
			    digits=prec
			    prec=10^(-prec)
			    x <- sort(x)
			    minh <- min(diff(x))		#minimal possible h
			    maxh <- diff(range(x))/2	#maximal possible h
			    a <- maxh
			    b <- minh
			    
			    while (abs(b-a)>prec){
			      m <- nr.modes(nor.kernel(x,a))
			      
			      b <- a
			      if (m > k){
			        minh <- a
			        a <- (a + maxh)/2
			      } 
			      else {
			        maxh <- a
			        a <- (a - minh)/2
			      }
			    }
			    
			    a=round(a,digits)
			    
			    
			    if(nr.modes( nor.kernel(x,a) ) <= k){
			      # subtract until more than k modes
			      while(nr.modes( nor.kernel(x,a) ) <= k){
			        a = a - prec
			      }
			      a=a+prec
			    }
			    
			    if(nr.modes( nor.kernel(x,a) ) > k){
			      # add until nr. of modes correct
			      while(nr.modes( nor.kernel(x,a) ) > k){
			        a = a + prec
			      }
			    }
			    
			    a
			  }

			########### Setup ############


			# Load necessary packages. Modified code from: https://stackoverflow.com/questions/15155814/check-if-r-package-is-installed-then-load-library
			# Splines is necessary only when adjusting p-values, so only install if necessary
			if(&adjust) {
				# if package is installed locally, then load
				if("splines" %in% rownames(installed.packages())) {
					do.call("library", list("splines"))
				} else { # if package is not installed locally, then download and load
					cat("Installing splines package.\n\n")
					install.packages("splines")
					do.call("library", list("splines"))
				}
			}
			
			######### Run the test ###########
			stresult <- silverman.test(as.matrix(silvData), &k, &M, &adjust, &digits, &setSeed)


			############ Print the data set information ###########
			# Printing output. Use cat() rather than print() to avoid line numbers in output
			# Title
			cat("----------------------\n")
			cat("Clusterability Test\n")
			cat("----------------------\n\n")
			
			# Data set information
			cat(paste("Data set name: ", "&dsname", "\n", sep = ""))
			cat(paste("Your data set has", &clust_nobs, "observations and", &clust_nvars, "variable(s). For more information, please run PROC CONTENTS.\n", sep = " "))

			if(&nmiss > 0){
				cat(paste('WARNING: Of these,', &nmiss, 'observation(s) is/are missing at least one variable value.\n\n', sep = ' '))
			} else {
				cat('There were no missing values. Your data set is complete.\n\n')
			}
			
			
			# Print reduction method, distance metric, and standardization method (if applicable)
			# Prefer identical() over == for comparison inside if()
			if(identical(toupper('&sreduction'), 'DISTANCE')) {
 				cat('Data Reduced Using: Pairwise Distances\n')
				cat(paste('Distance Metric:', '&smetric', '\n', sep = ' '))
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
			} else if(identical(toupper('&sreduction'), 'PCA')) {
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
			cat("\n----------------------------------------------\n")
			cat("Results: Silverman's Critical Bandwidth Test\n")
			cat("----------------------------------------------\n\n")
			cat(paste("Null Hypothesis: number of modes <=", stresult$k, "\n",  sep=" "))
			cat(paste("Alternative Hypothesis: number of modes >", stresult$k, "\n", sep = " "))
			cat(paste("p-value:", stresult$p_value, "\n", sep=" "))
			cat(paste("Critical bandwidth:", stresult$hcrit, "\n\n", sep=" "))




			############ Print the options used to generate that particular output ###########
			cat("---------------------\n")
			cat("Test Options Used\n")
			cat("---------------------\n\n")

			# Display RNG seed if one was provided
			if(!is.null(&setSeed)) {
				cat(paste("Seed set in R to:", &setSeed, "\n", sep = " "))
			 }

			# Print more information about options used
			# Number of bootstrap replicates. Always relevant.
			cat(paste("p value based on", &M, "bootstrap replicates\n", sep = " "))
			# If adjusted, print that p values were adjusted using work by Hall and York
			if(&adjust) {
				cat("Adjusted p-values based on the adjustment in Hall and York (2001)\n")
			}
			# If k == 1, adjust = TRUE, then show how many digits we rounded the p value to
			if(&adjust && &k == 1) {
				cat(paste("p value rounded to:", &digits, "digits\n", sep = " "))
			}
	
			# If out was specified, then indicate that we stored the results of the test into the ___ data set
			if(!is.null(&out1)){
				cat(paste("Results from test stored in SAS data set:", "&out1", "\n", sep = " "))
			}


			######### Store results ##########
			# Create data frame from results to send back to SAS if the out parameter was specified
			if(!is.null(&out1)){
				# Begin populating with the variables that are always present
				resultdf <- data.frame(DataSet = "&dsname", Test = "Silverman",
										NullHypothesis = paste("Number of modes <=", stresult$k, sep = " "),
										AlternativeHypothesis = paste("Number of modes >", stresult$k, sep = " "), 
										Pvalue = stresult$p_value, CritBW = stresult$hcrit)

				# Now, fill in the optional parameters when applicable

				# digits
				if(&adjust && &k == 1) {
					resultdf$Digits <- &digits
				}

				# adjust
				resultdf$AdjustPval <- &adjust

				# reps
				resultdf$Reps <- &M

				# reduction
				resultdf$Reduction <- "&sreduction"
				
				# distance_metric and distance_standardize
				if(identical(toupper('&sreduction'), 'DISTANCE')) {
					resultdf$Distance_Metric <- "&smetric"
					resultdf$Distance_Standardize <- "&dist_std"
				}

				if(!is.null(&setSeed)) {
					resultdf$RNGseed <- &setSeed
			 	}

			}

		}
endsubmit;

/* If out parameter was used, then store the output data into a SAS data set. */
if compare(&out1, "NULL") ^= 0 then call ImportDataSetFromR(&out1, "resultdf");
quit;
