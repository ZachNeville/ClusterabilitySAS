/* =================================================================================== 
NOTE: This file contains the definitions necessary to run the %clusterability macro. 
Execute this file before using the macro.
=================================================================================== */

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

%macro clusterability(data, test = , reduction = PCA, pca_center = TRUE, pca_scale = TRUE, distance_metric = EUCLID, distance_standardize = STD, 
is_dist_matrix = FALSE, completecase = FALSE, folder = , out = ,/* Arguments for both tests */
d_simulatepvalue = FALSE, d_reps = 2000, /* Dip arguments preceded by d_ */
s_M = 999, s_adjust = TRUE, s_digits = 6, s_setSeed = ) / pbuff; /* Silverman arguments preceded by s_ */
/* ===============================================
================ Initial Setup ===================
================================================== */
option minoperator; /* Necessary to use the IN operator within a macro */
option formchar = "|----|+|---+=|-/\<>*"; /* SAS-recommended. Ensures portability to other systems */
option noquotelenmax; /* Prevent SAS from putting warning in log when multiple warnings from the macro occur, since we use string concatenation for the macro warning messages. */

/* Warning messages for the Results Window and for the Log must be formatted separately because we are using colors
in the Log */
/* Initialize all warning macro variables */
%let warnmsg_l =; 
%let warnmsg_w =;
%let warncount = 0; 

/* ===============================================
================ Data set checks ================= 
================================================== */
/* Verify data set exists; necessary before performing reduction */
%if not %sysfunc(exist(&data.)) %then %do;
	%let errmsg = ERROR: Data set &data does not exist. Make sure the data set name is spelled correctly.;
	%goto exit2;
%end;

/* Check for zero-observation dataset or other issues with the data set. Necessary before using PROC DISTANCE in %clusterability */
%let dsidc = %sysfunc(open(&data., i));
%let obs1c = %sysfunc(fetchobs(&dsidc., 1));

%if &obs1c = -1 %then %do;
	%let errmsg = ERROR: Data set &data has 0 observations.;
	%goto exit2;
%end;

%if &obs1c > 0 %then %do;
	%let errmsg = %sysfunc(sysmsg());
	%goto exit2;
%end;

/* Gather size of data set. This needs to be done before any data reduction. */
%let clust_nvars = %sysfunc(attrn(&dsidc., nvars));
%let clust_nobs = %sysfunc(attrn(&dsidc., nobs));

/* Check for non-numeric data. Could cause problems in PROC DISTANCE or the PCA calculation.
Also causes issues with Missing Data Check, so needs to be done early. */
%do i = 1 %to &clust_nvars.;
	%if %sysfunc(vartype(&dsidc., &i)) = C %then %do;
		%let errmsg = ERROR: Non-numeric data was found in the data set. These methods were not designed for non-numeric data.;
		%goto exit2;
	%end;
%end;

/* Missing data check. rowmiss stores the number of rows with at least one missing variable */
proc iml;
	use &data.;
	read all into x;
	close &data.;
	nmissing = countmiss(x, "row");
	rowmissing = sum(nmissing > 0);
	call symputx("rowmiss", rowmissing);
quit;

/* Validate is_dist_matrix is a boolean value and, if TRUE, is being used with the NONE reduction method. */
%if not(%sysfunc(upcase(&is_dist_matrix.)) in (FALSE TRUE)) %then %do;
	%let is_dist_matrix = FALSE;
	%let warncount = %eval(&warncount. + 1);
	%let warnmsg_l = &warnmsg_l. WARNING- NOTE: is_dist_matrix must be set to TRUE or FALSE. Continuing using default value of FALSE.%str(;);
	%let warnmsg_w = &warnmsg_w. NOTE: is_dist_matrix must be set to TRUE or FALSE. Continuing using default value of FALSE.%str(;);
%end;
%else %do;
	%let is_dist_matrix = %sysfunc(upcase(&is_dist_matrix.));
%end;

%if %sysfunc(compare(&is_dist_matrix., TRUE, il)) = 0 %then %do;
	%if %sysfunc(compare(&reduction., NONE, il)) ^= 0 %then %do;
		%let errmsg = ERROR: When providing a value of TRUE for the is_dist_matrix argument, the reduction argument must be NONE.;
		%goto exit2;
	%end;
	
	%if %sysevalf(&clust_nvars) ^= %sysevalf(&clust_nobs) %then %do;
		%let errmsg = ERROR: When providing a value of TRUE for the is_dist_matrix argument, the number of rows and columns in the dataset must be the same.;
		%goto exit2;
	%end;
	
	/* Verify data is either symmetric or upper/lower triangular */
	%let isnotsymmetric = 0;
	%let hasvaluesupper = 0;
	%let hasvalueslower = 0;

	proc iml;
		use &data;
		read all var _ALL_ into x;
   		y = t(x);
		if x = y then
			call symputx("isnotsymmetric", 1);

		lowtri = ( remove(vech(x), cusum( 1 || (ncol(x):2) )) )`;
		uptri = ( remove(vech(t(x)), cusum( 1 || (ncol(t(x)):2) )) )`;

		if nmiss(lowtri) = 0 then call symputx("hasvalueslower", 1);
		if nmiss(uptri) = 0 then call symputx("hasvaluesupper", 1);
		close &data;
	quit;


	%if %sysevalf(&hasvaluesupper) ^= 0 and %sysevalf(&hasvalueslower) ^= 0 %then %do;
		%if %sysevalf(&isnotsymmetric) ^= 1 %then %do;
			%let errmsg = ERROR: When providing a value of TRUE for the is_dist_matrix argument, the dataset must be symmetric or upper triangular or lower triangular.;
			%goto exit2;
		%end;
	%end;
%end;

/* If missing data and completecase was not specified, then we can throw error here and stop early. */
%if %sysevalf(&rowmiss.) ^= 0 %then %do;
	%if %sysfunc(compare(&is_dist_matrix., TRUE, il)) ^= 0 %then %do;
		%if %sysfunc(compare(&completecase., TRUE, il)) ^= 0 %then %do; /* if completecase was not set and missing data was found, then display error. Note this is case-insensitive comparison and removes leading blanks (if any). */
			%let errmsg = ERROR: The data set &data. has missing values and the completecase parameter was not set. To perform your test with complete case analysis, set COMPLETECASE = TRUE. Please note the implications of ignoring missing data are unknown.;
			%goto exit2;
		%end;
	%end;
%end;

/* If we reach this point in the code, then either the data set is complete (no missing values) or the user specified COMPLETECASE = TRUE.
In both situations, we only keep the complete cases */
data clust_completecase;
	set &data;
	if nmiss(of _NUMERIC_)=0;
run;


/* ===============================================
================ Parameter checks ================ 
================================================== */
/* Check for correct test. This is up here because several other things below rely on the &test being valid */
%if not( %sysfunc(upcase(&test.)) in (DIP SILVERMAN)) %then %do;
	%let errmsg = ERROR: Invalid test was specified. Please set either TEST = DIP or TEST = SILVERMAN.;
	%goto exit2;
%end;


/* Reduction method error checking */
%if not( %sysfunc(upcase(&reduction.)) in (PCA DISTANCE NONE)) %then %do;
	%let warncount = %eval(&warncount. + 1);
	%let warnmsg_l = &warnmsg_l. WARNING: Invalid reduction method was entered. No reduction was performed. Please use REDUCTION = PCA, DISTANCE, or NONE. (NONE is only acceptable for data containing only one variable.)%str(;);
	%let warnmsg_w = &warnmsg_w. WARNING: Invalid reduction method was entered. No reduction was performed. Please use REDUCTION = PCA, DISTANCE, or NONE. (NONE is only acceptable for data containing only one variable.)%str(;);
%end;


/* Distance metric error checking */
/* USER NOTE: To add a new distance metric, please scroll down to the %clust_checkmetric definition and add it inside the list */
%if %sysfunc(compare(&reduction., DISTANCE, il)) = 0 %then %do;
	%let metrics_flag = 0;
	%clust_checkmetric(&distance_metric.);
	%if &metrics_flag. ^= 0 %then %do;
		%let warncount = %eval(&warncount. + 1);
		%let warnmsg_l = &warnmsg_l. WARNING: Invalid distance metric was entered. The default metric (EUCLID) will be used. Please see the paper for a list of valid metrics.%str(;);
		%let warnmsg_w = &warnmsg_w. WARNING: Invalid distance metric was entered. The default metric (EUCLID) will be used. Please see the paper for a list of valid metrics.%str(;);
		%let distance_metric = EUCLID;
	%end;

	/* Distance_Standardize checking, since we're already in the DISTANCE %if block anyway */
	/* USER NOTE: To add a new standardization technique, please scroll down to the %clust_checkstdize definition and add it inside the list */
	%let std_method_flag = 0;
	%clust_checkstdize(&distance_standardize);
	%if &std_method_flag. ^= 0 %then %do;
		%let warncount = %eval(&warncount. + 1);
		%let warnmsg_l = &warnmsg_l. WARNING: Invalid standardization method was entered. The default method (STD) will be used. Please see the paper for a list of valid standardization methods.%str(;);
		%let warnmsg_w = &warnmsg_w. WARNING: Invalid standardization method was entered. The default method (STD) will be used. Please see the paper for a list of valid standardization methods.%str(;);
		%let distance_standardize = STD;
	%end;
%end;

/* Warning message printed if PCA specified AND the data set is 1-dimensional.
This is done out here rather than inside dip or silverman because it involves checking the original data set, while 
dip and silverman work with the reduced data set. Less confusion. */
%let original_numvars = %sysfunc(attrn(&dsidc., nvars));
%if (&original_numvars. = 1) and (%sysfunc(compare(&reduction., PCA, il)) = 0) %then %do;
	%let warncount = %eval(&warncount. + 1);
	%let warnmsg_l = &warnmsg_l. WARNING: The data set &data. is univariate, but PCA was specified (or the default was selected) for dimension reduction. Performing PCA on a one-dimensional dataset is not recommended (CITE). Consider rerunning and setting the REDUCTION parameter to NONE.%str(;);
	%let warnmsg_w = &warnmsg_w. WARNING: The data set &data. is univariate, but PCA was specified (or the default was selected) for dimension reduction. Performing PCA on a one-dimensional dataset is not recommended (CITE). Consider rerunning and setting the REDUCTION parameter to NONE.%str(;);
%end;

/* Check for pca_center and pca_scale valid values */
%if not(%sysfunc(upcase(&pca_center.)) in (FALSE TRUE)) %then %do;
	%let pca_center = TRUE;
	%let warncount = %eval(&warncount. + 1);
	%let warnmsg_l = &warnmsg_l. WARNING- NOTE: pca_center must be set to TRUE or FALSE. Continuing using default value of TRUE.%str(;);
	%let warnmsg_w = &warnmsg_w. NOTE: pca_center must be set to TRUE or FALSE. Continuing using default value of TRUE.%str(;);
%end;
%else %do; /* If this is not uppercase before it is sent to R, it will arrive as the wrong data type */
	%let pca_center = %sysfunc(upcase(&pca_center.));
%end;

%if not(%sysfunc(upcase(&pca_scale)) in (FALSE TRUE)) %then %do;
	%let pca_scale = TRUE;
	%let warncount = %eval(&warncount. + 1);
	%let warnmsg_l = &warnmsg_l. WARNING- NOTE: pca_scale must be set to TRUE or FALSE. Continuing using default value of TRUE.%str(;);
	%let warnmsg_w = &warnmsg_w. NOTE: pca_scale must be set to TRUE or FALSE. Continuing using default value of TRUE.%str(;);
%end;
%else %do; /* If this is not uppercase before it is sent to R, it will arrive as the wrong data type */
	%let pca_scale = %sysfunc(upcase(&pca_scale.));
%end;




/* ===============================================
== Check if user supplied superfluous arguments == 
================================================== */
/* Get the macro call as a string */
%let trimcall = %sysfunc(compress(%superq(syspbuff),,s));

/* Initial screening. This is much cheaper than running through the entire process, so if we can rule out superfluous variables right here, then we can skip the rest of the code */
%if %sysfunc(compare(&test, SILVERMAN, il)) = 0 %then %do;
	%let quickargcheck = %sysfunc(index(&trimcall., d_));
%end;
%else %do;
	%let quickargcheck = %sysfunc(index(&trimcall., s_));
%end;

/* Only if they failed the initial screening do we go and find out exactly what they specified that was superfluous */
%if &quickargcheck. > 0 %then %do;
	/* Depending on which test was used, that will dictate which (superfluous) arguments we're searching for */
	/* The = sign is part of the regex to prevent a false positive if the user has s_ or d_ as part of their own data set name, libref, etc. */
	%if %sysfunc(compare(&test., SILVERMAN, il)) = 0 %then %do;
		%let regex = prxparse('/d_[a-z]+=/');
	%end;
	%else %do;
		%let regex = prxparse('/s_[a-z]+=/');
	%end;

	%let excess =;

	/* Parse through the original macro call. A list of superfluous variables is stored in the macro variable &excess */
	data _null_;
	   ExpressionID = &regex.;
	   text = "&trimcall";
	   start = 1;
	   stop = length(text);
	   call prxnext(ExpressionID, start, stop, text, position, length);
	      do while (position > 0);
	         found = substr(text, position, length);
			 call symputx('excess', catx(', ', symget("excess"), substr(found, 1, length(found) - 1)));
	         call prxnext(ExpressionID, start, stop, text, position, length);
	      end;
	run;

	/* If at least one superfluous variable was found, then warn the user */
	%if %length(&excess.) > 0 %then %do;
		%let warncount = %eval(&warncount. + 1);
		%let warnmsg_l = &warnmsg_l. WARNING: The %sysfunc(propcase(&test.)) test was specified, but the macro was called with the following unnecessary arguments: &excess. %str(;);
		%let warnmsg_w = &warnmsg_w. WARNING: The %sysfunc(propcase(&test.)) test was specified, but the macro was called with the following unnecessary arguments: &excess. %str(;);
	%end;
%end;

/* Another superfluous argument situation is to specify "s_digits=" when the Silverman Test is not adjusted.
Since only searching for one thing, we don't need regex. Nesting like this can reduce unnecessary string operations in the non-Silverman cases. */
%if %sysfunc(compare(&test., SILVERMAN, il)) = 0 %then %do;
	%if (%sysfunc(index(&trimcall., %str(s_digits=))) > 0) and (%sysfunc(compare(&s_adjust., FALSE, il)) = 0) %then %do;
		%let warncount = %eval(&warncount. + 1);
		%let warnmsg_l = &warnmsg_l. WARNING: The Silverman Test implementation does not round digits unless using the adjustment from Hall and York. Since you did not use the adjustment, the s_digits argument will be ignored. %str(;);
		%let warnmsg_w = &warnmsg_w. WARNING: The Silverman Test implementation does not round digits unless using the adjustment from Hall and York. Since you did not use the adjustment, the s_digits argument will be ignored. %str(;);
	%end;
%end;
/* Another superfluous argument situation is to specify "distance_metric" or "distance_standardize" when the reduction
method is not pairwise distances. Searching for two things, but will avoid data step and regex to save on complexity */
%if %sysfunc(compare(&reduction., DISTANCE, il)) ^= 0 %then %do;
	%if (%sysfunc(index(&trimcall., %str(distance_metric=))) > 0) or (%sysfunc(index(&trimcall., %str(distance_standardize=))) > 0) %then %do;
		%let warncount = %eval(&warncount. + 1);
		%let warnmsg_l = &warnmsg_l. WARNING: When using REDUCTION = %sysfunc(upcase(&reduction.)), the distance_standardize and distance_metric arguments are ignored, since they are only used when REDUCTION = DISTANCE. %str(;);
		%let warnmsg_w = &warnmsg_w. WARNING: When using REDUCTION = %sysfunc(upcase(&reduction.)), the distance_standardize and distance_metric arguments are ignored, since they are only used when REDUCTION = DISTANCE. %str(;);
	%end;
%end;



/* ===============================================
================ Data Reduction ================== 
================================================== */
/* PCA */
ods select none; /* Prevent a bunch of output on the window and log */
%if %sysfunc(compare(&reduction., PCA, il)) = 0 %then %do;

	/* Check if the user wants to do centering, scaling, both, or neither */
	/* Center and scale (default) */
	%if %sysfunc(compare(&pca_center., TRUE, il)) = 0 and %sysfunc(compare(&pca_scale., TRUE, il)) = 0 %then %do;
		proc princomp data = clust_completecase out = clust_pca outstat = clust_pca_stat;
		run;
	%end; /* Don't center or scale */
	%else %if %sysfunc(compare(&pca_center., TRUE, il)) ~= 0 and %sysfunc(compare(&pca_scale., TRUE, il)) ~= 0 %then %do;
		proc princomp data = clust_completecase out = clust_pca cov noint outstat = clust_pca_stat;
		run;
	%end; /* Center and don't scale */
	%else %if %sysfunc(compare(&pca_center., TRUE, il)) = 0 and %sysfunc(compare(&pca_scale., TRUE, il)) ~= 0 %then %do;
		proc princomp data = clust_completecase out = clust_pca cov outstat = clust_pca_stat;
		run;
	%end; /* Scale and don't center */
	%else %if %sysfunc(compare(&pca_center., TRUE, il)) ~= 0 and %sysfunc(compare(&pca_scale., TRUE, il)) = 0 %then %do;
		proc princomp data = clust_completecase out = clust_pca noint outstat = clust_pca_stat;
		run;
	%end;

	/* Because signs of first principal component can differ between PCA implementations or computer, multiply scores
	by -1 if the first loading is negative. This should allow for consistent results. */

	data _NULL_;
		set clust_pca_stat;
		where _NAME_ = "Prin1";
		array _n _numeric_;
		call symputx("clust_pcasn", _n[1]);
	run;

	%if %sysevalf(&clust_pcasn) < 0 %then %do;
		data clust_data2;
			set clust_pca;
			keep Prin1;
			Prin1_2 = -1 * Prin1;
			keep Prin1_2;
		run;
	%end;
	%else %do;
		data clust_data2;
			set clust_pca;
			keep Prin1;
		run;
	%end;
%end;

/* Pairwise Distances */
%else %if %sysfunc(compare(&reduction., DISTANCE, il)) = 0 %then %do;
	/* If no standardization is requested, then use this proc distance */
	%if %sysfunc(compare(%bquote(&distance_standardize.), NONE, il)) = 0 %then %do;
		proc distance data = clust_completecase method = &distance_metric. out = clust_dist nostd;
			var interval(_ALL_);
		run;
	%end;
	%else %do; /* If standardization is requested, then use this other proc distance */
		proc distance data = clust_completecase method = &distance_metric. out = clust_dist;
			var interval(_ALL_ / std = &distance_standardize.);
		run;
	%end;

	/* Extract the distances from below the diagonal of this lower-triangular distance matrix */
	proc iml;
		use work.clust_dist;
		read all var _ALL_ into x;
   		y = ( remove(vech(X), cusum( 1 || (ncol(X):2) )) )`;
		create clust_data2 from y[colname = 'y'];
		append from y;
		close work.clust_dist;
	quit;
%end;
/* Otherwise */
%else %do;
	%if %sysfunc(compare(&is_dist_matrix., TRUE, il)) = 0 %then %do;
		/* A distance matrix was provided, so extract the lower triangular portion. */
		proc iml;
			use &data;
			read all var _ALL_ into x;
   			y = ( remove(vech(X), cusum( 1 || (ncol(X):2) )) )`;
			create clust_data2 from y[colname = 'y'];
			append from y;
			close &data;
		quit;
	%end;
	%else %do;
		/* For internal compatibility with the output from PCA and DISTANCE methods */
		data clust_data2;
			set clust_completecase;
		run;
	%end;
%end;
ods select all;

/* ===============================================
================ Perform Test ==================== 
================================================== */
/* Perform the test */
%if %sysfunc(compare(&test., SILVERMAN, il)) = 0 %then %do;
	%silverman(silvData = clust_data2, k = 1, M = &s_M., adjust = &s_adjust., digits = &s_digits., setSeed = &s_setSeed., 
		out = &out., completecase = &completecase., include = &folder.);
%end; 
%else %if %sysfunc(compare(&test., DIP, il)) = 0 %then %do;
	%dip(dipData = clust_data2, simulatepvalue = &d_simulatepvalue., reps = &d_reps., out = &out., 
		completecase = &completecase., include = &folder.);
%end;


/* ===============================================
================ Cleanup =========================
================================================== */
%exit2: /* Exit macro */
%if %symexist(dsid) %then %do; /* If a data set was opened (dip/silverman checks), close it. */
	%if &dsid. > 0 %then %do; 
		%let rc = %sysfunc(close(&dsid.));
	%end;
%end;

%if %symexist(dsidc) %then %do; /* If a second data set was opened (clusterability dimension check), close it. */
	%if &dsidc. > 0 %then %do; 
		%let rc = %sysfunc(close(&dsidc.));
	%end;
%end;

/* Print error message if applicable */
%if %symexist(errmsg) %then %do;
	title Clusterability Test: ERROR;
	%put &errmsg.;
	data _NULL_;
		file print;
		put "&errmsg";
	run;
%end;

title; /* Reset title */

%mend clusterability;







%macro dip(dipdata = , simulatepvalue = FALSE, reps = 2000, out = , completecase = FALSE, include = );
/* dipdata: a SAS data set to use in the analysis.
   simulatepvalue: whether or not to simulate pvalues by Monte Carlo simulation. Either FALSE or TRUE.
   reps: an integer specifying the number of replicates used in the Monte Carlo test.
   out: the name of a SAS data set to store the result from Dip test
   completecase: whether to use complete case analysis. Either FALSE or TRUE.
   include: a fileref associated with the folder containing the dip.sas file  */

/* ===============================================
============ Data set/Folder checks ==============
================================================== */
/* Verify data set exists */
%if not %sysfunc(exist(&dipdata.)) %then %do;
	%let errmsg = ERROR: Data set &data. does not exist. Make sure the data set name is spelled correctly.;
	%goto exit;
%end;

/* Verify file path is valid and was specified */
/* First, check if specified at all. Second, check if folder/file exists */
%if %sysevalf(%superq(include)=,boolean) %then %do;
	%let errmsg = ERROR: Folder path for FOLDER parameter is not valid. Please provide a valid SAS fileref to the folder containing the dip.sas and silverman.sas files.;
	%goto exit;
%end;
%else %if not %sysfunc(fexist(&include)) %then %do;
	%let errmsg = ERROR: Folder path for FOLDER parameter is not valid. Please provide a valid SAS fileref to the folder containing the dip.sas and silverman.sas files.;
	%goto exit;
%end;

/* Check if dip.sas file located within the folder. Check both \ and / just in case */
%let fileloc1 = %sysfunc(pathname(&include.))\dip.sas;
%if %sysfunc(fileexist(&fileloc1.)) = 0 %then %do;
	%let fileloc2 = %sysfunc(pathname(&include.))/dip.sas;
	%if %sysfunc(fileexist(&fileloc2.)) = 0 %then %do;
		%let errmsg = ERROR: Folder path for FOLDER parameter is valid but the dip.sas file could not be found. Please provide a valid SAS fileref to the folder containing the dip.sas and silverman.sas files.;
		%goto exit;
	%end;
%end;

/* Check for zero-observation dataset or other issues with the data set. */
%let dsid = %sysfunc(open(&dipdata., i));
%let obs1 = %sysfunc(fetchobs(&dsid., 1));

%if &obs1. = -1 %then %do;
	%let errmsg = ERROR: Data set &data. has 0 observations.;
	%goto exit;
%end;

%if &obs1. > 0 %then %do;
	%let errmsg = %sysfunc(sysmsg());
	%goto exit;
%end;

/* Check for non-numeric data or multidimensional data. The data should have been reduced and screened for non-numeric data in %clusterability() before it arrives here. */
%let numvars = %sysfunc(attrn(&dsid., nvars));
%if &numvars. > 1 %then %do;
	%let errmsg = ERROR: The dataset &data. has multiple dimensions. These methods were not designed for multivariate data. Principal Component Analysis and Pairwise Distance methods are available by specifying REDUCTION = PCA or REDUCTION = DISTANCE (for pairwise distances, a distance metric is required, with the default as Euclidean) in the macro call.;
	%goto exit;
%end;
%if %sysfunc(vartype(&dsid., 1)) = C %then %do;
	%let errmsg = ERROR: Non-numeric data was found in the data set. These methods were not designed for non-numeric data.;
	%goto exit;
%end;

/* Checking for missing data */
proc iml;
	use &dipdata.;
	read all into x;
	close &dipdata.;
	nmissing = countmiss(x);
	if nmissing > 0 then call symputx("nmiss", 1);
	else call symputx("nmiss", 0);
quit;

/* rowmiss was defined in %clusterability() */
%if %sysevalf(&nmiss.) ~= 0 %then %do;
	%if %sysfunc(compare(&completecase., TRUE, il)) ~= 0 %then %do; /* if completecase was not set and missing data was found, then display error */
		%let errmsg = ERROR: The data set &data. has missing values and the completecase parameter was not set. To perform the Dip Test with complete case analysis, set COMPLETECASE = TRUE.;
		%goto exit;
	%end;
	%else %do; /* if completecase = TRUE was set and missing data found, then display note */
		%let warncount = %eval(&warncount. + 1);
		%let warnmsg_l = &warnmsg_l. NOTE: A complete case analysis was used when performing the Dip Test.%str(;);
		%let warnmsg_w = &warnmsg_w. NOTE: A complete case analysis was used when performing the Dip Test.%str(;);
	%end;
%end;

/* Remove missing observations. Placement of DATA step here is necessary to ensure dipData2 is defined when we
call ExportDataSetToR in dip.sas. If no missing observations, then data set will be unchanged. */
data dipData2;
	set &dipData.;
	if nmiss(of _NUMERIC_)=0;
run;

/* ===============================================
================ Parameter checks ================= 
================================================== */
/* Ensure valid value for simulatepvalue */
%if not(%sysfunc(upcase(&simulatepvalue.)) in (FALSE TRUE)) %then %do;
	%let simulatepvalue = FALSE;
	%let warncount = %eval(&warncount. + 1);
	%let warnmsg_l = &warnmsg_l. WARNING- NOTE: d_simulatePvalue must be set to FALSE or TRUE. Continuing using default value of FALSE.%str(;);
	%let warnmsg_w = &warnmsg_w. NOTE: d_simulatePvalue must be set to FALSE or TRUE. Continuing using default value of FALSE.%str(;);
%end;
%else %do; /* If this is not uppercase before it is sent to R, it will arrive as the wrong data type */
	%let simulatepvalue = %sysfunc(upcase(&simulatepvalue.));
%end;


/* Ensure valid value for reps */
/* Had to arrange this way because short circuit evaluation for the if statement wasn't happening */
%if %sysfunc(notdigit(&reps.)) ^= 0 %then %do;
	%let reps = 2000;
	%let warncount = %eval(&warncount + 1);
	%let warnmsg_l = &warnmsg_l. WARNING- NOTE: d_reps must be a positive integer. Continuing using default value of 2000.%str(;);
	%let warnmsg_w = &warnmsg_w. NOTE: d_reps must be a positive integer. Continuing using default value of 2000.%str(;);
%end;
%else %if not(%sysfunc(mod(&reps., 1)) = 0 and &reps > 0) %then %do;
	%let reps = 2000;
	%let warncount = %eval(&warncount. + 1);
	%let warnmsg_l = &warnmsg_l. WARNING- NOTE: d_reps must be a positive integer. Continuing using default value of 2000.%str(;);
	%let warnmsg_w = &warnmsg_w. NOTE: d_reps must be a positive integer. Continuing using default value of 2000.%str(;);
%end;

/* Setup for out data set */
%if not %sysevalf(%superq(out)=,boolean) %then %do;
	%let out1 = %str("&&out");
%end;
%else %do;
	%let out1 = %str("NULL");
%end;

/* ===============================================
================ Warning Messages ================ 
================================================== */
/* Print warning messages. In most cases, there will be either warnings or errors, but not both. */
%if &warncount. > 0 %then %do;
	title Clusterability Test: Warnings;
	data _NULL_;
	/* Print to log */
		do i = 1 to &warncount.;
			word2 = scan("&warnmsg_l.", i, ";");
			put word2;
			put;
		end;
	/* Print to window */
	file print;
	array x[50] $100;
		do i = 1 to &warncount.;
			word2 = scan("&warnmsg_w.", i, ";");

			k = 1;
			do j = 1 by 1 while(scan(word2, k, " ") ^= "");
				x[j] = scan(word2, j, " ");
				k = k + 1;
			end;

			put x{*};

			do m = 1 to 50;
				x[m] = "";
			end;
		end;
	run;
%end;


/* ===============================================
================ Perform Test ==================== 
================================================== */
/* Titles make output more readable */
title Clusterability Test: &&data;

%include &folder.(dip.sas);


/* ===============================================
================ Cleanup =========================
================================================== */
%exit: /* Exit macro */
%if %symexist(dsid) %then %do; /* If a data set was opened, close it. */
	%if &dsid. > 0 %then %do; 
		%let rc = %sysfunc(close(&dsid.));
	%end;
%end;


/* Print error message if applicable */
%if %symexist(errmsg) %then %do;
	title Clusterability Test: ERROR;
	%put &errmsg.;
	data _NULL_;
		file print;
			array x[50] $100;
			k = 1;
			do j = 1 by 1 while(scan("&errmsg", k, " ") ^= "");
				x[j] = scan("&errmsg", j, " ");
				k = k + 1;
			end;

			put x{*};

			do m = 1 to 50;
				x[m] = "";
			end;
	run;
%end;

title; /* Reset title */
%mend dip;








%macro silverman(silvData = , k = 1, M = 999, adjust = TRUE, digits = 6, setSeed = , out = , completecase = FALSE, include = );
/* silvData: a SAS data set to use in the analysis.
   k: number of modes to be tested. H0: number of modes <= k. For clusterability tests, this is always 1.
   M: number of bootstrap replications.
   adjust: boolean (either FALSE or TRUE) to activate the adjusting of the p-value (valid only if k=1) (see Hall and York)
   digits: number of digits of the p-value when it is printed. Only applicable when adjust = 1. The full-precision p-value is always stored in any output datasets.
   setseed: a seed to be passed as an argument to set.seed().
   out: a SAS data set where output from the Silverman test will be stored. 
   completecase: whether to use complete case analysis. Either TRUE or FALSE.
   include: a fileref associated with the folder containing the silverman.sas file */

/* ===============================================
================ Data Set/Folder Checks ========== 
================================================== */
/* Verify data set exists */
%if not %sysfunc(exist(&silvdata.)) %then %do;
	%let errmsg = ERROR: Data set &data. does not exist. Make sure the data set name is spelled correctly.;
	%goto exit;
%end;

/* Verify file path is valid and was specified */
/* First, check if specified at all. Second, check if folder/file exists */
%if %sysevalf(%superq(include)=,boolean) %then %do;
	%let errmsg = ERROR: Folder path for FOLDER parameter is not valid. Please provide a valid SAS fileref to the folder containing the dip.sas and silverman.sas files.;
	%goto exit;
%end;
%else %if not %sysfunc(fexist(&include.)) %then %do;
	%let errmsg = ERROR: Folder path for FOLDER parameter is not valid. Please provide a valid SAS fileref to the folder containing the dip.sas and silverman.sas files.;
	%goto exit;
%end;

/* Check if silverman.sas file located within the folder. Check both \ and / just in case */
%let fileloc1 = %sysfunc(pathname(&include.))\silverman.sas;
%if %sysfunc(fileexist(&fileloc1.)) = 0 %then %do;
	%let fileloc2 = %sysfunc(pathname(&include.))/silverman.sas;
	%if %sysfunc(fileexist(&fileloc2.)) = 0 %then %do;
		%let errmsg = ERROR: Folder path for FOLDER parameter is valid but the silverman.sas file could not be found. Please provide a valid SAS fileref to the folder containing the dip.sas and silverman.sas files.;
		%goto exit;
	%end;
%end;

/* Check for zero-observation dataset or other data set problems. */
%let dsid = %sysfunc(open(&silvData., i));
%let obs1 = %sysfunc(fetchobs(&dsid., 1));

%if &obs1. = -1 %then %do;
	%let errmsg = ERROR: Data set &data. has 0 observations.;
	%goto exit;
%end;

%if &obs1. > 0 %then %do;
	%let errmsg = %sysfunc(sysmsg());
	%goto exit;
%end;


/* Check for non-numeric data or multidimensional data. The data should have been reduced and screened for non-numeric data before it reaches this point. */
%let numvars = %sysfunc(attrn(&dsid., nvars));
%if &numvars. > 1 %then %do;
	%let errmsg = ERROR: The dataset &data. has multiple dimensions. These methods were not designed for multivariate data. Principal Component Analysis and Pairwise Distance methods are available by specifying REDUCTION = PCA or REDUCTION = DISTANCE (for pairwise distances, a distance metric is required, with the default as Euclidean) in the macro call.;
	%goto exit;
%end;

%if %sysfunc(vartype(&dsid., 1)) = C %then %do;
	%let errmsg = ERROR: Non-numeric data was found in the data set. These methods were not designed for non-numeric data.;
	%goto exit;
%end;

/* Checking for missing data. rowmiss was defined in %clusterability */
proc iml;
	use &silvdata.;
	read all into x;
	close &silvdata.;
	nmissing = countmiss(x);
	if nmissing > 0 then call symputx("nmiss", 1);
	else call symputx("nmiss", 0);
quit;

%if %sysevalf(&nmiss.) ~= 0 %then %do;
	%if %sysfunc(compare(&completecase., TRUE, i)) ~= 0 %then %do; /* if completecase was not set and missing data was found, then display error */
		%let errmsg = ERROR: The data set &data. has missing values and the completecase parameter was not set. To perform the Silverman Test with complete case analysis, set completecase = TRUE.;
		%goto exit;
	%end;
	%else %do; /* if completecase = TRUE was set and missing data found, display note */
		%let warncount = %eval(&warncount. + 1);
		%let warnmsg_l = &warnmsg_l. NOTE: A complete case analysis was used when performing the Silverman Test.%str(;);
		%let warnmsg_w = &warnmsg_w. NOTE: A complete case analysis was used when performing the Silverman Test.%str(;);
	%end;
%end;

/* Remove missing observations. Placement of DATA step here is necessary to ensure silvData2 is defined when we
call ExportDataSetToR in silverman.sas. If no missing observations, then data set will be unchanged. */
data silvData2;
	set &silvData.;
	if nmiss(of _NUMERIC_)=0;
run;

/* ===============================================
================ Parameter checks ================ 
================================================== */
/* Check for valid values of k. Lack of short-circuit evaluation requires we structure it like this. */
%if %sysfunc(notdigit(&k.)) ^= 0 %then %do;
	%let k = 1;
	%let warncount = %eval(&warncount. + 1);
	%let warnmsg_l = &warnmsg_l. WARNING- NOTE: s_k must be a positive integer. Continuing using default value of 1.%str(;);
	%let warnmsg_w = &warnmsg_w. NOTE: s_k must be a positive integer. Continuing using default value of 1.%str(;);
%end;
%else %if not(%sysfunc(mod(&k., 1)) = 0 and &k. > 0) %then %do;
	%let k = 1;
	%let warncount = %eval(&warncount. + 1);
	%let warnmsg_l = &warnmsg_l. WARNING- NOTE: s_k must be a positive integer. Continuing using default value of 1.%str(;);
	%let warnmsg_w = &warnmsg_w. NOTE: s_k must be a positive integer. Continuing using default value of 1.%str(;);
%end;

/* Ensure valid value for M */
%if %sysfunc(notdigit(&M.)) ^= 0 %then %do;
	%let M = 999;
	%let warncount = %eval(&warncount. + 1);
	%let warnmsg_l = &warnmsg_l. WARNING- NOTE: s_M must be a positive integer. Continuing using default value of 999.%str(;);
	%let warnmsg_w = &warnmsg_w. NOTE: s_M must be a positive integer. Continuing using default value of 999.%str(;);
%end;
%else %if not(%sysfunc(mod(&M., 1)) = 0 and &M. > 0) %then %do;
	%let M = 999;
	%let warncount = %eval(&warncount. + 1);
	%let warnmsg_l = &warnmsg_l. WARNING- NOTE: s_M must be a positive integer. Continuing using default value of 999.%str(;);
	%let warnmsg_w = &warnmsg_w. NOTE: s_M must be a positive integer. Continuing using default value of 999.%str(;);
%end;

/* Ensure valid value for adjust */
%if not(%sysfunc(upcase(&adjust.)) in (FALSE TRUE)) %then %do;
	%let adjust = TRUE;
	%let warncount = %eval(&warncount. + 1);
	%let warnmsg_l = &warnmsg_l. WARNING- NOTE: s_adjust must be set to TRUE or FALSE. Continuing using default value of TRUE.%str(;);
	%let warnmsg_w = &warnmsg_w. NOTE: s_adjust must be set to TRUE or FALSE. Continuing using default value of TRUE.%str(;);
%end;
%else %do; /* If this is not uppercase before it is sent to R, it will arrive as the wrong data type */
	%let adjust = %sysfunc(upcase(&adjust.));
%end;

/* Ensure valid value for digits */
%if %sysfunc(notdigit(&digits.)) ^= 0 %then %do;
	%let digits = 6;
	%let warncount = %eval(&warncount. + 1);
	%let warnmsg_l = &warnmsg_l. WARNING- NOTE: s_digits must be a positive integer. Continuing using default value of 6.%str(;);
	%let warnmsg_w = &warnmsg_w. NOTE: s_digits must be a positive integer. Continuing using default value of 6.%str(;);
%end;
%else %if not(%sysfunc(mod(&digits., 1)) = 0 and &digits. > 0) %then %do;
	%let digits = 6;
	%let warncount = %eval(&warncount. + 1);
	%let warnmsg_l = &warnmsg_l. WARNING- NOTE: s_digits must be a positive integer. Continuing using default value of 6.%str(;);
	%let warnmsg_w = &warnmsg_w. NOTE: s_digits must be a positive integer. Continuing using default value of 6.%str(;);
%end;


/* Check for valid values for setSeed */
/* First %if checks is the argument is null, and sets to "NULL" if it is. Makes handling in R easier if we do this. */
%if %sysevalf(%superq(setSeed)=,boolean) %then %do;
	%let setSeed = %str("NULL");
%end;
%else %if (%sysfunc(notdigit(&setSeed.)) ^= 0) and (%sysfunc(compare(&setSeed., "NULL")) ^= 0) %then %do;
	%let setSeed = %str("NULL");
	%let warncount = %eval(&warncount. + 1);
	%let warnmsg_l = &warnmsg_l. WARNING- NOTE: s_setSeed must be an integer. The seed will not be set.%str(;);
	%let warnmsg_w = &warnmsg_w. NOTE: s_setSeed must be an integer. The seed will not be set.%str(;);
%end;

/* Print warning if k = 1 and unadjusted test used */
%if %sysfunc(compare(&adjust., FALSE, il)) = 0 and &k. = 1 %then %do;
	%let warncount = %eval(&warncount. + 1);
	%let warnmsg_l = &warnmsg_l. WARNING: When s_k = 1, the adjusted test by Hall and York (2001) has better asymptotic accuracy than the unadjusted test. Your implementation is unadjusted. To use the adjusted test, change the parameter to s_adjust = TRUE.%str(;);
	%let warnmsg_w = &warnmsg_w. WARNING: When s_k = 1, the adjusted test by Hall and York (2001) has better asymptotic accuracy than the unadjusted test. Your implementation is unadjusted. To use the adjusted test, change the parameter to s_adjust = TRUE.%str(;);
%end;

/* Print warning if k > 1, and additional warning if they tried to adjust also. */
%if &k. > 1 %then %do;
	%let warncount = %eval(&warncount. + 1);
	%let warnmsg_l = &warnmsg_l. WARNING: You provided s_k %str(=) &k., which corresponds to testing %str(<=) &k. modes. In testing for clusterability, the interpretation of this test is questionable, and its applicability is unknown. %str(;);
	%let warnmsg_w = &warnmsg_w. WARNING: You provided s_k %str(=) &k., which corresponds to testing %str(<=) &k. modes. In testing for clusterability, the interpretation of this test is questionable, and its applicability is unknown. %str(;);

	/* Print warning if k > 1 and adjusted test (attempted) to be used. Adjustment is only available for k = 1 */
	%if %sysfunc(compare(&adjust., TRUE, il)) = 0 %then %do;
		%let warncount = %eval(&warncount. + 1);
		%let warnmsg_l = &warnmsg_l. WARNING: The adjustment to the Silverman Test (using work from Hall and York) is only available when s_k = 1. You provided s_k %str(=) &k, so the adjustment will not be performed. %str(;);
		%let warnmsg_w = &warnmsg_w. WARNING: The adjustment to the Silverman Test (using work from Hall and York) is only available when s_k = 1. You provided s_k %str(=) &k, so the adjustment will not be performed. %str(;);
		%let adjust = FALSE;
	%end;
%end;

/* Setup for out data set */
%if not %sysevalf(%superq(out)=,boolean) %then %do;
	%let out1 = %str("&&out");
%end;
%else %do;
	%let out1 = %str("NULL");
%end;

/* ===============================================
================ Warnings ======================== 
================================================== */

/* Print warning messages. In most cases, there will be either warnings or errors, but not both. */
%if &warncount. > 0 %then %do;
	title Clusterability Test: Warnings;
	data _NULL_;
	/* Print to log */
		do i = 1 to &warncount.;
			word2 = scan("&warnmsg_l.", i, ";");
			put word2;
			put;
		end;
	/* Print to window */
	file print;
	array x[50] $100;
		do i = 1 to &warncount.;
			word2 = scan("&warnmsg_w.", i, ";");

			k = 1;
			do j = 1 by 1 while(scan(word2, k, " ") ^= "");
				x[j] = scan(word2, j, " ");
				k = k + 1;
			end;

			put x{*};

			do m = 1 to 50;
				x[m] = "";
			end;
		end;
	run;
%end;
/* ===============================================
================ Perform Test ==================== 
================================================== */
/* Titles make output more readable */
title Clusterability Test: &&data;

/* Execute the setup + R code */
%include &include.(silverman.sas);


/* ===============================================
================ Cleanup ========================= 
================================================== */
%exit: /* Exit macro */
%if %symexist(dsid) %then %do; /* If a data set was opened, close it. */
	%if &dsid. > 0 %then %do; 
		%let rc = %sysfunc(close(&dsid.));
	%end;
%end;

/* Print error message if applicable */
%if %symexist(errmsg) %then %do;
	title Clusterability Test: ERROR;
	%put &errmsg.;
	data _NULL_;
		file print;
			array x[50] $100;
			k = 1;
			do j = 1 by 1 while(scan("&errmsg", k, " ") ^= "");
				x[j] = scan("&errmsg", j, " ");
				k = k + 1;
			end;

			put x{*};

			do m = 1 to 50;
				x[m] = "";
			end;
	run;
%end;

title; /* Reset title */
%mend silverman;




/* Utility macro to check the distance_metric argument */
%macro clust_checkmetric(dmetric);
	/* Convert to uppercase and compress to ensure comparisons are done correctly */
	%let comp_metric = %sysfunc(compress(%upcase("&dmetric")));
	/* Check if it's a valid metric that doesn't require additional arguments.
	USER NOTE: You can add or remove metrics in the list on the following line. */
	%if not ( %bquote(&comp_metric.) in ("EUCLID" "SQEUCLID" "SIZE" "SHAPE" "COV" "CORR" "DCORR" "SQCORR" "DSQCORR" "CITYBLOCK" "CHEBYCHEV") ) %then %do;
		/* The L(p) and POWER(p,r) are metrics whose arguments need to be validated */
		data _null_;
			/* Try to match L(p) pattern, where p > 0 */
			regex_lp = prxparse('/L\((([1-9]+\.?[0-9]*)|(0?\.0*[1-9]+[0-9]*))\)/'); 
			lmatch = prxmatch(regex_lp, &comp_metric.);
			call symputx('lmatch', lmatch);

			/* Try to match POWER(p,r) pattern, where p >= 0 and r > 0 */
			regex_ppr = prxparse('/POWER\(([0-9]+\.?[0-9]*),(([1-9]+\.?[0-9]*)|(0?\.0*[1-9]+[0-9]*))\)/');
			pprmatch = prxmatch(regex_ppr, &comp_metric.);
			call symputx('pprmatch', pprmatch);
		run;

		/* If neither POWER(p,r) nor L(p) patterns matched, then metric is invalid */
		%if(&lmatch. = 0 and &pprmatch. = 0) %then %do;
			%let metrics_flag = 1;
		%end;
	%end;
%mend clust_checkmetric;


/* Utility macro to check the distance_standardize argument */
%macro clust_checkstdize(stdize);
	/* Convert to uppercase and compress to ensure comparisons are done correctly */
	%let std_method = %sysfunc(compress(%upcase("&stdize")));
	/* Check if it's a valid standardization method that doesn't require additional arguments.
	USER NOTE: You can add or remove methods in the list on the following line. */
	%if not ( %bquote(&std_method.) in ("NONE" "STD" "MEAN" "MEDIAN" "RANGE" "MIDRANGE" "IQR" "MAD") ) %then %do;
		/* The L(p) method has arguments which need to be validated */
		data _null_;
			/* Try to match L(p) pattern, where p > 0 */
			regex_lp = prxparse('/L\((([1-9]+\.?[0-9]*)|(0?\.0*[1-9]+[0-9]*))\)/'); 
			lmatch = prxmatch(regex_lp, &std_method.);
			call symputx('lmatch', lmatch);
		run;

		/* If L(p) pattern is not matched, then standardization method is invalid */
		%if(&lmatch. = 0) %then %do;
			%let std_method_flag = 1;
		%end;
	%end;
%mend clust_checkstdize;


