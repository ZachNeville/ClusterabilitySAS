/* =========================================
Code for plots in clusterability SAS paper 
========================================== */

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


/* Create data sets to be used in the examples */

/* 2d normals, 1 cluster */
data normals1;
	call streaminit(1234);
	do i = 1 to 150;
		x = rand('normal', 0, 1);
		y = rand('normal', 4, 1);
		color = "CX445694";
		output;
	end;
	drop i;
run;



/* 2d normals, 2 clusters close but not overlapping */
data normals2;
	call streaminit(1234);
	do i = 1 to 75;
		x = rand('normal', -3, 1);
		y = rand('normal', -2, 1);
		color = "CX445694";
		output;
		x = rand('normal', 1, 1);
		y = rand('normal', 1, 1);
		color = "CXA23A2E";
		output;
	end;
	drop i;
run;

/* 2d normals, 3 clusters pretty far apart */
data normals3;
	call streaminit(1234);
	do i = 1 to 50;
		x = rand('normal', 3, 1);
		y = rand('normal', 0, 1);
		color = "CX445694";
		output;
		x = rand('normal', 0, 1);
		y = rand('normal', 3, 1);
		color = "CXA23A2E";
		output;
		x = rand('normal', 3, 1);
		y = rand('normal', 6, 1);
		color = "CX01665E";
		output;
	end;
	drop i;
run;


/* 3d normals, 2 clusters close but still decently spread apart */
data normals4;
	call streaminit(1234);
	do i = 1 to 75;
		x = rand('normal', 1, 1);
		y = rand('normal', 3, 1);
		z = rand('normal', 2, 1);
		shape = "star";
		color = "CX445694";
		output;
		x = rand('normal', 4, 1);
		y = rand('normal', 6, 1);
		z = rand('normal', 0, 1);
		shape = "balloon";
		color = "CXA23A2E";
		output;
	end;
	drop i;
run;


/* 3d normals, 3 clusters close */
data normals5;
	call streaminit(1234);
	do i = 1 to 50;
		x = rand('normal', 1, 1);
		y = rand('normal', 3, 1);
		z = rand('normal', 3, 1);
		shape = "star";
		color = "CX445694";
		output;
		x = rand('normal', 4, 1);
		y = rand('normal', 6, 1);
		z = rand('normal', 0, 1);
		shape = "balloon";
		color = "CXA23A2E";
		output;
		x = rand('normal', 2, 1);
		y = rand('normal', 8, 1);
		z = rand('normal', -3, 1);
		shape = "flag";
		color = "CX01665E";
		output;
	end;
	drop i;
run;

/* cars. From base R software. Original source: Ezekiel, M. (1930) Methods of Correlation Analysis. Wiley. */
data cars;
	input speed dist @@;
	datalines; 
	4 2 4 10 7 4 7 22 8 16 9 10 10 18 10 26 10 34 11 17
	11 28 12 14 12 20 12 24 12 28 13 26 13 34 13 34 13 46 14 26
	14 36 14 60 14 80 15 20 15 26 15 54 16 32 16 40 17 32 17 40
	17 50 18 42 18 56 18 76 18 84 19 36 19 46 19 68 20 32 20 48
	20 52 20 56 20 64 22 66 23 54 24 70 24 92 24 93 24 120 25 85
	;
run;

/* Setup for the plots later on */
proc princomp data = sashelp.iris out = iris_pca;
	var SepalLength SepalWidth PetalLength PetalWidth;
run;

proc distance data = sashelp.iris method = EUCLID out = iris_dist;
	var interval(SepalLength SepalWidth PetalLength PetalWidth / std = STD);
run;

proc iml;
	use work.iris_dist;
	read all var _ALL_ into x;
	y = ( remove(vech(X), cusum( 1 || (ncol(X):2) )) )`;
	create iris_dist2 from y[colname = 'y'];
	append from y;
	close work.iris_dist;
quit;

/* Generate the plots */

/* normals1 */
ods graphics / attrpriority=none;
proc sgplot data = normals1 noautolegend;
	title height = 2.5 font = 'Cambria Math' 'normals1';
	styleattrs
		datacolors = (CX445694 CXA23A2E CX01665E)
		datasymbols = (star circle triangle);
	xaxis label = "x" labelattrs = (Family = Cambria Size = 16);
	yaxis label = "y" labelattrs = (Family = Cambria Size = 16);
	scatter x = x y = y / group = color markerattrs = (size = 10);
run;
ods graphics / reset = attrpriority;

/* normals2 */
ods graphics / attrpriority=none;
proc sgplot data = normals2 noautolegend;
	title height = 2.5 font = 'Cambria Math' 'normals2';
	styleattrs
		datacolors = (CX445694 CXA23A2E CX01665E)
		datasymbols = (star circle triangle);
	xaxis label = "x" labelattrs = (Family = Cambria Size = 16);
	yaxis label = "y" labelattrs = (Family = Cambria Size = 16);
	scatter x = x y = y / group = color markerattrs = (size = 10);
run;
ods graphics / reset = attrpriority;

/* normals3 */
ods graphics / attrpriority=none;
proc sgplot data = normals3 noautolegend;
	title height = 2.5 font = 'Cambria Math' 'normals3';
	styleattrs
		datacolors = (CX445694 CXA23A2E CX01665E)
		datasymbols = (star circle triangle);
	xaxis label = "x" labelattrs = (Family = Cambria Size = 16);
	yaxis label = "y" labelattrs = (Family = Cambria Size = 16);
	scatter x = x y = y / group = color markerattrs = (size = 10);
run;
ods graphics / reset = attrpriority;

/* normals4 */
proc g3d data = normals4;
	goptions hsize = 6in vsize = 6in;
	title font = 'Cambria Math' height = 3.5 move = (35, -15)pct 'normals4';
	scatter x * y = z / noneedle size = 1 color = color rotate = 140 tilt = 40 shape = shape;
run;

/* normals5 */
proc g3d data = normals5;
	goptions hsize = 6in vsize = 5in;
	title font = 'Cambria Math' height = 4.2 'normals5';
	axis1 order = (-1 to 7 by 1);
	axis2 order = (0 to 11 by 1);
	axis3 order = (-5 to 6 by 1);
	scatter x * y = z / noneedle size = 1 color = color rotate = 110 tilt = 87.5 shape = shape 
		xaxis = axis1 yaxis = axis2 zaxis = axis3;
run;

/* iris scatter plot matrix */
ods graphics / attrpriority=none;
proc sgscatter data = sashelp.iris datasymbols = (star circle hash) datacolors = (CX445694 CXA23A2E CX01665E);
	title font = 'Cambria Math' height = 3.5 'iris';
	matrix sepallength sepalwidth petallength petalwidth / group = species;
run;
ods graphics / reset = attrpriority;

/* iris PCA scores */
proc sgplot data = iris_pca;
	title height = 2.5 font = 'Cambria Math' 'iris';
	histogram Prin1 / fillattrs = (color = "CX445694");
	xaxis label = 'First Principal Component Score' labelattrs = (Family = Cambria Size = 16);
	yaxis label = 'Percent' labelattrs = (Family = Cambria Size = 16);
run;

/* iris pairwise distances */
proc sgplot data = iris_dist2;
	title height = 2.5 font = 'Cambria Math' 'iris';
	histogram y / fillattrs = (color = "CXA23A2E");
	xaxis label = 'Pairwise Distance' labelattrs = (Family = Cambria Size = 16);
	yaxis label = 'Percent' labelattrs = (Family = Cambria Size = 16);
run;

/* cars */
proc sgplot data = cars noautolegend;
	title height = 2.5 font = 'Cambria Math' 'cars';
	styleattrs
		datacolors = (CX445694 CXA23A2E CX01665E)
		datasymbols = (star circle plus);
	xaxis label = "speed" labelattrs = (Family = Cambria Size = 16);
	yaxis label = "dist" labelattrs = (Family = Cambria Size = 16);
	scatter x = speed y = dist / markerattrs = (symbol = star size = 10);
run;
