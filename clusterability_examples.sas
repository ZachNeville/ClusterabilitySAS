/* =================================
Examples for %clusterability macro.
The macro needs to be initialized before
running this code. 
================================== */

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



/* NOTE: Please update this to reflect the folder containing the dip.sas and silverman.sas files on your machine */
filename clust "C:\Documents\Clusterability\";



/* Create data sets to be used in the examples */
/* 2d normals, 1 cluster */
data normals1;
	call streaminit(1234);
	do i = 1 to 150;
		x = rand('normal', 0, 1);
		y = rand('normal', 4, 1);
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
		output;
		x = rand('normal', 1, 1);
		y = rand('normal', 1, 1);
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
		output;
		x = rand('normal', 0, 1);
		y = rand('normal', 3, 1);
		output;
		x = rand('normal', 3, 1);
		y = rand('normal', 6, 1);
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
		output;
		x = rand('normal', 4, 1);
		y = rand('normal', 6, 1);
		z = rand('normal', 0, 1);
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
		output;
		x = rand('normal', 4, 1);
		y = rand('normal', 6, 1);
		z = rand('normal', 0, 1);
		output;
		x = rand('normal', 2, 1);
		y = rand('normal', 8, 1);
		z = rand('normal', -3, 1);
		output;
	end;
	drop i;
run;

data iris;
	set sashelp.iris;
	drop Species;
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


/* 2d normals, 1 cluster */
%clusterability(data = normals1, test = dip, reduction = PCA, folder = clust);
%clusterability(data = normals1, test = dip, reduction = distance, distance_standardize = NONE, folder = clust);
%clusterability(data = normals1, test = silverman, reduction = PCA, folder = clust, s_adjust = TRUE, s_setSeed = 123);
%clusterability(data = normals1, test = silverman, reduction = distance, distance_standardize = NONE, folder = clust, s_adjust = TRUE, s_setSeed = 123);


/* 2d normals, 2 clusters spread enough apart */
%clusterability(data = normals2, test = dip, reduction = PCA, folder = clust);
%clusterability(data = normals2, test = dip, reduction = distance, distance_standardize = NONE, folder = clust);
%clusterability(data = normals2, test = silverman, reduction = PCA, folder = clust, s_adjust = TRUE, s_setSeed = 123);
%clusterability(data = normals2, test = silverman, reduction = distance, distance_standardize = NONE, folder = clust, s_adjust = TRUE, s_setSeed = 123);


/* 2d normals, 3 clusters close */
%clusterability(data = normals3, test = dip, reduction = PCA, folder = clust);
%clusterability(data = normals3, test = dip, reduction = distance, distance_standardize = NONE, folder = clust);
%clusterability(data = normals3, test = silverman, reduction = PCA, folder = clust, s_adjust = TRUE, s_setSeed = 123);
%clusterability(data = normals3, test = silverman, reduction = distance, distance_standardize = NONE, folder = clust, s_adjust = TRUE, s_setSeed = 123);


/* 3d normals, 2 clusters pretty far apart */
%clusterability(data = normals4, test = dip, reduction = PCA, folder = clust);
%clusterability(data = normals4, test = dip, reduction = distance, distance_standardize = NONE, folder = clust);
%clusterability(data = normals4, test = silverman, reduction = PCA, folder = clust, s_adjust = TRUE, s_setSeed = 123);
%clusterability(data = normals4, test = silverman, reduction = distance, distance_standardize = NONE, folder = clust, s_adjust = TRUE, s_setSeed = 123);


/* 3d normals, 3 clusters very close  */
%clusterability(data = normals5, test = dip, reduction = PCA, folder = clust);
%clusterability(data = normals5, test = dip, reduction = distance, distance_standardize = NONE, folder = clust);
%clusterability(data = normals5, test = silverman, reduction = PCA, folder = clust, s_adjust = TRUE, s_setSeed = 123);
%clusterability(data = normals5, test = silverman, reduction = distance, distance_standardize = NONE, folder = clust, s_adjust = TRUE, s_setSeed = 123);


/* Iris example */
%clusterability(data = iris, test = dip, reduction = PCA, folder = clust);
%clusterability(data = iris, test = dip, reduction = distance, distance_standardize = NONE, folder = clust);
%clusterability(data = iris, test = silverman, reduction = PCA, folder = clust, s_adjust = TRUE, s_setSeed = 123);
%clusterability(data = iris, test = silverman, reduction = distance, distance_standardize = NONE, folder = clust, s_adjust = TRUE, s_setSeed = 123);


/* cars */
%clusterability(data = cars, test = dip, reduction = PCA, folder = clust);
%clusterability(data = cars, test = dip, reduction = distance, distance_standardize = NONE, folder = clust);
%clusterability(data = cars, test = silverman, reduction = PCA, folder = clust, s_adjust = TRUE, s_setSeed = 123);
%clusterability(data = cars, test = silverman, reduction = distance, distance_standardize = NONE, folder = clust, s_adjust = TRUE, s_setSeed = 123);
