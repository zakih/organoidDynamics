# organoidDynamics
Time series analysis of sets of 2D organoid contours 

# Instructions
	* Follow the sequence in sections 1-5 to perform the analysis
	* Organoid contour (x,y) time series are included in subdirectory 'data'
	* Section 6 below contains optional scripts for visualizing 
	* For visualizing, external code required is for plotting in MATLAB using a script called subaxis (Copyright (c) 2014, Aslak Grinsted) available on Mathworks

# Source files
1. MATLAB
	1. preprocessing.m
	2. resampleWithDensity.m
	3. geom.m
	4. pdm_organoid_preprocessing.m
	5. pdm_organoid_compute.m
	6. resampleWithConstantNumPoints.m
	7. featureExtraction_signalProcessing.m
	8. featureExtraction_geomExport.m
	9. getDetSign.m
   	10. getPmoi.m
   	11. xyTimeSeriesPlotter.m
   	12. signalProcessing_perOrganoid.m
2. R
	1. dimReduction.R
	2. dimReduction_extra.R
	3. cluster.R
	4. plot_timeSeriesPlots.R
	5. plot_perimeterVSarea.R

# 1. Data Preprocessing (using MATLAB \~ 2 min)
1. Run preprocessing.m to perform the following functions:
	1. Read sample basal/FGF2 raw data
	2. Scale (default no scaling, set 'scalingFactor' at top to change this)
	3. Smoothing (turn on/off by setting 'smooth' at top)
	4. Even density sampling
2. Preprocessed contours saved as 
	* 'zAllOrganoids.mat'
3. Functions used:
	1. resampleWithDensity.m
	2. geom.m

# 2.0 Point Distribution model (using MATLAB \~ 1 hr)
1. Run pdm_organoid_preprocessing.m to (~ 131s):
	1. 'qArray.mat' (MATLAB cell)
	2. 'qMatrix.mat' (MATLAB array)
2. Run pdm_organoid_compute.m to output eigenvalues, eigenvectors, meanshapes, and corresponding statistics saved as:
	1. 'U_XX_.mat'
	2. 'eigenVals_XX.mat'
	3. 'qbar_XX.mat'
	4. 'varExplained_1stMode.csv'
	5. 'auc_5Modes.csv'
	6. 'varExplained_3Modes.csv'
3. Fucntions used:
	1. resampleWithConstantNumPoints.m
	2. geom.m

# 3.0 Feature extraction (using MATLAB \~ 5 min) 
1. run featureExtraction_signalProcessing.m
	* Exports DFT amplitudes and frequencies
		*'DTFT_X_shift_Organoids.mat'
		*'DTFT_freq_Organoids.mat'
2. run featureExtraction_geomExport.m
	* Exports feature table to use in R
		* 'allOrganoidFeatures.csv'
	* Exports same data as MATLAB data
		* 'allOrganoidFiles.mat'
3. Functions used:
	1. geom.m
	2. getDetSign.m
	3. getPmoi.m
	
# 4. Dimensionality Reduction (using R \~ 10 seconds) 
1. Run following sections in dimReduction.R:
	1. Load extracted features
	2. PCA
	3. OPTIONAL: PCA-Viz
2. Optional: Extra visualizations in dimReduction_extra.R 
	1. PCA animation
	2. PCA 3D plot

# 5. Time Series Clustering (using R \~ 10 seconds) 
1. Run following sections in cluster.R
	1. PCA-Heirarchichal clustering
2. Optional: visualizations & stats in cluster.R by running following sections:
	1. Visualize clustering
	2. Cluster feature stats
		* Outputs cluster membership as 'clusterMembership.csv'
	3. extra plots

# 6. Extra files
1. Plot time series of contours
	* xyTimeSeriesPlotter.m
	* Can create three types of image sets:
		1. Thumbnails contour series (output in ~/image_thumbnail/)
		2. Full contour series (output in ~/image_fullImage/)
		3. First and last contour overlaid (output in ~/image_firstLast/)
2. Plot DFT results & contour reconstrucitons
	* signalProcessing_perOrganoid.m
3. Plot time series of extracted features
	* plot_timeSeriesPlots.R
4. Plot perimeter vs area and curve fit coefficients
	* plot_perimeterVSarea.R
