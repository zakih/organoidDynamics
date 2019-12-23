setBatchMode(true)

//enter the path to the folder containing the images and the file for results output
input = "TypeInputDirectoryPathHere";
output_R = "TypeOutputDirectoryPathHere"; //CSV output directory
output_T = "TypeDirectoryPathHere"; //Drawn boundary image output directory
list = getFileList(input);

run("Line Width...", "line=5"); 
setForegroundColor(0, 255, 0);

roiManager("Associate", "true");
roiManager("Centered", "false"); 
roiManager("UseNames", "false");

for (i = 0; i < list.length; i++){
	filename = list[i];
	open(input + filename); 
	ACM();
	ROI_CSV(output_R , filename); 
	ROI_draw(output_T , filename);
	close("*"); 
}

setBatchMode(false)

function ROI_CSV(output , filename){
	//This function creates a csv file with the X/Y Coordinates of all the ROIs detected 
	//It also saves the "Results" window from each image
	n = roiManager("count");
	
	for (j = 0; j < n; j++) { 
		roiManager("Select", j);
		File.makeDirectory(output + filename + "/");
		saveAs("XY Coordinates", output + filename + "/" + j + ".csv");
	}
	
	saveAs("Results", output + filename + ".csv"); 
	roiManager("Deselect");
}

function ACM() {
//This function creates applies filters to image and generates boundary ROI
	run("Duplicate...", "title=ACM duplicate range 1-1000"); 
	run("Subtract Background...", "rolling=50 disable light stack");
	run("Enhance Contrast...", "saturated=0.2 normalize process_all"); 
	setOption("BlackBackground", true);
	run("Make Binary", "method=Default background=Default calculate black"); 
	run("Gaussian Blur...", "sigma=1 stack");
	run("Make Binary", "method=Default background=Default calculate black"); 
	run("Close-", "stack");
	run("Fill Holes", "stack");
	run("Remove Outliers...", "radius=5 threshold=0 which=Dark stack");
	run("Maximum...", "radius=15 stack"); 
	run("Close-", "stack");
	run("Fill Holes", "stack"); 
	run("Minimum...", "radius=15 stack");
	run("Analyze Particles...", "size=5000-Infinity pixel circularity=0-1.00 show=[Overlay Outlines] display add stack");
}

function ROI_draw(output , filename){
	//This function creates an image overlying the boundary ROI
	n = roiManager("count"); 
	selectWindow(filename);
	
	run("RGB Color");
	roiManager("Deselect"); 
	roiManager("Draw");
	roiManager("Delete")
	save(output + filename + ".Traced.tif");
}