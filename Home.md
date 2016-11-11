
UBC Ecological Niche Modeling Workshop
======================================

In this workshop we cover the basics of building an ecological niche model and you will specifically learn how to use R to generate MAXENT niche models.

### *Course Outline*

**Day 1**: Preparing data for niche modeling
**Day 2**: Model Calibration
**Day 3**: Model Evaluation and Prediction

### *Important Information*

#### Prior to the start of the workshop, please do the following:

1.  Free up as much space on your hard-drive as possible (preferrably 15-20 GB but at least 10 GB)
2.  Download the "Data" folder (and extract the zipped files) from: <https://github.com/jullee/UBC-ENM-Workshop-Fall-2016/tree/master/Data>
3.  Download and install R (or make sure you have a fairly recent version)
4.  Download MAXENT from here: <https://www.cs.princeton.edu/~schapire/maxent/>
5.  Install the following R packages and any dependancies:

-   raster
-   maptools
-   dismo
-   spThin
-   rgeos
-   adehabitatHR
-   spatial.tools

In R, you can install packages using the following command:

``` r
install.packages("<PACKAGE_NAME>") # replace <PACKAGE_NAME> with the name of the package you wish to install
```

You may need to update Java. You also need to follow the instructions for the dismo package carefully: put a copy of the maxent.jar file into the dismo "java" folder (you need to locate your R libraries on your computer).

If you can't figure this out before the first class, please plan to stay a bit longer at the end of the class so we can get you set up.

### *Questions?*

Contact me: lee-yaw [at] zoology [dot] ubc [dot] ca
