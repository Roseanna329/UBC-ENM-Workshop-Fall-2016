Day 2: Tutorial
================

Today's objective is do a little bit more thinking about the data we use in specific niche modeling projects and to introduce ourselves to niche modeling in R.

------------------------------------------------------------------------

### *Sampling Bias*

#### **Exercise D2.1**

1.  Examine the distribution of our filtered frog localities in geographic and environmental space.
2.  Thin these locality records based on geography and then based on environment. Compare the full dataset to the thinned dataset in both cases in environmental space.
3.  Decide on the method of thinning (if any) that you want to use for our case study. Be sure to write down your reasoning.

Let's first examine our records in geographic space.

``` r
### Let's first examine our records in geographic space

### Load all relevant files
setwd("/<PATH_TO_FILES>") # insert the path to the "Data" directory
```

``` r
library(maptools)

data<-read.csv("./Occurrences/bullfrogLocs_clean.csv",stringsAsFactors=FALSE,header=TRUE) # By now we have a final set of occurrence records filtered to the native range of the species. 

range<-readShapePoly("./Range_Maps/bullfrog_NativeRange.shp",proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) # The model will be based on the native range.

# Plot the localities overtop the native range
# Your map will look different from the example below, which is based on only a quick and dirty subset of the records

plot(range)
points(data[,c("decimallongitude","decimallatitude")])
```

![](Day2_Tutorial_files/figure-markdown_github/unnamed-chunk-2-1.png)

##### QUESTIONS:

1.  Are there regions of the native range that are underrepresented?
2.  Are there regions of the native range that are overrepresented?

``` r
### Let's now examine our records in geographic space

library(raster)

# Load the environmental layers

files<-list.files(path="./Environmental_Layers",pattern="_final.tif",full.names=TRUE)
layers<-stack(files)

# The raster layers are in a different projection than both our occurrence records and range maps. Let's reproject the latter two inputs.

# define the projection
laea<-proj4string(layers)

# First the occurrence records
# Subset them to be a bit more amenible for maxent

locs<-data[,c("scientificname","decimallongitude","decimallatitude")]
names(locs)<-c("taxa","x","y") # rename the columns to be a bit more conventional
coordinates(locs)<-~x+y # convert to SpatialPointsDataframe 
proj4string(locs)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" # tell R what the original projection of the occurrence records is
locs_laea<-spTransform(locs,CRS=CRS(laea)) # project the data

# Now the range shapefile

range_laea<-spTransform(range,CRS=CRS(laea))

# check that everything looks right
plot(layers[[1]])
plot(range_laea,add=TRUE)
points(locs_laea) # note here that because locs_laea is a spatial object, we don't need to specify the coordinates in the points command
```

![](Day2_Tutorial_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
# Now let's extract environmental information about our localities so that we can examine them in environmental space 

values<-extract(layers,locs_laea)

# Make a new dataframe with the projected coordinates and these values. Include the original unprojected coordinates (we need them below). Examine the resulting dataframe. 

data_values<-cbind(as.data.frame(locs_laea),data[,c("decimallongitude","decimallatitude")],values)
```

##### QUESTION:

1.  Why do we have NAs in the resulting dataframe of environmental values?

``` r
# Let's use PCA to examine the distribution of points in environmental space
# You could use other ordination techniques...

# We are going to have to first get rid of NAs (standard PCA does not work with missing data)

data_complete<-data_values[complete.cases(data_values),]
pca<-prcomp(data_complete[,c(6:10)],scale=TRUE,center=TRUE) # 6:10 specifies the columns with the environmental data

# Add the PCA scores of each site for the first three PC axes to our dataframe

data_complete<-cbind(data_complete,pca$x[,1:3])

# Let's plot the points in PC space
# Again, your plot should look different from my example here

plot(data_complete$PC1,data_complete$PC2)
```

![](Day2_Tutorial_files/figure-markdown_github/unnamed-chunk-4-1.png)

##### QUESTIONS:

1.  Are there regions of environmental space that are underrepresented? Is this a problem?
2.  Do we have any obvious clumping of records in environmental space?

#### Thinning Records

There are multiple suggestions in the literature for thinning records/ dealing with spatial autocorrelation in a locality dataset. There are also multiple types of software and R packages for this. We will use a couple of these today (not necessarily my favourites) but in your own work, you probably want to read up on the strengths and weaknesses of different approaches.

``` r
### Let's thin records based on geographic distance.
# We will use the spThin package but see also dismo's gridSample function 

library(spThin)

# We first have to make sure the species is called the same thing in all rows
data_complete$taxa<-"Lithobates catesbeianus"

# Use the thin command with the original lat/long coordinates to created a thinned dataset.
```

``` r
geo_thinned<-as.data.frame(thin(data_complete,lat.col="decimallatitude",long.col="decimallongitude",spec.col="taxa",thin.par=100,reps=1,locs.thinned.list.return=TRUE,write.files=FALSE))

# Take a look at the thinning results

plot(range) # unprojected
points(data_complete[,c("decimallongitude","decimallatitude")])
points(geo_thinned[,c("Longitude","Latitude")],col="red")
```

##### QUESTIONS:

1.  What does the thin.par=100 parameter mean? What happens if you increase this value? Decrease it? Do you think we should be using a different value?
2.  Go through the steps above to get the distribution of our thinned records in PC space (name things differently so you can compare the old and new plots). Did the thinning result improve the distribution of points in environmental space?

``` r
### Let's thin records based on environmental space
# For this we can "hack" the gridSample funciton in the dismo package functions

library(dismo)

# First, because we've been subsetting our cleaned dataframe and now have row numbers that aren't consecutive, let's rename our rows. 
# We want to do this because we will later want to use our row indices

row.names(data_complete)<-1:length(data_complete[,1])

# To thin in environmental space, let's make dummy raster, the "extent" of which are the range of our PC1 and PC2 values

dum<-raster(extent(range(data_complete[,"PC1"]),range(data_complete[,"PC2"]))+0.5) # in addition to reading in rasters, the raster function can be used to define them manually
res(dum)<-0.5# set the resolution of the dummy raster
sample<-gridSample(data_complete[,c("PC1","PC2")],dum,n=1)
```

``` r
# Now you can look at the original and thinned points in PC space

plot(data_complete[,c("PC1","PC2")])
points(sample,col="red")
```

``` r
# The gridSample function only returns the row names and "xy" values (in this case PC1 and PC2) of the thinned points. We want the other columns so let's take advantage of the row numbers to get the full thinned dataframe.

env_thinned<-data_complete[which(row.names(data_complete) %in% row.names(sample)),]
```

------------------------------------------------------------------------

##### QUESTIONS:

1.  Which method of thinning (if either) did a better job of reducing clustering in environmental space?
2.  What are some of the challenges of thinning based on geographic distances?
3.  What are some of the challenges of thinning based on environmental space? What do you make of the use of the PCA analysis to accomplish this task?
4.  Are there any drawbacks to thinning in general?
5.  Which method of thinning (if either) do you think is appropriate for our case study? Save the corresponding dataframe. Note: This is where we may start to all diverge in the decisions we make for this case study!

``` r
### Save your final (thinned) set of occurrence records (even if you decided not to thin)

write.csv(<DATAFRAME>, "./Occurrences/bullfrogLocs_clean_thinned.csv",row.names=FALSE) # insert the dataframe from above that you think is best 
```

##### ADVANCED CHALLENGE:

The environmental filtering we did above only dealt with the first two axes of variation in our PCA analysis. Can you think of a way to extend this type of approach to handle multivariate data (not necessarily PCA-converted).
HINT: Think about the types of matrices used by ecologists to describe the similarity/dissimilarity of sites.

### *Environmental Correlations*

#### **Exercise D2.2**

------------------------------------------------------------------------

### *Basic MAXENT Modeling*

#### **Exercise D2.3**
