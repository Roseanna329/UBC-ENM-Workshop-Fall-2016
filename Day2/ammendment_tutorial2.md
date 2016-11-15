Ammendments to Tutorial 2 (additional data cleaning)
================

We observed that our class efforts to clean the bullfrog data still left us with some undesirable locality records.

Let's clean these up now.

``` r
data<-read.csv("./Data/Occurrences/bullfrogLocs_clean.csv",stringsAsFactors=FALSE,header=TRUE)

### Let's remove the rows with NA values in either the long or lat columns

# the "complete.cases" command is useful for this type of filtering

data_noNA<-data[complete.cases(data[,c("decimallongitude", "decimallatitude")]),]

### We also had a few sites still kicking around that are from outside of our species' native range. Let's remove these.

# List the offending states

introducedRange<-c("California","Arizona")

# now exclude all rows with these states

data_noNA_east<-data_noNA[!(data_noNA$stateprovince %in% introducedRange),]

# rename the row names in our new subsetted dataframe so that we don't confuse R (or ourselves) in future steps

row.names(data_noNA_east)<-1:length(data_noNA_east[,1])

### We also noticed that some of our points have positive longitudes, which places them in Europe/Asia somewhere. However, a quick look at those rows suggests they really are from North America, given the stateprovince column. 

# Convert positive longitudes to negative

# First determine which rows this pertains to

pos<-data_noNA_east[which(data_noNA_east$decimallongitude>0),]

row.names(pos)
```

    ## [1] "319"

``` r
# Check these rows indeed have positive longitudes

data_noNA_east[201,"decimallongitude"]
```

    ## [1] -72.9851

``` r
data_noNA_east[377,"decimallongitude"]
```

    ## [1] -73.1345

``` r
data_noNA_east[383,"decimallongitude"]
```

    ## [1] -72.51183

``` r
# Now convert the positive values into negative ones

data_noNA_east[201,"decimallongitude"]<-data_noNA_east[201,"decimallongitude"]*-1
data_noNA_east[377,"decimallongitude"]<-data_noNA_east[377,"decimallongitude"]*-1
data_noNA_east[383,"decimallongitude"]<-data_noNA_east[383,"decimallongitude"]*-1

### Finally reassign this cleaned up dataframe to the "data" object

data<-data_noNA_east
```
