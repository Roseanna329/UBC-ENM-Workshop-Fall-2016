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

pos
```

    ##     Record_number  type   modified license rightsholder
    ## 319           334 Sound 11/04/2015     CC0             
    ##                                accessrights
    ## 319 http://vertnet.org/resources/norms.html
    ##                                                                                                                                                                                                      bibliographiccitation
    ## 319 Macaulay Library. Macaulay Library Audio and Video Collection. Record ID: d8e247cb-ff8c-4405-bd9e-5aef2a85947d. Source: http://ipt.vertnet.org:8080/ipt/resource.do?r=cuml_sound_film (source published on 2015-11-04)
    ##                                  references                 institutionid
    ## 319 http://macaulaylibrary.org/audio/166449 urn:lsid:biocol.org:col:34939
    ##     collectionid datasetid institutioncode collectioncode datasetname
    ## 319           NA        NA             CLO             ML            
    ##          basisofrecord informationwithheld datageneralizations
    ## 319 MachineObservation                  NA                  NA
    ##     dynamicproperties                         occurrenceid catalognumber
    ## 319                   d8e247cb-ff8c-4405-bd9e-5aef2a85947d        166449
    ##     recordnumber     recordedby individualcount   sex lifestage
    ## 319              William V Ward              NA males          
    ##     reproductivecondition behavior establishmentmeans occurrencestatus
    ## 319                    NA                                      present
    ##     preparations disposition                         associatedmedia
    ## 319                       NA http://macaulaylibrary.org/audio/166449
    ##     associatedreferences associatedsequences associatedtaxa
    ## 319                                       NA             NA
    ##     othercatalognumbers
    ## 319                    
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              occurrenceremarks
    ## 319 Typical bullfrog: single syllable, Rum, Rum, Rum.  two-syllable: More Rum, More Rum.  Also a coughing, clicking, or throat-clearing sound.  Dr. Hubert Frings states that these recordings are somewhat different from those he has heard of frogs in the Eastern United States.  Several males, not observed.  Recorded from a lily pond situated on the plant rental farm of Nursery Associates.  There were no recording problems.  Parabola was set up at pond edge, and the recorder turned on.  Frogs at different distances provided a feeling of depth.  These bullfrogs were introduced, about 1900, by frog farmers who were not successful in raising the frogs for market.  Few are left; but some small colonies seem to maintain themselves.
    ##     organismid organismname organismscope associatedoccurrences
    ## 319         NA           NA            NA                    NA
    ##     associatedorganisms previousidentifications organismremarks
    ## 319                  NA                                      NA
    ##     materialsampleid eventid fieldnumber  eventdate eventtime
    ## 319               NA      NA             03/12/1961        NA
    ##     startdayofyear enddayofyear year month day verbatimeventdate habitat
    ## 319             71           71 1961     3  12         12-Mar-61      NA
    ##     samplingprotocol samplingeffort fieldnotes eventremarks locationid
    ## 319               NA             NA         NA           NA         NA
    ##     highergeographyid                        highergeography continent
    ## 319                NA | United States | Hawaii |  |  |  |  |   Oceania
    ##               waterbody      islandgroup island       country countrycode
    ## 319 North Pacific Ocean Hawaiian Islands     NA United States          US
    ##     stateprovince county municipality                   locality
    ## 319        Hawaii                     46-316 Heiku Road, Kaneohe
    ##                                        verbatimlocality
    ## 319 United States | Hawaii | 46-316 Heiku Road, Kaneohe
    ##     minimumelevationinmeters maximumelevationinmeters verbatimelevation
    ## 319                       NA                       NA                  
    ##     minimumdepthinmeters maximumdepthinmeters verbatimdepth
    ## 319                   NA                   NA            NA
    ##     minimumdistanceabovesurfaceinmeters
    ## 319                                  NA
    ##     maximumdistanceabovesurfaceinmeters locationaccordingto
    ## 319                                  NA                    
    ##     locationremarks decimallatitude decimallongitude
    ## 319              NA        21.41667         157.8167
    ##                   geodeticdatum coordinateuncertaintyinmeters
    ## 319 not recorded (forced WGS84)                            NA
    ##     coordinateprecision verbatimcoordinates verbatimlatitude
    ## 319                  NA                           21.4166667
    ##     verbatimlongitude verbatimcoordinatesystem verbatimsrs footprintwkt
    ## 319       157.8166667                       NA          NA           NA
    ##     footprintsrs georeferencedby georeferenceddate georeferenceprotocol
    ## 319           NA                                                       
    ##     georeferencesources georeferenceverificationstatus georeferenceremarks
    ## 319                              requires verification                    
    ##     geologicalcontextid earliesteonorlowesteonothem
    ## 319                  NA                          NA
    ##     latesteonorhighesteonothem earliesteraorlowesterathem
    ## 319                         NA                         NA
    ##     latesteraorhighesterathem earliestperiodorlowestsystem
    ## 319                        NA                           NA
    ##     latestperiodorhighestsystem earliestepochorlowestseries
    ## 319                          NA                          NA
    ##     latestepochorhighestseries earliestageorloweststage
    ## 319                         NA                       NA
    ##     latestageorhigheststage lowestbiostratigraphiczone
    ## 319                      NA                         NA
    ##     highestbiostratigraphiczone lithostratigraphicterms group formation
    ## 319                          NA                      NA    NA        NA
    ##     member bed identificationid identificationqualifier typestatus
    ## 319     NA  NA               NA                      NA         NA
    ##     identifiedby dateidentified identificationreferences
    ## 319           NA             NA                       NA
    ##     identificationverificationstatus identificationremarks
    ## 319                               NA                    NA
    ##     scientificnameid namepublishedinid          scientificname
    ## 319               NA                NA Lithobates catesbeianus
    ##     acceptednameusage originalnameusage namepublishedin
    ## 319                NA                NA              NA
    ##     namepublishedinyear higherclassification  kingdom   phylum    class
    ## 319                  NA                      Animalia Chordata Amphibia
    ##     order  family      genus subgenus specificepithet infraspecificepithet
    ## 319 Anura Ranidae Lithobates       NA    catesbeianus                   NA
    ##     taxonrank verbatimtaxonrank scientificnameauthorship    vernacularname
    ## 319   species                NA                          American Bullfrog
    ##     nomenclaturalcode taxonomicstatus taxonremarks lengthinmm lengthtype
    ## 319              ICZN              NA           NA         NA         NA
    ##     lengthunitsinferred massing massunitsinferred underivedlifestage
    ## 319                  NA      NA                NA                   
    ##     underivedsex dataset_url
    ## 319                       NA
    ##                                                                                                                                                          dataset_citation
    ## 319 Macaulay Library. Macaulay Library Audio and Video Collection. Source: http://ipt.vertnet.org:8080/ipt/resource.do?r=cuml_sound_film (source published on 2015-11-04)
    ##                            gbifdatasetid
    ## 319 7f6dd0f7-9ed4-49c0-bb71-b2a9c7fed9f1
    ##                          gbifpublisherid     dataset_contact_email
    ## 319 e2e717bf-551a-4917-bdc9-4fa0f342c530 edwin.scholes@cornell.edu
    ##              dataset_contact dataset_pubdate lastindexed migrator_version
    ## 319 Edwin Scholes III, Ph.D.      11/04/2015  08/11/2016       10/15/2015
    ##     hasmedia hastissue wascaptive isfossil      vntype haslength
    ## 319        1         0          0        0 observation         0

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
