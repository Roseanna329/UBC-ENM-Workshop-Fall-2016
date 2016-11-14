Day 3: Tutorial
================

Today's objective is learn how to evaluate niche models and how tune them to work towards a final model.

NOTE: Please save a version of the Rscript you use in this tutorial. Having a record of the specific commands you use and the ways they differ from the ones below (preferrably with comments explaining your decisions) will greatly facilitate any troubleshooting we have to do.

------------------------------------------------------------------------

### *Basic Model Evaluation*

#### **Exercise D3.1**

Using our preliminary model from yesterday, let's explore common model evaluation metrics (one threshold-independant and two threshold-dependant statistics).

1.  Get the AUC score for your model based using a *resubstitution* approach.
2.  Use two common thresholds to convert the continuous model output to binary predictions and calculate TSS and Accuracy.

``` r
setwd("/<PATH_TO_PARENT>") # insert the path to the directory with both the Data and MaxentModels subdirectories
```

``` r
library(raster)
library(dismo)
library(maptools)

### First load your saved model object.

load("./MaxentModels/model_object") # it will be loaded as an object called "me", the name we assigned it yesterday

# The environmental values at the presences and background points in our models were saved as output when we ran MAXENT

pres<-read.csv("./MaxentModels/presence",header=TRUE)
back<-read.csv("./MaxentModels/absence",header=TRUE) # Note even with resubstitution approaches you might want to at least replace the training backgorund data with new random background points. To do this you would use the sampleRandom command in the raster package to sample background points, then extract the environmental values for these points. You can try this if you'd like to see if the results below change.

head(pres)
```

    ##   species x y precip_final tmax_final tmean_final  tmin_final
    ## 1 species 1 1    1448.5699   332.3895   168.40812   -2.151543
    ## 2 species 2 2     917.5834   242.8943    35.89430 -181.798783
    ## 3 species 3 3    1168.1872   277.5494    77.62410 -119.375899
    ## 4 species 4 4    1061.2306   238.2625    35.26245 -178.235717
    ## 5 species 5 5    1059.6251   256.5378    50.07560 -164.021416
    ## 6 species 6 6    1237.0442   272.9652    94.70589  -78.616993
    ##   wetness_final
    ## 1           872
    ## 2           793
    ## 3           680
    ## 4           421
    ## 5           451
    ## 6           328

``` r
head(back)
```

    ##      species x y precip_final tmax_final tmean_final tmin_final
    ## 1 background 1 1    1486.5051   310.3045   143.49233  -29.40451
    ## 2 background 2 2     950.8509   303.1491   108.00000  -93.00000
    ## 3 background 3 3    1272.0430   326.2972   192.00000   40.32213
    ## 4 background 4 4    1068.0681   273.6556    72.02738 -127.97616
    ## 5 background 5 5     982.7889   355.1009   197.00000   37.00000
    ## 6 background 6 6    1177.6245   284.7510    96.06763  -84.24373
    ##   wetness_final
    ## 1           209
    ## 2           749
    ## 3           691
    ## 4           282
    ## 5           619
    ## 6           275

``` r
### Use dismo's evaluate function with the information from the presences and background to evaluate the niche model using a resubstitution approach

eval<-evaluate(me, p=pres[,4:length(pres)],a=back[,4:length(back)])
eval
```

    ## class          : ModelEvaluation 
    ## n presences    : 83 
    ## n absences     : 10000 
    ## AUC            : 0.8587157 
    ## cor            : 0.1377267 
    ## max TPR+TNR at : 0.4755533

``` r
# Get the AUC score specifically

auc<-slot(eval,"auc")
```

##### QUESTIONS:

1.  How does your AUC score suggest your model did?
2.  What are the problems with relying on this score?

``` r
# Get a list of cutoff values for common thresholds 

t<-threshold(eval)
t
```

    ##                kappa spec_sens no_omission prevalence equal_sens_spec
    ## thresholds 0.7762213 0.4755533   0.1881016 0.03067031       0.4879809
    ##            sensitivity
    ## thresholds   0.3610059

``` r
# We can also take advantage of the MAXENT output files listing the predictions for training data to calculate two common thresholds. 

pres_pred<-read.csv("./MaxentModels/species_samplePredictions.csv",header=TRUE)
back_pred<-read.csv("./MaxentModels/species_backgroundPredictions.csv",header=TRUE) # we instructed MAXENT to save the predictions for the background points. Load this file as well, although we won't use it until further down in the exercise.

# Minimum suitability score of the occurrence records

minSuitPres<-min(pres_pred[,"Logistic.prediction"]) # should be roughly the same as the "no_omission" threshold produced above

# Cutoff for 95% of the occurrence records
# This attempts to account for occurrences that may be in sink habitat.

quantCutoff<-quantile(pres_pred[,"Logistic.prediction"],0.05)
```

Let's use our two calculated cutoff to convert MAXENT's continuous predictions to binary predictions.

``` r
### Convert continous point predictions to binary predictions of presence-absence

# We will focus on the logistic output for now.
# Using a cutoff based on the minimum suitability score of the occurrence records:

pres_pred$minSuitBinary<-NA # make a new column to store our first set of binary predictions and for now give all rows a value of "NA"
pres_pred$minSuitBinary[which(pres_pred$Logistic.prediction>=minSuitPres)]<-1 # rows with logistic scores higher than or equal to our cutoff get a 1
pres_pred$minSuitBinary[which(pres_pred$Logistic.prediction<minSuitPres)]<-0
# rows with logistic scores lower than our cutoff get a 0

# Do the same for the background predictions. (Note that the column names are different in the file saved by MAXENT for some reason.)
back_pred$minSuitBinary<-NA
back_pred$minSuitBinary[which(back_pred$logistic>=minSuitPres)]<-1
back_pred$minSuitBinary[which(back_pred$logistic<minSuitPres)]<-0

# Repeat for the other threshold we calculated.

pres_pred$quantBinary<-NA
pres_pred$quantBinary[which(pres_pred$Logistic.prediction>=quantCutoff)]<-1  
pres_pred$quantBinary[which(pres_pred$Logistic.prediction<quantCutoff)]<-0

back_pred$quantBinary<-NA
back_pred$quantBinary[which(back_pred$logistic>=quantCutoff)]<-1
back_pred$quantBinary[which(back_pred$logistic<quantCutoff)]<-0

### Now we can make a confusion matrix

# First give each dataframe a column scoring the sites correctly as presence of absence (note not true absences!).

pres_pred$real<-1
back_pred$real<-0

# Now make a new dataframe with the relevant information

dat<-rbind(pres_pred[,c("real","minSuitBinary","quantBinary")],back_pred[,c("real","minSuitBinary","quantBinary")])

# Now we can make a confusion matrix based on each of the thresholds

# Minimum suitabilty threshold

confuse_min<-table(dat[,c("real","minSuitBinary")])
confuse_min
```

    ##     minSuitBinary
    ## real    0    1
    ##    0 2721 7279
    ##    1    0   83

``` r
confuse_quant<-table(dat[,c("real","quantBinary")])
confuse_quant
```

    ##     quantBinary
    ## real    0    1
    ##    0 4765 5235
    ##    1    5   78

##### QUESTIONS:

1.  Are there any values in these tables that are NOT surprising? (Hopefully you answer yes). We can begin to see why a resubstitution approach to model evaluation is not ideal.
2.  What other challenges do we face with these evaluation measures?

Let's go on to calculate the true skill statistic (TSS) and model accuracy...just so we know how for future purposes. In the interest of time, let's just do this for the second threshold that we used above.

We will use the "PresenceAbsence" library for this purpose.

``` r
### Threshold-dependent test statistic using the quantile threshold

library(PresenceAbsence)

# We need a dataframe of observed presences and "absences" and the continuous model preditions
# Here is a way to make this table (differing from the type of approach we used above)

p1<-as.data.frame(slot(eval,"presence"))
names(p1)<-"Predicted"
a1<-as.data.frame(slot(eval,"absence"))
names(a1)<-"Predicted"
p1$Observed<-1
a1$Observed<-0
gg<-data.frame(c(1:sum(length(p1[,1]),length(a1[,1]))))
names(gg)<-"plotID"
newdf<-cbind(gg,rbind(p1[,c(2,1)],a1[,c(2,1)]))

head(newdf)
```

    ##   plotID Observed Predicted
    ## 1      1        1 0.2863013
    ## 2      2        1 0.4979348
    ## 3      3        1 0.7971819
    ## 4      4        1 0.5258762
    ## 5      5        1 0.4260297
    ## 6      6        1 0.8126557

``` r
### Calculate different metrics
eval2<-presence.absence.accuracy(newdf,threshold=minSuitPres,find.auc=FALSE,st.dev=FALSE)

# model accuracy is given in the results as PCC

accuracy<-eval2$PCC
accuracy
```

    ## [1] 0.2678766

``` r
# and now we can calculate TSS

TSS<-eval2$sensitivity+eval2$specificity-1 
TSS
```

    ## [1] 0.2498518

``` r
### NOTE: Calculation of actual thresholds may differ slightly between dismo and PresenceAbsence...use dismo's in these cases

# dismo calculations 
t 
```

    ##                kappa spec_sens no_omission prevalence equal_sens_spec
    ## thresholds 0.7762213 0.4755533   0.1881016 0.03067031       0.4879809
    ##            sensitivity
    ## thresholds   0.3610059

``` r
# PresenceAbsence calculations

optimal.thresholds(newdf,opt.methods=c(2:3))
```

    ##         Method Predicted
    ## 1    Sens=Spec      0.49
    ## 2 MaxSens+Spec      0.47

------------------------------------------------------------------------

### *Tuning and Internal Validation*

The default settings of MAXENT are not necessarily appropriate for every problem. Of particular interest are the features used in model building and the controls on regularization. We can optimize these parameters by "tuning" our models. The tuning approach involves trying different numbers and combinations of features and values of the regularization parameter to decide on final model settings.

After deciding upon appropriate parameter settings and recalibrating the model, we should evaluate the final model. This time we will use a more appropriate *internal validation* approach, specifically K-fold cross-validation.

#### **Exercise D3.2**

1.  Try rebuilding the model to test the effects of different features and settings of the regularization parameter.
2.  Use 5-fold cross-validation to evaluate the final tuned model.

Let's start with tuning the features used to build the model. We will test just a few very simple feature sets. In your own work, you probably want to compare more.

``` r
### Let's get all the data we need for niche modeling and mask the layers like we did yesterday.

# Occurrence Records

locs<-read.csv("./Data/Occurrences/bullfrogLocs_clean_thinned.csv",header=TRUE)

# Raster layers

files<-list.files(path="./Data/Environmental_Layers",pattern="final.tif",full.names=TRUE) # change the start of the path if your Data directory is not one level above your models directory

layers<-stack(files)

# masking background 

background<-readShapePoly("./Data/Range_Maps/mcp_range.shp") # use the shapefile you used yesterday

# mask the environmental layers
m<-mask(layers[[1]],background) # mask the first raster
calibrationRasters<-stack(m,layers[[2:nlayers(layers)]]) # stack this new version of layer 1 with the remaining rasters
names(calibrationRasters)[1]<-names(layers)[1] # rename the first raster in new stack to it's original name 

### Now let's play around with tuning the features. In the interest of time, we will only examine a small portion of the possible feature combinations. 

# First we want to set aside a random percentage of the locality records (say 20%) to "test" the models we generate with different features

numTesting<-ceiling(length(locs[,1])*0.2) #Determine how many records to set aside if we want to keep aside 20%. The "ceiling" command" rounds up in case 20% ends up not being an integer
shuffle<-locs[sample(nrow(locs)),] # randomize the order of the locality data
test<-shuffle[1:numTesting,] # pick the top 20% of the random records to be our testing data
train<-shuffle[-(1:numTesting),] # the rest of the records will be for calibrating the models

# Now let's define three different sets of features to test

# If we want to build a model with linear features only, we change our "myArgs" vector from yesterday to include the following. Note the betamultiplier parameter sets regularization to 0 so that we can better see the impacts of the features.

myArgs_L<-c("-J","-P","removeDuplicates=TRUE","writebackgroundpredictions", "maximumiterations=5000","noquadratic","noproduct","nothreshold","nohinge","betamultiplier=0")

# Likewise if we want to build a model with only linear and quadratic features, our vector looks like this:

myArgs_LQ<-c("-J","-P","removeDuplicates=TRUE","writebackgroundpredictions", "maximumiterations=5000","noproduct","nothreshold","nohinge","betamultiplier=0")

# Finally let's try a model with linear, quadratic and product features allowed

myArgs_LQP<-c("-J","-P","removeDuplicates=TRUE","writebackgroundpredictions", "maximumiterations=5000","nothreshold","nohinge","betamultiplier=0")

# Build the three models, first defining and creating new directories for them

dir<-"./MaxentModels/model_L"
dir.create(dir) 
me_L<-maxent(calibrationRasters,train[,c("x","y")],args=myArgs_L,path=dir)

dir<-"./MaxentModels/model_LQ"
dir.create(dir) 
me_LQ<-maxent(calibrationRasters,train[,c("x","y")],args=myArgs_LQ,path=dir)

dir<-"./MaxentModels/model_LQP"
dir.create(dir) 
me_LQP<-maxent(calibrationRasters,train[,c("x","y")],args=myArgs_LQP,path=dir)

### Now evaluate each of the models using the withheld records and well as random background points.

# First choose random background points for testing.

backPts<-randomPoints(calibrationRasters, length(test[,1]))

eval_L<-evaluate(p=test[,c("x","y")], a=backPts, me_L, x=calibrationRasters) 
eval_LQ<-evaluate(p=test[,c("x","y")], a=backPts, me_LQ, x=calibrationRasters)
eval_LQP<-evaluate(p=test[,c("x","y")], a=backPts, me_LQP, x=calibrationRasters)
```

##### QUESTIONS:

1.  Examine the AUC scores for each of these models. Which one has the highest AUC score?
2.  How does the highest AUC score from above compare to our initial model and resubstitution evaluation metric? Can you explain these differences?
3.  Based on these results, which feature set will you keep for your final model?\*

Let's turn now to tuning the regularization parameter. Once again, we will try only a very small number of values. In your own work, you will probably want to extend this set to include more values.

``` r
### The process for tuning the regularization multiplier is similar to above, except this time you are keeping the features constant and changing the "betamultiplier" setting.

# Let's try values from 0.5 to 1.5

# NOTE: In the following, I chose the LQ feature set. But you should adjust the script to use the feature set that produced the model with the highest AUC from the steps above.

myArgs_LQ05<-c("-J","-P","removeDuplicates=TRUE","writebackgroundpredictions", "maximumiterations=5000","noproduct","nothreshold","nohinge","betamultiplier=0.5") 

myArgs_LQ1<-c("-J","-P","removeDuplicates=TRUE","writebackgroundpredictions", "maximumiterations=5000","noproduct","nothreshold","nohinge","betamultiplier=1") # MAXENT betamultiplier default

myArgs_LQ15<-c("-J","-P","removeDuplicates=TRUE","writebackgroundpredictions", "maximumiterations=5000","noproduct","nothreshold","nohinge","betamultiplier=1.5")

# Make new directories and build these models

dir<-"./MaxentModels/Model_LQ05"
dir.create(dir) 
me_LQ05<-maxent(calibrationRasters,train[,c("x","y")],args=myArgs_LQ05,path=dir)

dir<-"./MaxentModels/Model_LQ1"
dir.create(dir) 
me_LQ1<-maxent(calibrationRasters,train[,c("x","y")],args=myArgs_LQ1,path=dir)

dir<-"./MaxentModels/Model_LQ15"
dir.create(dir) 
me_LQ15<-maxent(calibrationRasters,train[,c("x","y")],args=myArgs_LQ15,path=dir)

# Evaluate these models

eval_LQ05<-evaluate(p=test[,c("x","y")], a=backPts, me_LQ05, x=calibrationRasters) 
eval_LQ1<-evaluate(p=test[,c("x","y")], a=backPts, me_LQ1, x=calibrationRasters)
eval_LQ15<-evaluate(p=test[,c("x","y")], a=backPts, me_LQ15, x=calibrationRasters)
```

##### QUESTIONS:

1.  Which setting of the betamultiplier resulted in the highest AUC score?
2.  In general, how is our best model performing based on AUC?
3.  Are there any other parameters you might want to tune?

-   NOTE: Ideally, you wouldn't use AUC alone during model selection. Using a method that controls for model complexity (which is expected to go up as you add more features) is better. For this reason comparing models using AIC would be better. Take a look at the "ENMeval" package for ways to calculate AIC scores for the models when you want to tune parameters in the future.

Let's focus now on evaluating models calibrated with our best feature and regularization settings.

``` r
### K-fold crossvalidation 

# Note again, the script below uses the LQ model, with a betamultiplier of 0.5. If you got better AUC scores with a different combination of features and regularization, use those settings instead.

myArgs_LQ05<-c("-J","-P","removeDuplicates=TRUE","writebackgroundpredictions", "maximumiterations=5000","noproduct","nothreshold","nohinge","betamultiplier=0.5")

# Create a "final model" directory to store the model outputs

dir<-"./MaxentModels/Final_Model"
dir.create(dir) 

# Divide up the occurrence records into 5 folds/groups

k<-5
group <- kfold(locs[,c("x","y")], k)

# Now using a loop, let's calibrate and evaluate five different models, holding out a different fold of the occurrence records as the test dataset each time. 

e<-list() #initialize a list to store the output from each model evaluation

for (i in 1:k){
   train<-locs[group!=i,] # training records
   test<-locs[group==i,] # testing records for evaluation
   backPts<-randomPoints(calibrationRasters,100) # e.g. 100 background points for evaluation 
   model<-maxent(calibrationRasters,train[,c("x","y")],args=myArgs_LQ05,path=dir) # run the model 
   e[[i]]<-evaluate(p=test[,c("x","y")],a=backPts,model,x=calibrationRasters) # evaluate the model
}

# Finally, we can get the range of AUC scores. 

aucs<-sapply(e, function(x){slot(x, 'auc')}) # Newbies to R might want to look up the R apply family of functions. These functions are very useful for many things. In this case we use it to extract the AUC score from each of the outputs in our list of outputs.
  
# NOTE: In your own work, you may want to be more thorough and add the calculations for some of the other evaluation metrics we've discussed to the loop above. 
```

##### QUESTIONS:

1.  What is the range of AUC scores?
2.  Did any models have scores that you would consider good?
3.  What accounts for the variation in scores?

NOTE: There are different approaches to using the results from the K-fold modeling. Some investigators use the full model (based on all points) to make final predictions and simply use the evaluation metrics from the K-fold modeling to say something about overall model accuracy. Another approach is to base final predictions on the average of your k-fold model predictions. I like the latter because it somewhat accounts for uncertaintly in the dataset and the model evaluation scores are actually specific to each of the prediction surfaces. However, in the interest of simplicity for the remainder of the tutorial, we will treat our full tuned model as our final model.

Save your final model before moving on.

``` r
save(me_LQ05,file="./MaxentModels/Final_Model/final_model_object")
```

##### ADVANCED CHALLENGE:

You will probably at some point want to run many models, whether for tuning purposes or because you are building models for many species. Having a way to automate the modeling process is desirable and loops (and the apply family of functions) facilitate this process. See if you can make loops for the above tuning steps. HINT: In my own work, I often first write "functions" for each step that take a set of inputs and do something (e.g. "tuneFeatures" function). I then loop through different inputs, calling these functions. You will have to figure out how to name different output objects automatically or save them with different names but it isn't too hard. Feel free to ask me after the course if you want more help with this. I am aiming to eventually put up some of my own scripts on GitHub at some point so check back from time to time.

------------------------------------------------------------------------

### *Variable Importance*

Apart from using our model to make predictions across the landscape and for different sites, we often want to know what environmental variables are the most important predictors of presence on the landscape. MAXENT has a few ways of assessing variable importance. Let's use our case study to examine these outputs.

#### **Exercise D3.3**

1.  Examine the variable importance plots generated by MAXENT for your tuned model (with all points).
2.  Examine how the probability of presence varies with each variable.

The .html files produced by MAXENT give all of this information in a nice visual summary. But we can also use R to make some similar plots.

``` r
### Let's first plot a basic graph of the contribution of different variables to our model. 

plot(me_LQ05)
```

![](Day3_Tutorial_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
# You can also get the values directly from the MAXENT output file: "maxentResults.csv"

results<-read.csv("./MaxentModels/Final_Model/maxentResults.csv")

names(results) # Ugly naming but you get the point
```

    ##  [1] "Species"                                                                           
    ##  [2] "X.Training.samples"                                                                
    ##  [3] "Regularized.training.gain"                                                         
    ##  [4] "Unregularized.training.gain"                                                       
    ##  [5] "Iterations"                                                                        
    ##  [6] "Training.AUC"                                                                      
    ##  [7] "X.Background.points"                                                               
    ##  [8] "precip_final.contribution"                                                         
    ##  [9] "tmax_final.contribution"                                                           
    ## [10] "tmean_final.contribution"                                                          
    ## [11] "tmin_final.contribution"                                                           
    ## [12] "wetness_final.contribution"                                                        
    ## [13] "precip_final.permutation.importance"                                               
    ## [14] "tmax_final.permutation.importance"                                                 
    ## [15] "tmean_final.permutation.importance"                                                
    ## [16] "tmin_final.permutation.importance"                                                 
    ## [17] "wetness_final.permutation.importance"                                              
    ## [18] "Training.gain.without.precip_final"                                                
    ## [19] "Training.gain.without.tmax_final"                                                  
    ## [20] "Training.gain.without.tmean_final"                                                 
    ## [21] "Training.gain.without.tmin_final"                                                  
    ## [22] "Training.gain.without.wetness_final"                                               
    ## [23] "Training.gain.with.only.precip_final"                                              
    ## [24] "Training.gain.with.only.tmax_final"                                                
    ## [25] "Training.gain.with.only.tmean_final"                                               
    ## [26] "Training.gain.with.only.tmin_final"                                                
    ## [27] "Training.gain.with.only.wetness_final"                                             
    ## [28] "Entropy"                                                                           
    ## [29] "Prevalence..average.of.logistic.output.over.background.sites."                     
    ## [30] "Fixed.cumulative.value.1.cumulative.threshold"                                     
    ## [31] "Fixed.cumulative.value.1.logistic.threshold"                                       
    ## [32] "Fixed.cumulative.value.1.area"                                                     
    ## [33] "Fixed.cumulative.value.1.training.omission"                                        
    ## [34] "Fixed.cumulative.value.5.cumulative.threshold"                                     
    ## [35] "Fixed.cumulative.value.5.logistic.threshold"                                       
    ## [36] "Fixed.cumulative.value.5.area"                                                     
    ## [37] "Fixed.cumulative.value.5.training.omission"                                        
    ## [38] "Fixed.cumulative.value.10.cumulative.threshold"                                    
    ## [39] "Fixed.cumulative.value.10.logistic.threshold"                                      
    ## [40] "Fixed.cumulative.value.10.area"                                                    
    ## [41] "Fixed.cumulative.value.10.training.omission"                                       
    ## [42] "Minimum.training.presence.cumulative.threshold"                                    
    ## [43] "Minimum.training.presence.logistic.threshold"                                      
    ## [44] "Minimum.training.presence.area"                                                    
    ## [45] "Minimum.training.presence.training.omission"                                       
    ## [46] "X10.percentile.training.presence.cumulative.threshold"                             
    ## [47] "X10.percentile.training.presence.logistic.threshold"                               
    ## [48] "X10.percentile.training.presence.area"                                             
    ## [49] "X10.percentile.training.presence.training.omission"                                
    ## [50] "Equal.training.sensitivity.and.specificity.cumulative.threshold"                   
    ## [51] "Equal.training.sensitivity.and.specificity.logistic.threshold"                     
    ## [52] "Equal.training.sensitivity.and.specificity.area"                                   
    ## [53] "Equal.training.sensitivity.and.specificity.training.omission"                      
    ## [54] "Maximum.training.sensitivity.plus.specificity.cumulative.threshold"                
    ## [55] "Maximum.training.sensitivity.plus.specificity.logistic.threshold"                  
    ## [56] "Maximum.training.sensitivity.plus.specificity.area"                                
    ## [57] "Maximum.training.sensitivity.plus.specificity.training.omission"                   
    ## [58] "Balance.training.omission..predicted.area.and.threshold.value.cumulative.threshold"
    ## [59] "Balance.training.omission..predicted.area.and.threshold.value.logistic.threshold"  
    ## [60] "Balance.training.omission..predicted.area.and.threshold.value.area"                
    ## [61] "Balance.training.omission..predicted.area.and.threshold.value.training.omission"   
    ## [62] "Equate.entropy.of.thresholded.and.original.distributions.cumulative.threshold"     
    ## [63] "Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold"       
    ## [64] "Equate.entropy.of.thresholded.and.original.distributions.area"                     
    ## [65] "Equate.entropy.of.thresholded.and.original.distributions.training.omission"

``` r
# Example: Get the training gain when we don't include "wetness" in the model and compare to gain when we don't include maximum temperature (or pick any two variables if you didn't include these ones in your model)

no_wet_gain<-results$Training.gain.without.wetness_final
no_tmax_gain<-results$Training.gain.without.tmax_final
```

##### QUESTIONS:

1.  Which variables contribute the most to the model? Which contribute the least?
2.  What do the training gain scores without each variable tell you? What do you learn about the importance of the two variables we looked at above?
3.  Based on these results, are there any variables that did not contribute much to our model? If so, do you think we should just exclude them? Why or why not?

``` r
### We can also delve into the specific relationships of each variable with the predicted probability of presence.

# Plot the response curves

response(me_LQ05)
```

![](Day3_Tutorial_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
# Plot the densities of presences (red) versus background (blue) for different environmental values. (e.g. Akin to the types of probability density functions MAXENT is using to optimize the model...but not exactly because the algorithm is tranforming the variables)

density(me_LQ05)
```

![](Day3_Tutorial_files/figure-markdown_github/unnamed-chunk-11-2.png)

##### QUESTIONS:

1.  How do these plots further inform variable importance?
2.  In the response plots, do we see evidence of "clamping"?
3.  What else do the response plots allow you to say about the relationship between probability of presence and each variable? What do they NOT allow you to say?
4.  What do we learn from the density plots?
5.  My hasty example model produced the density plots pictured in this document. Hopefully yours look different (but maybe not!). What might I be concerned about based on these plots?

##### ADVANCED CHALLENGE:

How might you go about summarizing variable importance in the case of taking a more "ensemble"-like approach to niche modeling, for instance if you wanted to make use of all the models from the k-fold cross-validation step?

------------------------------------------------------------------------

### *Dealing with Extrapolation*

You probably saw some indication of where extrapolation may be a problem when examining the response curves above. Now let's think about one way that we can minimize extrapolation when making predictions.

``` r
### Let's use dismo's "mess" function to generate Multivariate Environmental Similarity Surfaces as described by Elith et al. 2010.

# We need a table of the environmental values actually used during model calibration: for both occurrences and background points. Recall this information was available from two of the output files that we previously loaded.

head(pres)
```

    ##   species x y precip_final tmax_final tmean_final  tmin_final
    ## 1 species 1 1    1448.5699   332.3895   168.40812   -2.151543
    ## 2 species 2 2     917.5834   242.8943    35.89430 -181.798783
    ## 3 species 3 3    1168.1872   277.5494    77.62410 -119.375899
    ## 4 species 4 4    1061.2306   238.2625    35.26245 -178.235717
    ## 5 species 5 5    1059.6251   256.5378    50.07560 -164.021416
    ## 6 species 6 6    1237.0442   272.9652    94.70589  -78.616993
    ##   wetness_final
    ## 1           872
    ## 2           793
    ## 3           680
    ## 4           421
    ## 5           451
    ## 6           328

``` r
head(back)
```

    ##      species x y precip_final tmax_final tmean_final tmin_final
    ## 1 background 1 1    1486.5051   310.3045   143.49233  -29.40451
    ## 2 background 2 2     950.8509   303.1491   108.00000  -93.00000
    ## 3 background 3 3    1272.0430   326.2972   192.00000   40.32213
    ## 4 background 4 4    1068.0681   273.6556    72.02738 -127.97616
    ## 5 background 5 5     982.7889   355.1009   197.00000   37.00000
    ## 6 background 6 6    1177.6245   284.7510    96.06763  -84.24373
    ##   wetness_final
    ## 1           209
    ## 2           749
    ## 3           691
    ## 4           282
    ## 5           619
    ## 6           275

``` r
# Make a dataframe of these values. 

vals<-rbind(pres[,-c(1:3)],back[,-c(1:3)])

# Now use the full raster layers with the "mess" command; Recall our calibrationRasters were masked so we don't want to use those here.

mess<-mess(layers,vals)

# plot the mess map (a bit of fancy plotting here)

h<-hist(mess,breaks=10)
```

![](Day3_Tutorial_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
brk<-h$breaks
plot(mess,breaks=brk,col=rev(terrain.colors(length(brk))))
```

![](Day3_Tutorial_files/figure-markdown_github/unnamed-chunk-12-2.png)

``` r
# Let's convert this into a binary map 

mess_binary<-mess
mess_binary[mess_binary<=0]<-NA
mess_binary[mess_binary>0]<-1

plot(mess_binary)

### Let's plot the species' native range and our occurrence records on top of the MESS map.

range<-readShapePoly("./Data/Range_Maps/bullfrog_NativeRange_laea.shp")

plot(mess_binary)
plot(range,add=TRUE)
points(locs[,c("x","y")])
```

![](Day3_Tutorial_files/figure-markdown_github/unnamed-chunk-12-3.png)

##### QUESTIONS:

1.  Are there areas outside of the species' range where we may not want to use our models to make predictions?
2.  Are there areas inside of the species' range where extrapolation would be necessary? Why might this be the case? Why aren't there even more?

NOTE: The MESS approach does not deal with differences in the structure of variable correlations, which we know are important for the MAXENT model. However, it gives us a first-pass assessment of where we are sure extrapolation will be necessary.

------------------------------------------------------------------------

### *Predictions and Mapping*

Finally we have a model that we have more-or-less assessed in a robust fashion. Let's now turn to making maps of model predictions.

#### **Exercise D3.4**

1.  Generate a prediction surface based on your final model.
2.  Use the MESS map to mask out areas where we may want to avoid making predictions.

``` r
### Get other data of interest, in this case the map of the species' full range in North America 

# if unprojected
full_range<-readShapePoly("./Data/Range_Maps/bullfrog_NorthAmericanRange.shp",proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

# reproject if you haven't yet
laea<-proj4string(layers)
full_range_laea<-spTransform(full_range,laea)

### Use our final model to make predictions across the study extent; remember now, we want our original layers, not the calibrationRasters which were based on the native range.

suitMap<-predict(me_LQ05,x=layers)

plot(suitMap)
```

![](Day3_Tutorial_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
# Let's mask out the regions where extrapolation is a problem.

suitMap_noExt<-mask(suitMap,mess_binary)

# Let's plot the suitability map (minus areas where extrapolation would be needed) and add our range and locality data 

plot(suitMap_noExt)
plot(full_range_laea,add=TRUE)
points(locs[,c("x","y")])
```

![](Day3_Tutorial_files/figure-markdown_github/unnamed-chunk-13-2.png)

``` r
### Let's save a version of this as a PDF so we can compare across the class.

pdf("./MaxentModels/Final_Model/<YOURNAME>_suitability_map.pdf") # save with your name or group's name
  plot(suitMap_noExt)
  plot(full_range_laea,add=TRUE)
  points(locs[,c("x","y")])
dev.off()

# Email me a copy of your pdf and I'll post it the website. It would be great if you also send me a point-form list of any unique modeling decisions you made so we can see how decisions made influenced the final predictions.


### Finally, you probably want to save the projected version of the full range, your mess maps, and any other modeling objects etc. that you haven't saved already (perhaps the evaluation list etc.)
```

##### QUESTIONS:

1.  Where are the most suitable conditions for the species within it's native range? Are there any problems with this prediction? In the case of the map I show above, there certainly are. What is responsible for these issues?
2.  Based on your own results, how well are we able to make predictions outside of the species' native range?
3.  Of the areas where extrapolation may be less of a problem, which are the most suitable?
4.  According to your model, are there areas in North America where the species could be but wasn't at the time the range shapefile was made? What might this mean for conservation?
5.  Given the data at hand (including the original locality dataset), what are some other types of questions we could answer?

##### ADVANCED CHALLENGE:

The map we made above is not necessarily nice to look at. See if you can figure out how to make it nicer. Some suggestions:
1) Change the colour scheme (look into "colorRampPalette" and "RColorBrewer" in R).
2) Use the "raster" package to add a scale bar.
3) Look into the package "GISTools" for several other mapping tools, including how to add a north arrow.
4) Figure out how to colour the ocean and areas outside of the study extent dark grey and areas we are masking out because of extrapolation a lighter shade of grey (this should also outline North America nicely).
Again, I will hopefully one day get around adding some of my own scripts to my Github site. So check back regularly for additional tips.
