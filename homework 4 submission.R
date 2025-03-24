## Homework 4 Submission ## 




# NOTES BEFORE ANSWERING QUESTION 1 ----------------------------------

### Question 1 involves a dataset about the impacts of mistletoe infection
### in Spanish forests, published in Functional Ecology


### The authors of this study assessed the impacts of mistletoe on forest
###  regeneration, measuring the abundance of seedlings below trees that were
###  either parasitized by mistletoe or not parasitized. They hypothesized
###  that mistletoe changes light availability and soil fertility beneath the
###  canopies of parasitized trees, facilitating recruitment of seedlings
###  below these weakened trees and shifting the community composition in
###  these forests.

###  The authors of this study assessed the impacts of mistletoe on forest
###  regeneration, measuring the abundance of seedlings below trees that were
###  either parasitized by mistletoe or not parasitized. They hypothesized
###  that mistletoe changes light availability and soil fertility beneath the
###  canopies of parasitized trees, facilitating recruitment of seedlings
###  below these weakened trees and shifting the community composition in
###  these forests.


r
mistletoe <- read.csv("mistletoes.csv")
head(mistletoe)
```

##   TreeID     Treatment Seedlings Year
## 1      1   parasitized         8 2011
## 2      2 unparasitized         0 2011
## 3      3   parasitized       153 2011
## 4      4 unparasitized         0 2011
## 5      5   parasitized        40 2011
## 6      6 unparasitized         0 2011


###  The full paper includes many variables, but we will focus just on the
###  following variables:
  
###   **TREATMENT** = A categorical variable describing whether a tree is
###  parasitized by mistletoe or not

### **SEEDLINGS** = A count of the number of seedlings in 5m x 5m plots
###  beneath parasitized and unparasitized trees, during the authors’ 2011
###  and 2012 surveys. 

###  **YEAR** = Whether the survey was taken in 2011 or 2012.


# ANSWERING QUESTION 1 ----------------------------------------------------




## Question 1: Mistletoe impacts on seedlings


### Loading libraries:

library(MASS)
library(performance)
library(marginaleffects)
library(modelr)

library(ggplot2)
library(dplyr)



## 1a) (5 pts)** Fit a glm assessing evidence for the following
###  hypothesis: Seedling density is increased beneath trees experiencing
###  mistletoe infection. Describe the rationale you used in selecting a glm
###  structure and probability distribution. Calculate model fit using MAE.


#### ANSWERING QUESTION 1a)---------------------------

mistletoe <- read.csv("mistletoes.csv")



### MY MODEL


#### Now, I will fit a model with a poisson distribution
#### no Including YEAR in model

mod.pois1 <- glm(Seedlings ~ Treatment, data = mistletoe, family = poisson(link = "log"))

summary(mod.pois1)


#### Now I will check for overdispersion

## We can use a dispersion test from the "performance" package.
# This function tests the null hypothesis that the Poisson's assumptions (that variance=mean) are appropriate.
## Reports p-value associated with this hypothesis test (i.e. if p<0.05, then reject hypothesis that variance=mean)

check_overdispersion(mod.pois1)

### or we can use the next code to look for overdispersion

dispersion_ratio <- sum(residuals(mod.pois1, type = "pearson")^2) / df.residual(mod.pois1)
dispersion_ratio

### The function detected overdispersion, so we will use a "Negative Binomial GLM"


mod.nbin <- glm.nb(Seedlings ~ Treatment, data = mistletoe)
summary(mod.nbin)


### Now I will transform the intercept and slope from the log scale to real numbers.

### Intercept
exp(5.7308)

###  Slope

exp(-3.1575)

1/0.04253194


exp(5.7308 + (-3.1575))

### To analyze the effect of mistletoe infection on seedling density,
### we selected a generalized linear model (GLM) 
### because the response variable Seedlings 
### represents count data, it mean number of seedlings under each tree). 
### In IT type of data, values are non-negative integers as 2, 5, 23, etc, 
### and makes that a traditional linear regression unsuitable, 
### as it assumes a normal distribution.


### Now, I will calculate MAE to asses the model performance


mae <- function(model) {
  mean(abs(mistletoe$Seedlings - predict(model, type = "response")))
}


#### Now, I will calculate MAE for both models
mae_poisson <- mae(mod.pois1)
mae_nb <- mae(mod.nbin)

#### Printing results

cat("Poisson MAE:", mae_poisson, "\n")
cat("Negative Binomial MAE:", mae_nb, "\n")


###  MAE (Mean Absolute Error) is a measure of the accuracy of the model, 
### that tells us how much the model prediction deviates on average from the actual values
### of the response variable (Seedlings in this case).
### Thus, the interpretation is:
### A lower MAE indicates that the model predicts the actual values better.
### A higher MAE indicates that the model has more error in its predictions.
### Thus, MAE allows us to compare if Poisson or Negative Binomial makes better predictions 
### about the number of seedlings under the trees.

### Now, Why MAE in Poisson GLM and Negative Binomial is the same (145.841)? 

### It is because both models are fitting the same expected mean for each observation.

## ASW: yes! Exactly -- the reasons for selecting the poisson have more to do with the fact that when there's overdispersion, applying the poisson would violate the assumptions of that statistical tool (and cause us to falsely deflate the p-values!). They do predict similar mean rates, but the negative binomial describes the tail of our data differently, shifting the pvals.


### Poisson GLM assumes that the values of Seedlings follow a Poisson distribution, 
### thus, the variance is equal to the mean.


### Negative Binomial GLM relaxes that assumption by allowing 
### the variance to be greater than the mean, but does not change the estimate of the expected mean.

## ASW: great! the estimate of the mean may shift a bit depending on the variance, but does not move by much in this case.

# ANSWERING QUESTION 1b ----------------------------------------------------




## 1b) (15 pts)** Use visual (e.g. a marginal effects plot) and 
## written (e.g. effect sizes on scale of response) approaches to interpret
## the results of your model. 

## Based on your fitted model results and model fit, 
## write 3-4 sentences to discuss the following biolgical
## conclusions:  
##  Does mistletoe infection alter seedling density? How much does seedling
## recruitment differ beneath parasitized and unparasitized trees? Explain
## which elements of your glm results informed your conclusions and annotate the
## steps you needed to take to interpret your parameters. 




library(ggeffects)

pred <- ggpredict(mod.nbin, terms = "Treatment")


## THe next codes will generate predictions for the effect of Treatment on seedlings density, 
## while holding ppt at its mean.

plot_predictions(mod.nbin, condition="Treatment")
predictions(mod.pois, newdata=data.frame(genotype=c("parasitized", "unparasitized"),
                                         environ.ppt=mean(mistletoe$Seedlings)))

### Also we can use the next code

ggplot(pred, aes(x = x, y = predicted)) +
  geom_col(fill = "skyblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(x = "Treatment", y = "Seedling Density", title = "Effect of Mistletoe on Seedlings") +
  theme_minimal()




### ANSWERING QUESTION 1b

## ASW: no need to use the poisson, once we've demonstrated that we violate the poisson's assumptions if we apply it to this data/model structure!

### Both the negative binomial model and the Poisson GLM model 
###  show that mistletoe infection significantly increases seedling density under parasitized trees. 
### This is observed in the slope value (Treatment unparasitized), which is -3.1575 with a highly significant p-value (<2e-16), 
### indicating that unparasitized trees have lower plant recruitment compared to parasitized trees.

### Regarding the difference in seedlings under parasitized and unparasitized trees, 
### the intercept and slope were transformed from a log scale to real numbers, 
### yielding a number of approximately 308 seedlings under parasitized trees
###  and 13 under unparasitized trees (4.25% of the plant density of parasitized trees); 
### that is, parasitized trees have 23.4 times more seedlings than unparasitized trees.

### The number of seedling under parasitized trees and unparasitized trees can be seen
###  in plot prediction, as well.


## ASW: awesome work!



# ANSWERING QUESTION 1C ---------------------------------------------------



## 1c) (10 pts)** During the course of this study, 2012 was an atypically
## rainy year, compared to 2011. Fit an additional glm that quantifies how
## the effect of mistletoe differs between the two years in this study.
## Write ~2 new sentences that summarize the results of the new model and their
## biological implications.


### Now, I will fit a model with interaction Treatment * year


glm_interaction.nbin <- glm.nb(Seedlings ~ Treatment * Year, data = mistletoe)


summary(glm_interaction.nbin)

### Since the model coefficients are on a log scale, we will transform them to an exponential scale,
### so that they can generate real values.

### Year effect

exp(0.6000)  

### Treatment: Year interaction effect


## ASW: Remember that the slope on its own can't be interpreted in terms of "the amount" of seedlings
exp(0.8956)  


### ANSWERING QUESTION 1c



### The model suggests that the effect of mistletoe on seedling density was slightly different
###  between 2011 and 2012, but the coefficients were not significant.

### That is, Treatmentunparasitized:Year = 0.8956 (log scale) suggests that seedling density
###  on unparasitized trees was 2.45 (exp scale) times higher in 2012 (more rain) 
###  than in 2011 (less rain), but this was not significant (p-value = 0.0599). 
###  Although the higher rainfall may have increased seedling numbers on both parasitized and unparasitized
###  trees in 2012, there was no clear effect of mistletoe infection across years.


## ASW: This is really close! For interpreting the interaction try:
predictions(glm_interaction.nbin, newdata = data.frame(Treatment=c("parasitized", "unparasitized"),
                                                       Year=c(2011, 2011, 2012, 2012)))


## The difference between parasitized and unparasitized trees shifted from 213.6 in 2011 to  376.6 in year 2!


## ASW: 27/30

# NOTES BEFORE ANSWERING QUESTION 2 ---------------------------------------


## Questions 2 uses the “treemortality” dataset in this repository. Forest
## thinning is a forest management approach that aims to remove woody fuels
## from forests before wildfires to reduce subsequent severity of these
## disturbances. This data set examines the effects of a forest thinning
## treatment on tree mortality in a subsequent wildfire in forests in the
## Sierra Nevada mountains.

## In 2019, researchers measured the diameters of \>10,000 Ponderosa pine
## trees. Following this initial survey, some of the sampled areas happened
## to receive thinning treatments, applied by the US Forest Service in
## 2020-2022, in a fashion unrelated to the researchers’ original study
## design. In 2023, a large portion of their sampled area then burned in a
## wildfire, creating a “natural experiment.”

## In Fall 2023, the researchers returned to re-sample survival 1000 of
## these trees, to determine whether past thinning actions helped reduce
## tree mortality (0=survived; 1=died). They recorded mortality, tree size
## (cm in diameter), and whether the tree was located in an area that had
## received a thinning treatment (**thinning**, a categorical variable,
##   where 1 indicates that the plot received a thinning treatment). Due to
## the observational nature of their study, the researchers were worried
## about possible confounding relationships generated by the fact that: a)
## thinning treatments were more likely to occur in stands with lots of
## small trees, rather than larger diameter trees, and b) larger trees are
## more likely to survive fire. Therefore, they resampled the 1000 trees in
## a randomized fashion by tree size, to ensure that small and large stems
## were recorded equally commonly across thinned and unthinned forests.

## The researchers are interested in the following question: Does thinning
## decrease the probability of tree mortality in wildfire?
  
  ``` r
treemortality <- read.csv("treemortality.csv")
head(treemortality)
```

##   X mortality thinning  roaddist    slope   treesize
## 1 1         0        1 1.4148852 23.34290 12.3003737
## 2 2         0        0 0.5419534 29.92641  6.9480055
## 3 3         0        1 1.6301725 17.94698  2.1784680
## 4 4         0        1 3.7380622 16.96357 10.5182755
## 5 5         0        1 3.3916483 25.06625  0.5099761
## 6 6         0        0 6.9336465 20.41989  9.7882557



## Question 2:



# ANSWERING QUESTION 2a ---------------------------------------------------



## 2a) (5 pts)** Fit a glm (using a probability distribution of your
##choice) that reflects the following research question (including thinning
## as your only predictor and mortality as your response): Do forest
## thinning treatments reduce the probability of tree mortality? Interpret
## the results of the glm by writing 2-4 sentences about the biological
## significance of the effect of thinning, including descriptions of the
## sizes of the effect on the scale of the response variable, evidence
## for/against the hypothesis, and a metric of model fit.


treemortality <- read.csv("treemortality.csv")
head(treemortality)

#### Now, I will fit a GLM (binomial logistic regresion)


glm1_thinning1 <- glm(mortality ~ thinning, data = treemortality, family = binomial)

summary(glm1_thinning1)


### We also can use the next GLM

glm_thinning2 <- glm(mortality ~ thinning, 
                    family = binomial(link = "logit"), 
                    data = treemortality)

summary(glm_thinning2)

### Now I will transform the intercept and slope from the log scale to real numbers.

### Intercept


###  Slope
## ASW: See above, but transforming the slope on its own doesn't tell us about anything on the probability scale, and we need to use plogis to convert to probability scale for the binomial (though exp will tell you about the "odds")
exp(-1.8559)

1-0.1563122


### The glm shows that forest thinning treatments reduce the probability of tree mortality in wildfires.
### It can be seen in the negative slope coefficient (-1.8559) and the significant p value (p-value =2e-16) 
### But in the case of the Intercept and slope are in log scale, but translating it
### in probability scale means that, the probability of tree mortality of the thinned forest 
### is just the 15.63%, compare with the tree mortality in unthinned forest.
###  In other words, thinning treatment reduce mortality in wildfire in 84.37%.

### ASW: Probability in unthinned forest =
plogis(0.99)

## Probability in thinned forest = 
plogis(0.99 -1.8559)

## Thinning decreases probability of mortality from 73% to 30%

# ANSWERING QUESTION 2b ---------------------------------------------------


## 2b)(2 pts)** The researchers explicitly considered the potential for
## confounding relationships related to tree size in their design and
## randomized their post-fire sampling by tree size. Given this
## information, do the researchers need to incorporate tree size into their
## glm to accurately estimate the effect of thinning? Why or why not?


### To answer it question first of all I will compare tree size between thinned and unthinned forest
### with the next code

boxplot(treesize ~ thinning, data = treemortality, main="Tree Size Distribution by Thinning",
        xlab="Thinning (0 = No, 1 = Yes)", ylab="Tree Size (cm)", col=c("lightblue", "lightgreen"))

# Now, I will perform a t-test to compare mean between thinned and unthinned forest

t.test(treesize ~ thinning, data = treemortality)

#### The t-test shows no significant difference in tree size between thinned and unthinned forests 
### (p-value = 0.3536), confirming that tree size was adequately controlled in the study design.
### The boxplots suggest that tree sizes are balanced between clarified and unclarified forests.

### Both results confirm that the researchers correctly controlled for tree size in the study design, 
### so, it is not necessary to include tree size in the GLM.

###  The data collection process is already balanced. 
### This means that its effect on mortality is not systematically different between 
### thinned and unthinned forests, and adding it to the model would not modify the effect of thinning,

### as randomization ensures that thinning is independent of tree size.
### Including unnecessary variables can reduce model efficiency, 
### adding complexity without improving the accuracy of the estimation of the thinning effect.


## ASW: This is an amazing answer!! ! You could include it for other reasons, but it's not needed to accurately estimate the effect of thinning!



# ANSWERING QUESTION 2C ---------------------------------------------------




## 2c)(5 pts)** The researchers submit their analysis for peer review,
## and one of the reviewers raises some concerns about the sampling
## methods. The researchers were unable to control the placement of forest
## thinning treatments (which are controlled by a variety of ecological,
## Logistical, and political factors), so the reviewer is concerned about
## confounding relationships that might bias the thinning effect.

## In the reviewer’s experience, thinning treatments are not randomly
## applied across the landscape, and tend to occur in places that are
## easier to access with the heavy machinery required for thinning. For
## instance, steeper slopes might be less likely to be thinned, and they
## also tend to foster higher fire severities; similarly, forest patches
## that are farther from the road may also be less likely to be thinned and
## also experience higher fire severities, because they are farther away
## from firefighting/fire suppression activities the occur more commonly
## along roads. The reviewer sends the researchers the DAG below to
## summarize their concerns and asks the researchers to fit a new model
## that accounts for these confounding relationships. The researchers
## calculate the slope of the terrain around each sampled tree and how far
## each tree was from the nearest road, and add these variables (roaddist
 ## (in km), and slope (in degrees)) to their dataset.


## Refit the model from 2a to include the necessary variables to minimize
## bias in our estimation of the “thinning” variable, based on the
## reviewer’s proposed DAG (above). Does the effect of “thinning” change?
## If so, describe the degree of change and why the two models may differ
## in their conclusions. If needed, modify your model interpretation from 2a.

## (It is not required to include any biological/ecological hypotheses
## here that require additional or deeper knowledge about forest fires,
##  thinning treatments, etc – assume that the DAG proposed is complete.
##  Though feel free to come up with a biological explanation too, if you’d
##  like! …and feel free to scale variables where you find it useful!)*


### I will update the model to include confusion variables (SLPE and ROADDIST) 
### as predictors in the GLM.

glm_updated <- glm(mortality ~ thinning + slope + roaddist, family = binomial, data = treemortality)
summary(glm_updated)



### CONTINUING ANSWERING 1C

### According to the model that includes slope and road distance, 
### the effect of thinning continues to significantly reduce mortality (5.3e-05). 
### However, there is a reduction in the effect of thinning on reducing tree mortality 
### in wildfires in the new model (slope thinning = -0.91627) 
### compared to the original model (slope thinning = -1.8559).

### In the particular case of the slope in the new model, 
### it can be observed that the slope gradient is positive (Slope = 0.82480) 
### and highly significant (p-value = 2e-16), indicating that steeper slopes increase mortality.

### In the particular case of Roaddist, the slope is also positive (slope = 0.54468) 
### and its p-value is highly significant (2e-16), indicating that trees 
### located further from roads have a higher probability of mortality in wildfires compared 
### to those closer to roads.

### The new GLM that includes both Slope and Roaddist has a lower Residual Deviance (517.52) 
### compared to the original model (1196.2), indicating that the new model has a better fit 
### than the original model.

### The above results support the reviewer's concerns regarding the DAGs he proposed in the new model.


## ASW: Great answer! Try converting those slopes into a conversation about how the estimated effect of thinning shifts (on the probability scale).

## 16/20

## 43/50 

