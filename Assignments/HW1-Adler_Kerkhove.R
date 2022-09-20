## Goal of this assignment
# to review basic stats functions and assumption checking in R

## How to complete this assignment
# read through the comments and run the code below, writing and running your own code where indicated
# whenever you see a comment line that begins with Q, discuss that question with your partner and answer it together in a one or two line comment
# turn in your modified code by emailing the R file to me at ojensen@wisc.edu before the next class meeting (due 9am Monday, September 19)
# Hat tip: Modified from original assignment by Rae Winfree

## SOME HANDY THINGS TO KNOW, IF YOU ARE NEW TO R STUDIO
# you can set your working directory under Files, by clicking on the file path you want one folder at a time, then choosing 'more' and 'set as working directory'
# to run code from the script window, highlight it and hit control[command]+enter
# to run code in the console, just hit return
# R studio will highlight the opening parenthesis of the closing parenthesis you are typing
# if you want to put some text in quotes, highlight the text and type quote mark (single and double work the same)
# when you type the first parenthesis following a function, a yellow box will show you the arguments of that function

## Outline
# correlation
# regression
# anova
# multiple regression and ancova
# extra credit exercises

# preliminaries
# clear R's brain
rm(list = ls())
# load libraries
library(readr)
library(ggplot2) 
library(dplyr)
library(datasets)
# get data
# mtcars data are from 1974 Motor Trend car road tests
# click on mtcars in environment pane to see the row names (observations), which in this case are types of cars
# search help pane for mtcars to get more info on the data
data(mtcars)
glimpse(mtcars)

## correlation

# our correlation question: are car weight and mpg correlated?

# first, data picture
ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point()

# second, stats test
cor(mtcars$mpg, mtcars$wt, method = "pearson")

# third, check assumptions, in this case that both x and y are normally distributed
qqnorm(mtcars$mpg)
qqnorm(mtcars$wt)

# Q: put in words how the wt distribution differs from a normal distribution, along the lines of what was discussed in lecture
###A: The weight is not normal because there seems to be cluster of weights between 3 and 4 where the line plateaus rather than continues a straight line
# check your intuition

# Q: Is the Spearman or Pearson correlation a better choice here and why?
###A: Spearman seems like a better choice because the relationship does not appear to be linear but rather slightly curved at the highest weights

hist(mtcars$wt)
# while you're at it, check your other variable. should always look at histogram along with qq plot
hist(mtcars$mpg)
# Q: why do you think the histograms look only iffily normal, while the qq plots look pretty good?
### A: Only have 32 observations, and there is only a small range of values so the binning is narrow. 
# fourth, interpret results
# oops, we forgot to get a p value!
# which is actually good, because we shouldn't be thinking about the results until after checking assumptions
cor.test(mtcars$mpg, mtcars$wt)

        
## regression

# preliminaries
# clear R's brain
rm(list = ls())
# load libraries
library(readr)
library(ggplot2) # autoplot
library(ggfortify) # ggplot needs this to use autoplot for lm
library(dplyr)
library(datasets)
# get data
data(mtcars)
glimpse(mtcars)
# 'tidy data' means that each column is one variable, and each row is one observation
# Q: are these data tidy? why or why not?
### A: Yes this is tidy.
# our regression question: does car weight predict mpg?

# first, data picture
ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point()
# think about the analysis before doing it
# Q: what do you expect the slope to be, roughly?
### A: somehwere near -1

# Q: the intercept?
### A: Somewhere between 30 and 35. 

# Q: the df for the error?  Why?
### A: the degrees of freedom for the error is 30, because the sample size is 32 and we have one parameter which is weight.

# you should also use the data picture to check assumptions informally to decide what model to run
# linearity and homoskedasticity look pretty good, so try lm()
# note, you will need to run and save the regression model in order to get more detailed checks on assumptions

# second, run the linear model
# R reads the lm function as saying, 'fit a linear model, where we predict mpg as a function of car weight, using variables from the mtcars data frame'
car_mod <- lm (mpg ~ wt, data = mtcars)

# third, check assumptions
# first look at a histogram of the residuals
hist(car_mod$residuals)
# autoplot() produces same figures that plot() does in base R
autoplot(car_mod)
# the default blue lines are distracting and don't mean much, remove them next time
autoplot(car_mod, smooth.colour = NA)
# Q: for each of the 4 figures, write down what you are looking for and what you found 
### A: Residiuals vs fitted<- random dispursion in the plot, while this has a clustering in the middle
      #Normal QQ <- straight line from left to right, this plot looks pretty similar
      #Scale Location<-  We are looking for a straight line across the plot that indicates that the errors are randomly distributed across the values. This plot is close, but not perfect.
      # Residuals Vs leverage<- this plot we are looking for all the values to be centered around the 0 line to indicate that each value has the same amount of pull on the regression line. This data shows that there are influential points in the data pulling the regression. 
# fourth, interpret stats
# summary() gives you the regression-type output including the coefficients
summary(car_mod)
# for more details look under environment / values
# Q: are the slope, intercept, and error df roughly what you expected?
### A: The intercept 37.29 which is close to our guess, the slope is much steeper than we expected, and we calculated the DF correctly.
# anova() gives you anova-type output such as SS
anova(car_mod) 
# Q: compare the mean square for wt versus residuals. what does this result mean, and how does it lead to the p value?
### A: the Mean SQ: residual ratio indicates that weight is responsible for most of the variation in mpg, which leads to a very low p-value

# fifth, plot the model back onto the data 
# first extract the coefficients from the model output
# look at the output under Environment / Values first, to make sure you are extracting the right stuff
car_mod_intercept = coef(car_mod)[1]
car_mod_slope = coef(car_mod)[2]
# Q: what could happen if you didn't define the slope and intercept terms like this, but just typed in the numerical values instead?
### A: it would require more effort to go back and change code if your dataset was expanded/ you wanted to manipulate the data
# use geom_abline to plot using slope and intercept
# use manual x and y limits to override the defaults and see the intercept
ggplot(mtcars, aes(x = wt, y = mpg)) + 
        geom_point() + 
        geom_abline(intercept = car_mod_intercept, slope = car_mod_slope) +
        xlim(0,6) + ylim(10,40)
	
# add the confidence intervals
mtcars$lwr =  predict.lm(car_mod, interval="confidence", level = 0.95)[,"lwr"]
mtcars$upr =  predict.lm(car_mod, interval="confidence", level = 0.95)[,"upr"]
# Q: what is the interpretation of the confidence interval you just created?
### A: The interval from which we are 95% confident that the linear regression represents
# Q: if you used interval="prediction" instead, what would be the interpretation? would the ribbon be wider or narrower?
### A: The prediction interval shows a range of points where 95% of data is predicted to fall based on the model provided.
# make the data + model results picture
ggplot(mtcars, aes(x = wt, y = mpg)) + 
        geom_point() + 
        geom_abline(intercept = car_mod_intercept, slope = car_mod_slope) +
        geom_ribbon(data=mtcars,aes(ymin=lwr,ymax=upr),alpha=0.3)  # alpha makes things transparent



## some extra tools for doing regression

# extra tool 1:  standardizing variables
# let's center the wt variable, so that we can interpret the intercept as the mpg of a car of average weight, as opposed to the mpg of a car of weight=0 
# Q optional: why will the slope, p value, R2 all be the same?
### A: yes, all those parameters will stay the same.

mtcars$wt_centered = scale(mtcars$wt, center=TRUE, scale=FALSE) # center=true subtracts the mean from all data values, scale=true divides all data values by the sd 
car_mod2 = lm(mpg ~ wt_centered, data = mtcars)
summary(car_mod2)

ggplot(mtcars, aes(x = wt_centered, y = mpg)) + 
        geom_point() + 
        geom_abline(intercept = coef(car_mod2)[1], slope = coef(car_mod2)[2]) +
        geom_vline(xintercept = 0, linetype="dashed") 

# say we want to standardize both variables, i.e., do the analysis in terms of z scores
# clearly now the slope and intercept will both be different, but the R2 and significance should be the same
# Q optional: what are the units of a z score?  why? how does this make a z score particularly useful (or not)?
### A: the units of a z score are the number of standard derivations from the mean
mtcars$wt_z = scale(mtcars$wt, center = TRUE, scale = TRUE) 
mtcars$mpg_z = scale(mtcars$mpg, center = TRUE, scale = TRUE)
car_mod3 = lm(mpg_z ~ wt_z, data = mtcars)
summary(car_mod3)

ggplot(mtcars, aes(x = wt_z, y = mpg_z)) + 
        geom_point() + 
        geom_abline(intercept = coef(car_mod3)[1], slope = coef(car_mod3)[2]) +
        geom_vline(xintercept = 0, linetype="dashed") +
        geom_hline(yintercept = 0, linetype="dashed")



## anova 
# preliminaries
rm(list = ls())
library(datasets)
library(ggplot2)
library(ggfortify)
library(car) # for levene's test  
library(mosaic) # for contrasts. library(multicomp) is another option
glimpse(iris)

# our anova question: do irises of different species have different sepal widths?

# first, data picture
ggplot(iris, aes(x = Species, y = Sepal.Width)) +
        geom_boxplot() +
        geom_point(color = "purple", alpha = 0.5) +
        theme_bw()
# Q: what assumptions of anova can you roughly check by eye, and how do they look?
### A: The data is all independent both within and among groups. The groups have each come from a normal distribution, and the variation within groups is the same
# second, run model
iris_mod <- lm (Sepal.Width ~ Species, data = iris)

# third, check assumptions
autoplot(iris_mod, smooth.colour = NA)
# Q: for each of the four plots, say what assumption you were checking, how you checked it (i.e. what you looked for in the plot), and what you found
### A: Residuals<- Random, this is not random, QQ<- line, this is a clear line, Scale-Location<- horizontal line, this is not that, Constant Leverage<- All points should cluster around the 0 line, and some do not.
# good idea to check the variances of the groups separately, since that is the assumption of anova actually and autoplot doesn't do it
# levene's test for homogeneity of variances
leveneTest(iris_mod)
# looks okay

# fourth, interpret results
summary(iris_mod)
# Q: what does the estimate for the intercept mean? what do the t values mean? what does the R2 tell you?  the F and its p value?
### A: the estimate for the intercept is where our line intersects the y axis, the t-values indicate that the species are significantly different than each other. R2 indicates that 40% of the variance in sepal length can be attributed to species. The F statistic and the p-value indicate that there is significant differences between sepal lengths in the species

# if you wanted to force R to compare the group means to the grand mean instead of using a reference group, you could do that by suppressing the intercept and centered the outcome variable on zero
# it's a bit complicated though so we are not going to do that here
anova(iris_mod)
# Q: why are the df what they are? what do the two MS values tell you?
###A: 2 for species because there are 3 levels, and 147 because there are 150 observations (n-2-1)
# the null hypothesis we have just rejected is that all groups come from populations with the same mean
# that is, the F value suggests this is unlikely
# but anova itself does not tell you which group mean(s) are different from which other(s)
# need a tukey test for comparing all pairwise means
TukeyHSD(iris_mod, ordered = TRUE)
# Q: interpret your results
### A: this shows that virginica and versicolor are the most similar when comparing sepal lengths, but still signifiantly different. Setosa and versicolor are the most different. 
# check your results, at least the diff column
by_sp <- group_by(iris, Species)
summarize(by_sp, meanSW = mean(Sepal.Width))

# remember R orders levels, and thus chooses the reference level, alphabetically
# check the ordered list of levels:
levels(iris$Species)
# you can reorder the factor levels by giving it a new list
iris$Species = factor(iris$Species, levels = c("virginica", "versicolor", "setosa"))
levels(iris$Species)
# now your reference level and therefore some of your output will be different, although the F and p value should be the same of course
iris_mod <- lm (Sepal.Width ~ Species, data = iris)
summary(iris_mod)
# Q:  what components of the summary() output change when you reorder levels, and why?  what components stay the same and why?
###A: the p-values change because the lm treats one of the factors as the standard for the intercept, and the rest of the parameters are to change the intercept for that given species, in this case versicolor has an intercept that is -.2 lower than virginica. The overall f statistic and p-value are the same because they indicate if the groups are significantly different from eachother, and the order does not matter for that. 
# CHECK need to sort out answer to the above Q 


# fifth, plot model results back onto the data picture
# we can easily get the mean for each level and the associated se using the predict() function
iris_mean_se = predict(iris_mod, se=T, newdata=data.frame(Species=c("virginica", "versicolor", "setosa")))
# make a new dataframe to hold the mean and se output from the anova
output= data.frame(Species = c("virginica", "versicolor", "setosa"), 
	mean = iris_mean_se[[1]],
	se = iris_mean_se[[2]])
output
# now that we made a new dataframe, R has assigned the species order as alphabetical again, so...
output$Species = factor(output$Species, levels = c("versicolor", "virginica", "setosa"))
levels(output$Species)
# plot the means with error bars (+/- 1 SE)
ggplot(output, aes(x = Species, y = mean)) +
	geom_point(size=3) +
	geom_errorbar(data=output, aes(x=Species, ymin=mean-se, ymax=mean+se), width=.25)+
	ylab("model estimated mean +/- 1 SE") +
	theme_bw()
# plot the means with 95% CIs
ggplot(output, aes(x = Species, y = mean)) +
	geom_point(size=3) +
	geom_errorbar(data=output, aes(x=Species, ymin=mean-1.96*se, ymax=mean+1.96*se), width=.25)+
	ylab("model estimated mean and 95% CI") +
	theme_bw()
# check that you got the right numbers by comparing the means in your boxplots to the below
# we did this already above, but now let's do the same thing using piping from dplyr
check_means <- iris %>%
        group_by(Species) %>%
        summarize(meanSW = mean(Sepal.Width))
check_means
# we still need to add anova results to indicate which group mean(s) are different from which other(s)
# but, the significance stars and letters are easier to add in keynote/powerpoint/adobe than R, so we won't do that here





## multiple regression 

# preliminaries
# clear R's brain
rm(list = ls())
# load libraries
library(readr)
library(ggplot2) 
library(ggfortify) 
library(dplyr)
library(datasets)

# get data
data(mtcars)
mtcars

# our multiple regression question: do car weight and horsepower predict mpg?

# first, data pictures
ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point()
ggplot(mtcars, aes(x = hp, y = mpg)) +
        geom_point()
ggplot(mtcars, aes(x = hp, y = wt)) +
        geom_point()

# second, run model
car_mod2 <- lm(mtcars$mpg ~ wt + hp, data=mtcars)
summary(car_mod2)

# third, check assumptions the same as simple linear regression
hist(car_mod2$residuals)
autoplot(car_mod2, smooth.colour = NA)
# Q: for each plot, write down what you would conclude
###A: Residuals vs fitted seems like there are more negative than positive numbers, QQ plot looks linear until we look at the high values, scale location is not a horizontal line, the maserati has a lot of leverage on the line. 
# for multiple regression, also very important to check for multicollinearity
# first have a look at correlation of the two predictors:
# anything less than 0.7 is usually viewed as nothing to worry about, but bivariate correlations like this don't really determine how the model will be affected
cor(mtcars$wt, mtcars$hp)
# a better metric is the variance inflation factor, which actually measures how the model is affected
# as a guideline, vif should be less than 10 for each predictor
vif(car_mod2)


# fourth, interpret results
# Q: interpret your results
### A: There doesn't seem to be too much correlation between the parameters        
# fifth, plot results onto data picture
# this is difficult with 2 predictors--the most straightforward way to visualize would be a 3d plot:
library(scatterplot3d)
plot3d <-scatterplot3d(mtcars$wt,mtcars$hp,mtcars$mpg, pch=16, type="h", main="3D Scatterplot")
plot3d$plane3d(car_mod2)

# another useful method is a partial residual plot, which plots the effect of each predictor after controlling for the effect of the others
# warning, partial residual plots are not recommended when dealing with interactions, although this is sometimes done anyway, and I don't know exactly what the issues are
par(mfrow=c(1,2))
termplot(car_mod2, partial.resid=TRUE, col.res = "black")
# or equivalently:
library(car)
crPlots(car_mod2)
# set the par back now in case people run all the following stuff without closing their graph window
par(mfrow=c(1,1))



## ancova
# this exercise is modified from Beckerman et al 2017, Getting started with R
# preliminaries
# clear R's brain
rm(list = ls())
# load libraries
library(readr)
library(ggplot2) 
library(ggfortify) 
library(dplyr)
library(datasets)
# get some data on egg-laying by limpets
limpet <- read_csv("limpets.csv") #Note: need to have a valid path here
# click dataset name in environment / data to take a look
glimpse(limpet)

# our ancova question: does the mean number of eggs laid by limpets vary by limpet density and/or season?

# first, data picture
ggplot(limpet, aes(x = DENSITY, y = EGGS, color = SEASON)) +
        geom_point() +
        scale_color_manual(values = c(spring="green", summer="red")) +  # this is how you set colors manually in ggplot
        theme_bw()
# Q: what is your prediction about the answer to the ancova question?
### A: it varries by season and density. There are more eggs overall at lower densities and in the spring.
# Q: use the picture to estimate the slope for each season. what are the units of the slope?
###A. the slope is approximately -.03 in the spring and -.06 in the summer. eggs per one unit density of limpit.
# Q: do you expect the effect of density depends on season? that is, do you predict an interaction? explain why or why not
### A: no I do not suspect an interaction because the slopes seem the same for both seasons

# second, run model
limpet_mod <- lm(EGGS ~ DENSITY + SEASON + DENSITY:SEASON, data = limpet)
# look at all the output info R stores for you, under Values
names(limpet_mod)

# third, check assumptions
autoplot(limpet_mod, smooth.colour = NA)

# fourth, interpret results
# this gets a bit hairy, for ancova. both summary() and anova() are relevant
anova(limpet_mod)
# here is an interpretation of the anova table from the top row down
# R estimated a joint slope for both seasons, and it explained lots of variance, with MS 5.0
# R then estimated different intercepts for the two seasons, and that explained a bit more variance, for a MS of 3.3
# Last, R allowed the slopes to vary (the interaction), but doing so didn't explain much; MS 0.1

summary(limpet_mod)
# coefficients table: here is where we get our intercept and slope
# Q: write the model equation for egg production in the spring in y = a + bx form, filling in all coefficients and variable names
### A: y = 2.66 + (-.034X)
# SEASONsummer estimate is the difference between spring and summer in terms of egg production. in other words, a shift in the intercept
# Q: write the model equation for egg production in the summer in y = a + bx form, filling in all coefficients and variable names
### y= 1.85 -.03X
# you could use the DENSITY:SEASONsummer coefficient to flatten the slope by 0.003, but it won't make much difference, and anyway, wasn't significant
# the bottom line: R2 of 0.67, p<0.0001
# t test asks if the difference between 2 values is different from zero. so, what two values? is the question
# intercept: is intercept different from 0. yes
# density: is slope of density different from 0. yes
# SEASONsummer: is difference between the intercept of summer and spring different 0. yes
# DENSITY:SEASONsummer: is difference between the slopes of density between spring and summer different from 0. no


# fifth, plot results onto data picture
# we can just plot the estimated lines on the original data picture, being sure to add the coefficients correctly
ggplot(limpet, aes(x = DENSITY, y = EGGS, color = SEASON)) +
        geom_point() +
        scale_color_manual(values = c(spring="green", summer="red")) +  
		geom_abline(intercept = coef(limpet_mod)[1], slope = coef(limpet_mod)[2], colour="green") +
		geom_abline(intercept = (coef(limpet_mod)[1]+coef(limpet_mod)[3]), slope = (coef(limpet_mod)[2]+coef(limpet_mod)[4]), colour="red") +
        theme_bw()







