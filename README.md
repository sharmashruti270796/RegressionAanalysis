# RegressionAanalysis
Wine is an alcoholic beverage that has been a most favoured drink for numerous
occasions for centuries. Wine is made from fermented grapes. The yeast in grapes
consumes its sugar and breaks down into two compounds i.e ethanol ans carbon
dioxide. Wine has been further divided into two categories i.e red wine and white
wine which is formed from the red and white grapes respectively.There has been many
restrictions within different countries that impose a quality check on wine to make
sure it is good for consumption.Hence it is important to detect the quality of wine
that is being used for customer satisfaction. The quality of wine depends on variety of
factors that need to be taken into account. In this paper we will address the factors
on which the quality of wine depends on. We will also check if we can easily predict
the quality of wine given the variables of the data. Regression analysis will be used
for our experiments ,interpretations and results. Some of the the questions we will
address in this paper will be whether the , if there is any correlation between the
variables, if the variables in data are significant for our wine quality , what are the
top chemical properties chemical properties that differentiate high and low quality
wine and if we are correctly able to predict the wine quality through are final model.
1
CHAPTER 2
Dataset
The dataset we are currently using is Red and White Wine quality [1]. The
dataset consists of 13 variables that comprise of 12 quantitative variables and 1
category variable as shown below.
• type - type of wine either red or white
• fixed.acidity - most wine-related acids are either fixed or nonvolatile
• volatile.acidity - the amount of acetic acid in wine, which when present in excess
can give wine a bad, vinegar-like flavor
• citric.acid - present in wines in small amounts, can give them a "freshness" and
flavor
• residual.sugar - the quantity of sugar left over after fermentation is complete, is
what makes a wine sweet. It’s uncommon to find wines with less than one gram
of sugar per liter
• chlorides - the wine’s salt content
• free.sulfur.dioxide - reduces microbial development and wine oxidation by being
in equilibrium with bisulfite ions and molecular SO2 (as a dissolved gas)
• total.sulfur.dioxide - comprised of both free and bound forms, and in small
amounts
• density - Depending on the percentage of alcohol and sugar content, water’s
density is comparable to that of water
• pH - The pH scale ranges from 0 (extremely basic) to 14 (very acidic), with
most wines falling somewhere between 3 and 4 on the scale.
• sulphates - wine ingredient that may raise the amount of sulfur dioxide gas
(S02), an antioxidant and antibacterial.
• alcohol - The wine’s percentage alcohol content
2
• quality - quality rating of wine ranging from 1 to 7
The range of quality values for the two types of wines is shown in the Figure 1
Figure 1: Dataset Details - Quality(Response)
3
CHAPTER 3
Experiments and Analysis
We have conducted various statistical procedures and analysis on our dataset.
We started with stratified split on our data since the quality ranges from 3 to 9 , we
need to make sure our training sample has equal proportions of each quality. The
training sample consists of 5199 data points and validation consists of 1298 data
points.
We started by fitting the full model without the category variable first to check
our model summary as shown in Figure 2. It was found that our 𝑅2 accounted for
0.2921 which means only 29.21% of variability is explained through the model and the
residual standard error is 0.735. The summary shows that citric acid and chlorides are
not significant for the model as they have high p-value of 0.168 and 0.146 respectively
with a very low t-test value. This shows that given all other predictors are included
in the model , presence of citric acid and chlorides individually is not significantly
different from zero.
