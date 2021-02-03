##Set Working Directory - this will need to be reset for your
##own environment - Go to Session tab -> Set working directory -> T source file location
##setwd("I:/Social Work R Guides/Survey Research")

##Load Libraries
library(car)         ## Companion for applied regression   
library(effectsize)  ## Indexes of effect Sizes and standardized parameters    
library(ggplot2)     ## Graphics system
library(dplyr)       ## Comprehensive package for data science
library(knitr)       ## An engine for dynamic report generation with R
library(psych)       ## A general purpose toolbox for research
library(vcd)
library(gplots)      ## Various R Programming tools for plotting data
library(gridExtra)   ## Miscellaneous functions for "Grid" graphics 

##Load Data
survey <- read.csv("survey.csv")  ##load a .csv file
head(survey, 3) ## list a first few records to check your data set
attach(survey)

## Subset school connectedness items into data frame 
sc.items <- as.data.frame(cbind.data.frame(survey$sc.1, survey$sc.2, survey$sc.3, survey$sc.4))
colnames(sc.items) <- c("connect.1", "connect.2", "connect.3", "connect.4")
head(sc.items, 3)  ## check to make sure it worked

## Subset scales into a data frame
scores <- as.data.frame(cbind(survey$connect, survey$efficacy, survey$support, survey$press, survey$stress, 
                              survey$depression))
colnames(scores) <- c("connect", "efficacy", "support", "press", "stress", "depression") 
head(scores)  ## Check to make sure it worked

## Univariate Analysis
## Examining a nominal categorical variable 
# Family tabulation
fam.tab <- table(family)
fam.pct <- prop.table(fam.tab)*100
fam.pct

# Bar chart of family percentages
ggplot(survey, aes(x = family)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)*100), fill="steelblue") +
  ylab("Percent") +
  xlab("Parenting Arangement") +
  ggtitle("family")

##Examining an ordinal categorical variable
##School connectedness tabulations
sc1.counts <- table(sc.1)
sc2.counts <- table(sc.2)
sc3.counts <- table(sc.3)
sc4.counts <- table(sc.4)
sc.counts <- as.data.frame(cbind(sc1.counts, sc2.counts, sc3.counts, sc4.counts))
colnames(sc.counts) <- c("sc.1", "sc.2", "sc.3", "sc.4")
row.names(sc.counts) <- c("SD", "D", "S", "SA")
sc.counts%>% 
  kable(digits = 3, format="pandoc", caption="Connectedness Item Counts")

# Connectedness bar charts
sc1.bar <- ggplot(survey, aes(x = factor(sc.1))) +
  geom_bar(fill = "steelblue") +
  xlab("I feel close to people at my school")
sc2.bar <- ggplot(survey, aes(x = factor(sc.2))) +
  geom_bar(fill = "steelblue") +
  xlab("I am happy to be at my school")
sc3.bar <- ggplot(survey, aes(x = factor(sc.3))) +
  geom_bar(fill = "steelblue") +
  xlab("I feel like I am part of my school")
sc4.bar <- ggplot(survey, aes(x = factor(sc.4))) +
  geom_bar(fill = "steelblue") +
  xlab("I feel safe at my schoo")
grid.arrange(sc1.bar, sc2.bar, sc3.bar, sc4.bar, nrow = 2, ncol = 2)

##Examining continuous data
##Compute common descriptive statistic for continuous scales
##knitr Formatted output
descriptives <- as.data.frame(describe(scores))  
descriptives%>%
  select(mean, sd,median, min, max,range, skew, kurtosis)%>%
  kable(digits = 5, format="pandoc", caption="Descriptive Statistics")

## Support Stem-and-leaf
summary(support)
stem(support)

## Support Boxplot
ggplot(survey) +
  geom_boxplot(mapping = aes(y = support ), fill="steelblue") + 
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank()) +
  labs(y = "Support Score") +
  coord_flip()

## Support Boxplot using standard graphics
boxplot(support, horizontal=TRUE)

## Depression Histogram
ggplot(survey) +
  geom_histogram(mapping = aes(x = depression), fill = "steelblue", binwidth = 2) +
  xlab("CES-D Score")

## Bivariate Analysis
## Examining two categorical variables
ggplot(survey) +
  geom_bar(mapping = aes(x=dep_risk, fill = gender), position = "dodge") +
  xlab("Depression Risk") +
  ylab("Frequency") +
  scale_fill_manual(values = c("steelblue","lightgreen"))

##2x2 Crosstabulation
(gendep <- xtabs(~ dep_risk + gender, data = survey))%>% 
  kable(digits = 3, format="pandoc", caption="Depresion Risk by Gender Counts")
(prop.table(gendep, 1))%>% 
  kable(digits = 3, format="pandoc", caption="Depresion Risk by Gender Proportions")
assocstats(gendep)

##Examining a two-level categorical variable and a continuous variable
ggplot(survey) +
  geom_boxplot(mapping = aes(x = gender, y = stress), fill="steelblue") + 
  coord_flip() +
  labs(title = "Stress by Gender", x = "Gender", y = "Stress Score")

##t-test of Depression Gender Differences with Cohen's d Effect Size
t.test(stress ~ gender, data=survey)
cohens_d(stress ~ gender, data=survey)
interpret_d(d = 0.31, rules = "cohen1988")

##Examining a three-level categorical variable and a continuous variable
ggplot(survey) +
  geom_boxplot(mapping = aes(x = family, y = support ), fill="steelblue") + 
  coord_flip() +
  labs(title = "Support by Family", x = "Family", y = "Support Score")

#Anova of Family Situation and Depression
anova <- aov(support ~ family, data=survey)
summary(anova)
TukeyHSD(anova)
eta_squared(anova)

##Examining two continuous variables
ggplot(survey, mapping = aes(x = stress, y = depression)) +
  geom_point() +
  geom_smooth(color = "blue", method = 'lm', se=FALSE) +
  geom_smooth(color = "red", method = 'loess', se=FALSE) +
  labs(title = "Stress by Depression", x = "Stress", y = "Depression")

##Correlation Between Stress and Depression
cor.test(stress, depression, method = "pearson")

##Multivariate Analysis
pairs.panels(scores, main="Scatterplot Matix")

##Regression Modeling
##Multiple Linear Regression
fit <- lm(depression ~ connect + efficacy + press + support + stress, data = scores)
summary(fit)
standardize_parameters(fit)

##Set Correlation
setCor(y=c("stress","depression"), x=c("connect","efficacy","press","support"), data=scores)