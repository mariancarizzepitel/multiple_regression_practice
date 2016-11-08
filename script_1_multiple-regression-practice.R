library(tidyverse)
my.data <- read_csv("regLectureData.csv")
glimpse(my.data)
library(apaTables)
apa.cor.table(my.data)
psych::pairs.panels(as.data.frame(my.data)) # check that none of correlations are curvilinear

my.regression <- lm(VidScore ~ iq + age, data=my.data) # regression analysis
print(my.regression) # gives you the b weights
summary(my.regression) # more details! than print(); b-weights are under column ESTIMATE 
apa.reg.table(my.regression) # summary of regression analysis, in APA(/SPSS) format

x_axis_range <- data.frame(age=c(43),iq=c(130)) # given an individual with.. what is best guess? 
x_axis_range
CI_data <- predict(my.regression, newdata= x_axis_range, interval="confidence", level=.95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
CI_data # to find best guess estimate sample mean for people with these demographics
# Best guess for someone aged 43 with IQ of 130 is 128.97
PI_data <- predict(my.regression, newdata= x_axis_range, interval ="prediction", level=.95)
PI_data <- as.data.frame(cbind(x_axis_range, PI_data))
PI_data # look at lower and upper limits for best prediction for population mean for people with demographics
# Best guess for people in future samples - and what score they're likely to have (between 110 to 147)

# FINISH EXERCISES! Partial and semi-partial correlations 
# DO AND THINK ABOUT THEM 





head(attitude)
reg1 <- lm(rating ~ complaints + privileges, data=attitude) 
reg2 <- lm(rating ~ complaints + privileges + learning, data=attitude)
# example of HIERARCHICAL regression 

anova(reg1,reg2) # F values and significance for change in r2 (is the delta r2 between R1 and R2 significant?) 
## if not significant, learning did not add anything significant 

apa.reg.table (reg1,reg2)
## because delta r2 is not significant, that means no significant change due to addition 