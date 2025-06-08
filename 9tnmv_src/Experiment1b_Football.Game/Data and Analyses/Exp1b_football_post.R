## 9 February 2016

## clear workspace environment
rm(list=ls())

## necessary packages and settings
if (!require(car)) {install.packages("car"); require(car)} 
if (!require(psy)) {install.packages("psy"); require(psy)}
if (!require(lsr)) {install.packages("lsr"); require(lsr)}
if (!require(multcomp)) {install.packages("multcomp"); require(multcomp)}
op<-par(mfrow=c(1,1))

## get data
data.all <- read.csv('Exp1b_football_post.csv', header = TRUE)
head(data.all)
dim(data.all)

####=============================== Manipulation Check ===================================
## in-group evaluation and out-group evaluation
round(mean(data.all$Ingroup.Connected, na.rm = TRUE), 2)
round(sd(data.all$Ingroup.Connected, na.rm = TRUE), 2)
round(mean(data.all$Outgroup.Connected, na.rm = TRUE), 2)
round(sd(data.all$Outgroup.Connected, na.rm = TRUE), 2)

t.test(data.all$Ingroup.Connected, data.all$Outgroup.Connected, paired = TRUE, na.rm = TRUE)

## in-group evaluation by team
ingroup <- data.all[ which(data.all$condition == 'ingroup'),] 
Harvard.In <- data.all[ which(data.all$condition == 'ingroup' & data.all$team == '1'),] 
Yale.In <- data.all[ which(data.all$condition == 'ingroup' & data.all$team == '2'),] 
ingroup$team <- factor(ingroup$team, levels = c("1", "2"))
leveneTest(ingroup$rating ~ ingroup$team)
t.test(Harvard.In$rating, Yale.In$rating, paired = FALSE, var.equal = TRUE)

## out-group evaluation by team
outgroup <- data.all[ which(data.all$condition == 'outgroup'),] 
Harvard.Out <- data.all[ which(data.all$condition == 'outgroup' & data.all$team == '1'),] 
Yale.Out <- data.all[ which(data.all$condition == 'outgroup' & data.all$team == '2'),] 
outgroup$team <- factor(outgroup$team, levels = c("1", "2"))
leveneTest(outgroup$rating ~ outgroup$team)
t.test(Harvard.Out$rating, Yale.Out$rating, paired = FALSE, var.equal = TRUE)

####==================================== Results =========================================
## ANOVA
data.all$condition <- factor(data.all$condition, levels = c("experiencer", "neutral", "ingroup", "outgroup"))
aov<-lm(rating ~ condition, data = data.all)
anova(aov)
summary(aov)
etaSquared(aov, type = 2)

## Linear Trend
trend <- lm(rating ~ poly(as.numeric(condition), 2), data = data.all)
summary(trend)
confint(trend)

## Post-hoc Comparisons
postHoc <- glht(aov, linfct = mcp(condition = "Tukey"))
summary(postHoc)
confint(postHoc)


## Means and Standard Deviations of Conditions
# Experiencer
exp <- data.all[ which(data.all$condition == 'experiencer'),] 
round(mean(exp$rating), 2)
round(sd(exp$rating), 2)

# Unspecified
unspecified <- data.all[ which(data.all$condition == 'neutral'),] 
round(mean(unspecified$rating), 2)
round(sd(unspecified$rating), 2)

# In-group
ingroup <- data.all[ which(data.all$condition == 'ingroup'),] 
round(mean(ingroup$rating), 2)
round(sd(ingroup$rating), 2)

# Out-group
outgroup <- data.all[ which(data.all$condition == 'outgroup'),]
round(mean(outgroup$rating), 2)
round(sd(outgroup$rating), 2)


####======================================= endit ========================================