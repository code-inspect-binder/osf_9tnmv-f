## 21 Nov 2015

## clear workspace environment
rm(list=ls())

## necessary packages and settings
if (!require(car)) {install.packages("car"); require(car)} 
if (!require(psy)) {install.packages("psy"); require(psy)}
if (!require(lsr)) {install.packages("lsr"); require(lsr)}
if (!require(multcomp)) {install.packages("multcomp"); require(multcomp)}
op<-par(mfrow=c(1,1))

## get data
data.all <- read.csv('Exp2_OnlineExp_POST.csv', header = TRUE)
head(data.all)
dim(data.all)

####=============================== Manipulation Check ===================================
## Cronbach's alpha (in-group)
iden.ing.woOutliers.a <- c("ingroup.value", "ingroup.like", "ingroup.connected") 
iden.ing.woOutliers <- data.all[iden.ing.woOutliers.a]
cronbach(iden.ing.woOutliers)
# 0.95

## Cronbach's alpha (out-group)
iden.outg.woOutliers.b <- c("outgroup.value", "outgroup.like", "outgroup.connected") 
iden.outg.woOutliers <- data.all[iden.outg.woOutliers.b]
cronbach(iden.outg.woOutliers)
# 0.91

## in-group evaluation and out-group evaluation
round(mean(data.all$ingroup.average), 2)
round(sd(data.all$ingroup.average), 2)
round(mean(data.all$outgroup.average), 2)
round(sd(data.all$outgroup.average), 2)

t.test(data.all$ingroup.average, data.all$outgroup.average, paired = T)

####==================================== Results =========================================
## ANOVA
data.all$condition <- factor(data.all$condition, levels = c("experiencer","neutral", "self", "ingroup", "outgroup"))
aov<-lm(rating ~ condition, data = data.all)
anova(aov)
summary(aov)
etaSquared(aov, type = 2)

## Linear Trend
trend <- lm(rating ~ poly(as.numeric(condition), 2), data = data.all)
summary(trend)
confint(trend)

## Planned Contrasts
expNeu <- c(1, -1, 0, 0, 0)
expIn <- c(1, 0, 0, -1, 0)
expOut <- c(1, 0, 0, 0, -1)
expSelf<-c(1, 0, -1, 0, 0)
a <- cbind(expNeu, expIn, expOut, expSelf)
contr <- a%*%solve(t(a)%*%a)
contrasts(data.all$condition) <- cbind(contr)
aov2 <- lm(rating ~ condition, data = data.all)
summary(aov2)
confint(aov2)

## Post-hoc Comparison for In-group/Out-group
postHoc.forecast <- glht(aov, linfct = mcp(condition = "Tukey"))
summary(postHoc.forecast)
confint(postHoc.forecast)

## Means and Standard Deviations of Conditions
# Experiencer
exp <- data.all[ which(data.all$condition == 'experiencer'),] 
round(mean(exp$rating), 2)
round(sd(exp$rating), 2)

# Self
self <- data.all[ which(data.all$condition == 'self'),] 
round(mean(self$rating), 2)
round(sd(self$rating), 2)

# Unspecified
unspecified <- data.all[ which(data.all$condition == 'neutral'),] 
round(mean(unspecified$rating), 3)
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