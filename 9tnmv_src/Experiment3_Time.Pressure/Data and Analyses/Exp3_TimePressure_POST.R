## 21 Nov 2015

## clear workspace environment
rm(list=ls())

## necessary packages
if (!require(car)) {install.packages("car"); require(car)} 
if (!require(psy)) {install.packages("psy"); require(psy)}
op<-par(mfrow=c(1,1))

## get data
data.all <- read.csv('Exp3_TimePressure_post.csv',header = TRUE)
head(data.all)
dim(data.all)

####=============================== Manipulation Check ===================================
## Cronbach's alpha (in-group)
iden.ing.woOutliers.a <- c("ingroup.value", "ingroup.like", "ingroup.connected") 
iden.ing.woOutliers <- data.all[iden.ing.woOutliers.a]
cronbach(iden.ing.woOutliers)

## Cronbach's alpha (out-group)
iden.outg.woOutliers.b <- c("outgroup.value", "outgroup.like", "outgroup.connected") 
iden.outg.woOutliers <- data.all[iden.outg.woOutliers.b]
cronbach(iden.outg.woOutliers)

## in-group evaluation and out-group evaluation
round(mean(data.all$ingroup.average), 2)
round(sd(data.all$ingroup.average), 2)
round(mean(data.all$outgroup.average), 2)
round(sd(data.all$outgroup.average), 2)

t.test(data.all$ingroup.average, data.all$outgroup.average, paired = TRUE)


####==================================== Results =========================================
## ANOVA
data.all$target <- ifelse(data.all$condition == "ingroup.baseline"|data.all$condition == "ingroup.cogload", "ingroup", 
                           ifelse(data.all$condition == "outgroup.baseline"|data.all$condition == "outgroup.cogload", "outgroup", "voter"))
data.all$cond2 <- ifelse(data.all$condition == "ingroup.baseline"|data.all$condition == "outgroup.baseline"|
                           data.all$condition == "neutral.baseline", "baseline","cogload")
data.all$target <- factor(data.all$target, levels = c("voter","ingroup","outgroup"))

# Interaction Plot
interaction.plot(data.all$target, data.all$cond2, data.all$rating)

# Omnibus
data.all.aov <-lm(rating ~ cond2 + target + cond2*target, data = data.all)
anova(data.all.aov)

data.all$condition <- factor(data.all$condition, levels = c("ingroup.baseline", "outgroup.baseline", "neutral.baseline",
                                                            "ingroup.cogload", "outgroup.cogload", "neutral.cogload"))
outgbase.ingbase <- c(-1, 1, 0, 0, 0, 0)
ingbase.ingcog <- c(1, 0, 0, -1, 0, 0)
outgbase.outgcog <- c(0, 1, 0, 0, -1, 0)
votbase.votcog <- c(0, 0, 1, 0, 0, -1)
ingbase.votbase <- c(1, 0, -1, 0, 0, 0)
a <- cbind(ingbase.ingcog, outgbase.outgcog, votbase.votcog, ingbase.votbase, outgbase.ingbase)
contrasts <- a%*%solve(t(a)%*%a)
contrasts(data.all$condition) <- cbind(contrasts)
long.data.aov3 <-lm(rating ~ condition, data = data.all)
summary(long.data.aov3)
confint(long.data.aov3)

## Means and Standard Deviations of Conditions
# In-group Baseline
ingroup.base <- data.all[ which(data.all$condition == 'ingroup.baseline'),] 
round(mean(ingroup.base$rating), 2)
round(sd(ingroup.base$rating), 2)

# In-group Cog Load
ingroup.cog <- data.all[ which(data.all$condition == 'ingroup.cogload'),] 
round(mean(ingroup.cog$rating), 2)
round(sd(ingroup.cog$rating), 2)

# Out-group Baseline
outgroup.base <- data.all[ which(data.all$condition == 'outgroup.baseline'),] 
round(mean(outgroup.base$rating), 2)
round(sd(outgroup.base$rating), 2)

# Out-group Cog Load
outgroup.cog <- data.all[ which(data.all$condition == 'outgroup.cogload'),] 
round(mean(outgroup.cog$rating), 2)
round(sd(outgroup.cog$rating), 2)

# Unspecified Baseline
unspecified.base <- data.all[ which(data.all$condition == 'neutral.baseline'),]
round(mean(unspecified.base$rating), 2)
round(sd(unspecified.base$rating), 2)

# Unspecified Cog Load
unspecified.cog <- data.all[ which(data.all$condition == 'neutral.cogload'),]
round(mean(unspecified.cog$rating), 2)
round(sd(unspecified.cog$rating), 2)

## Linear Trend
data.base <- data.all[ which(data.all$cond2 == "baseline"), ]
data.cog <- data.all[ which(data.all$cond2 == "cogload"), ]

data.base$condition <- factor(data.base$condition, levels = c("neutral.baseline", "ingroup.baseline", "outgroup.baseline"))
base.trend <- lm(rating ~ poly(as.numeric(condition), 2), data = data.base)
summary(base.trend)
confint(base.trend)

data.cog$condition <- factor(data.cog$condition, levels = c("neutral.cogload","ingroup.cogload","outgroup.cogload"))
cog.trend <-lm(rating ~ poly(as.numeric(condition), 2), data = data.cog)
summary(cog.trend)
confint(cog.trend)

####======================================= endit ========================================