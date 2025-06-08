## 21 Nov 2015

## clear workspace environment
rm(list=ls())

## necessary packages
if (!require(car)) {install.packages("car"); require(car)} 
if (!require(psy)) {install.packages("psy"); require(psy)}
if (!require(lsr)) {install.packages("lsr"); require(lsr)}
if (!require(multcomp)) {install.packages("multcomp"); require(multcomp)}
op<-par(mfrow=c(1,1))

## get data
data.all <- read.csv('Exp1a_Election_post.csv',header = TRUE)
head(data.all)
dim(data.all)

####===================================== Checks =========================================
## Experiencers from Day 1 (5 Nov) and Day 2 (6 Nov) not significantly different  from one another
exper.repub.1 <- data.all[ which(data.all$condition == 'republican.win' & data.all$date == '1'),] 
exper.repub.2 <- data.all[ which(data.all$condition == 'republican.win' & data.all$date == '2'),] 
exper.dem.1 <- data.all[ which(data.all$condition == 'democrat.loss' & data.all$date == '1'),] 
exper.dem.2 <- data.all[ which(data.all$condition == 'democrat.loss' & data.all$date == '2'),] 

t.test(exper.repub.1$rating, exper.repub.2$rating, paired = FALSE)
t.test(exper.dem.1$rating, exper.dem.2$rating, paired = FALSE)

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

## in-group evaluation by party
ingroup.win <- data.all[ which(data.all$condition == 'ingroup.win'),] 
fore.demInwin <- data.all[ which(data.all$condition == 'ingroup.win' & data.all$party == '1'),] 
fore.repInwin <- data.all[ which(data.all$condition == 'ingroup.win' & data.all$party == '2'),] 
ingroup.win$party <- factor(ingroup.win$party, levels = c("1", "2"))
leveneTest(ingroup.win$rating ~ ingroup.win$party)
t.test(fore.demInwin$rating, fore.repInwin$rating, paired = FALSE, var.equal = TRUE)

ingroup.loss <- data.all[ which(data.all$condition == 'ingroup.loss'),] 
fore.demInloss <- data.all[ which(data.all$condition == 'ingroup.loss' & data.all$party == '1'),] 
fore.repInloss <- data.all[ which(data.all$condition == 'ingroup.loss' & data.all$party == '2'),] 
ingroup.loss$party <- factor(ingroup.loss$party, levels = c("1", "2"))
leveneTest(ingroup.loss$rating ~ ingroup.loss$party)
t.test(fore.demInloss$rating, fore.repInloss$rating, paired = FALSE, var.equal = TRUE)

## out-group evaluation by party
outgroup.win <- data.all[ which(data.all$condition == 'outgroup.win'),] 
fore.demOutwin <- data.all[ which(data.all$condition == 'outgroup.win' & data.all$party == '1'),]
fore.repOutwin <- data.all[ which(data.all$condition == 'outgroup.win' & data.all$party == '2'),]
outgroup.win$party <- factor(outgroup.win$party, levels = c("1", "2"))
leveneTest(outgroup.win$rating ~ outgroup.win$party)
t.test(fore.demOutwin$rating, fore.repOutwin$rating, paired = FALSE, var.equal = TRUE)

outgroup.loss <- data.all[ which(data.all$condition == 'outgroup.loss'),]
fore.demOutloss <- data.all[ which(data.all$condition == 'outgroup.loss' & data.all$party == '1'),]
fore.repOutloss <- data.all[ which(data.all$condition == 'outgroup.loss' & data.all$party == '2'),]
outgroup.loss$party <- factor(outgroup.loss$party, levels = c("1", "2"))
leveneTest(outgroup.loss$rating ~ outgroup.loss$party)
t.test(fore.demOutloss$rating, fore.repOutloss$rating, paired = FALSE, var.equal = TRUE)

####==================================== Results =========================================
## Split by win and loss conditions
data.win <- data.all[ which(data.all$condition == 'ingroup.win' | data.all$condition == 'outgroup.win' | 
                              data.all$condition == 'neutral.win' | data.all$condition == 'republican.win'),]
data.loss <- data.all[ which(data.all$condition == 'ingroup.loss' | data.all$condition == 'outgroup.loss' | 
                              data.all$condition == 'neutral.loss' | data.all$condition == 'democrat.loss'),]

## Win Conditions
data.win$condition <- factor(data.win$condition, levels = c("republican.win", "neutral.win", "ingroup.win", "outgroup.win"))
reg.win <- lm(rating ~ condition, data = data.win)
anova(reg.win)
etaSquared(reg.win, type = 2, anova = FALSE )

# Trend analysis
trend <- lm(rating ~ poly(as.numeric(condition), 2), data = data.win)
summary(trend)
confint(trend)

# Pairwise t-tests
postHocs.win <- glht(reg.win, linfct = mcp(condition = "Tukey"))
summary(postHocs.win)
confint(postHocs.win)

# Means and Stdevs
ingroup.win <- data.all[ which(data.all$condition == 'ingroup.win'),] 
round(mean(ingroup.win$rating), 2)
round(sd(ingroup.win$rating), 2)

outgroup.win <- data.all[ which(data.all$condition == 'outgroup.win'),] 
round(mean(outgroup.win$rating), 2)
round(sd(outgroup.win$rating), 2)

unspecified.win <- data.all[ which(data.all$condition == 'neutral.win'),] 
round(mean(unspecified.win$rating), 2)
round(sd(unspecified.win$rating), 2)

exp.win <- data.all[ which(data.all$condition == 'republican.win'),] 
round(mean(exp.win$rating), 2)
round(sd(exp.win$rating), 2)



## Loss Conditions
data.loss$condition <- factor(data.loss$condition, levels = c("democrat.loss", "neutral.loss", "ingroup.loss", "outgroup.loss"))
reg.loss <- lm(rating ~ condition, data = data.loss)
anova(reg.loss)
etaSquared(reg.loss, type = 2, anova = FALSE )

# Trend analysis
trend.loss <- lm(rating ~ poly(as.numeric(condition), 2), data = data.loss)
summary(trend.loss)
confint(trend.loss)

#Pairwise t-tests
postHocs.loss <- glht(reg.loss, linfct = mcp(condition = "Tukey"))
summary(postHocs.loss)
confint(postHocs.loss)

# Means and Stdevs
ingroup.loss <- data.all[ which(data.all$condition == 'ingroup.loss'),] 
round(mean(ingroup.loss$rating), 2)
round(sd(ingroup.loss$rating), 2)

outgroup.loss <- data.all[ which(data.all$condition == 'outgroup.loss'),] 
round(mean(outgroup.loss$rating), 2)
round(sd(outgroup.loss$rating), 2)

unspecified.loss <- data.all[ which(data.all$condition == 'neutral.loss'),] 
round(mean(unspecified.loss$rating), 2)
round(sd(unspecified.loss$rating), 2)

exp.loss <- data.all[ which(data.all$condition == 'democrat.loss'),] 
round(mean(exp.loss$rating), 2)
round(sd(exp.loss$rating), 2)


####======================================= endit ========================================