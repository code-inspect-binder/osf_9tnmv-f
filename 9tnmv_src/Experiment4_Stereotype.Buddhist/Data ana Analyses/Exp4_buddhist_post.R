## 2 March 2016

## clear workspace environment
rm(list=ls())

## necessary packages
if (!require(car)) {install.packages("car"); require(car)} 
if (!require(psy)) {install.packages("psy"); require(psy)}
if (!require(lsr)) {install.packages("lsr"); require(lsr)}
if (!require(multcomp)) {install.packages("multcomp"); require(multcomp)}
if (!require(reshape)) {install.packages("reshape"); require(reshape)}
op<-par(mfrow=c(1,1))

## get data
data.all <- read.csv('Exp4_buddhist_post.csv',header = TRUE)
head(data.all)
dim(data.all)

## factor condition
data.all$condition <- factor(data.all$condition, levels = c("buddhist","neutral","ingroup","outgroup"))

## subset data
buddhist <- data.all[ which(data.all$condition == 'buddhist'),]
unspecified <- data.all[ which(data.all$condition == 'neutral'),]
ingroup <- data.all[ which(data.all$condition == 'ingroup'),] 
outgroup <- data.all[ which(data.all$condition == 'outgroup'),] 


####=============================== Manipulation Check ===================================
## Cronbach's alpha (in-group)
iden.ing.a <- c("ingroup.value", "ingroup.like", "ingroup.connected") 
iden.ing <- data.all[iden.ing.a]
cronbach(iden.ing)

## Cronbach's alpha (out-group)
iden.outg.b <- c("outgroup.value", "outgroup.like", "outgroup.connected") 
iden.outg <- data.all[iden.outg.b]
cronbach(iden.outg)

## in-group evaluation and out-group evaluation
round(mean(data.all$ingroup.average), 2)
round(sd(data.all$ingroup.average), 2)
round(mean(data.all$outgroup.average), 2)
round(sd(data.all$outgroup.average), 2)

t.test(data.all$ingroup.average, data.all$outgroup.average, paired = TRUE)


####==================================== Results =========================================

## ==== Rating Omnibus ====
data.all$condition <- relevel(data.all$condition, ref = "neutral")
rating.all.aov <- lm(rating ~ condition, data = data.all)
anova(rating.all.aov)
etaSquared(rating.all.aov, type = 2)

## ==== Rating Linear Trend ====
data.all$condition <- relevel(data.all$condition, ref = "buddhist")
trend.rating <- lm(rating ~ poly(as.numeric(condition), 2), data = data.all)
summary(trend.rating)
confint(trend.rating)

## ==== Rating Post-hoc Comparisons ====
postHoc.rating <- glht(rating.all.aov, linfct = mcp(condition = "Tukey"))
summary(postHoc.rating)
confint(postHoc.rating)


## === Rating Means and Standard Deviations of Conditions ====
# Unspecified Rating
round(mean(unspecified$rating), 2)
round(sd(unspecified$rating), 2)

# In-group Rating
round(mean(ingroup$rating), 2)
round(sd(ingroup$rating), 2)

# Out-group Rating
round(mean(outgroup$rating), 2)
round(sd(outgroup$rating), 2)

# Buddhist Rating
round(mean(buddhist$rating), 2)
round(sd(buddhist$rating), 2)


## ==== Peer Rank Omnibus ====
data.all$condition <- relevel(data.all$condition, ref = "neutral")
peerrank.all.aov <- lm(peer.rank ~ condition, data = data.all)
anova(peerrank.all.aov)
etaSquared(peerrank.all.aov, type = 2)

## ==== Peer Rank Linear Trend ====
data.all$condition <- relevel(data.all$condition, ref = "buddhist")
trend.rank <- lm(peer.rank ~ poly(as.numeric(condition), 2), data = data.all)
summary(trend.rank)
confint(trend.rank)

## ==== Peer Rank Posthoc Comparisons ====
postHoc.rank <- glht(peerrank.all.aov, linfct = mcp(condition = "Tukey"))
summary(postHoc.rank)
confint(postHoc.rank)

## === Peer Rank Means and Standard Deviations of Conditions ====
# Unspecified Peer Rank
round(mean(unspecified$peer.rank), 2)
round(sd(unspecified$peer.rank), 2)

# In-group Peer Rank
round(mean(ingroup$peer.rank), 2)
round(sd(ingroup$peer.rank), 2)

# Out-group Peer Rank
round(mean(outgroup$peer.rank), 2)
round(sd(outgroup$peer.rank), 2)

# Buddhist Peer Rank
round(mean(buddhist$peer.rank), 2)
round(sd(buddhist$peer.rank), 2)

## ==== Comparing Both Linear Trends ====
# pull and organize relevant data
col <- c("rating","peer.rank2","condition")
data.a <- data.all[col]
head(data.a)
data.new <- melt(data.a)
head(data.new)
colnames(data.new)[2] <- "time"
data.new$time <- factor(data.new$time)

data.new$condition <- relevel(data.new$condition, ref = "buddhist")
trend.rating.rank <- lm(value ~ time * poly(as.numeric(condition), 2), data = data.new)
summary(trend.rating.rank)
confint(trend.rating.rank)

####======================================= endit ========================================