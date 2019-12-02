######################
### Experimental Design
### Final Project
### May 7, 2018
#######################

setwd('/Users/megansorenson/Documents/Cuddy_project/')
df <- read.table('ccy-clean-data.tab', sep = '\t', header = TRUE)

names(df)
df$id <- as.factor(df$id) ## individual ID's 
df$hptreat <- as.factor(df$hptreat) ### high or low treatment
nlevels(df$id)

df<-subset(df,inelig!="Ineligible (drop)") ### remove ineligible subjects
summary(df$female) ### this has 16 males and 26 females, as reported in paper

### 3 NA's to remove
summary(df$cortm1v2) ### 3 NA's
df <- df[which(df$cortm1v2 > 0),]

summary(df$hptreat)

#### double check the males and females counts with paper
summary(df$female)
### this has 25 females and 14 males.

summary(df$testm1v2)
sd(df$testm1v2, na.rm = TRUE)
summary(df$testm2v2)
sd(df$testm1v2)

### compare summary statistics
dat <- read.table('ccy-clean-data.tab', sep = '\t', header = TRUE)
summary(dat$testm1v2)
sd(dat$testm1v2, na.rm = TRUE)
summary(dat$testm2)
sd(dat$testm2, na.rm =TRUE)

#### then compare by male and female
summary(df$testm1[which(df$female == 'Female')])
summary(df$testm2[which(df$female == 'Female')])

summary(df$testm1[which(df$female == 'Male')])
summary(df$testm2[which(df$female == 'Male')])

#### graph the overall testosterone levels
library(ggplot2)
ggplot(df, aes(x=female, y=testm1v2, fill=female)) + 
  geom_boxplot() + labs(x = "Sex", y = 'Testosterone Level')

######################
### Testosterone Test
#######################

### check the variance
ggplot(df, aes(x=hptreat, y=tdiff, fill=hptreat)) + 
  geom_boxplot() + labs(x = "Power Pose", y = 'Testosterone Level')

var(df$tdiffv2[which(df$hptreat == 'High')])
var(df$tdiffv2[which(df$hptreat == 'Low')])
#### variance looks about the same
###### can we separate with men and women as well?

df$ctestm1v2<-scale(df$testm1v2,scale=F)
df$chptreat<-ifelse(df$hptreat=="High",1,-1)
# df$ctestm1<-scale(df$testm1,scale=F)
# df$chptreat<-ifelse(df$hptreat=="High",1,-1)
# df$testm1<-scale(df$testm1,scale=F)
# df$testm2<-scale(df$testm2,scale=F)
# df$female<-ifelse(df$female=="Female",1,-1)
# df$ctestm2<-scale(df$testm2,scale=F)


#### this model produces results <0.05
summary(m0<-lm(testm2v2~ctestm1v2+chptreat+cortm1v2+cortm2v2+female,df))
summary(m0a<-lm(testm2v2~ctestm1v2+cortm1v2+cortm2v2+female,df))
## effect on testosterone of power posing:
anova(m0,m0a)

### same as just using tdiff
summary(m0<-lm(tdiffv2~ctestm1v2+chptreat+cortm1v2+cortm2v2+female,df))
summary(m0a<-lm(tdiffv2~ctestm1v2+cortm1v2+cortm2v2+female,df))
## effect on testosterone of power posing:
anova(m0,m0a)

fit <-aov(tdiffv2~ctestm1v2+cortm1v2+cortm2v2+female+chptreat,data = df)
summary(fit)


#### Now include an interaction
summary(m0<-lm(testm2v2~ctestm1v2+chptreat+cortm1v2+
                 cortm2v2+female+female*chptreat,df))
#### interaction p-value 0.198

#### look at adding and subtracting variables

### remove post-measurement
summary(m0<-lm(tdiffv2~ctestm1v2+chptreat+cortm1v2+female,df))
summary(m0a<-lm(tdiffv2~ctestm1v2+cortm1v2+female,df))
## effect on testosterone of power posing:
anova(m0,m0a)

### including age
summary(m0<-lm(tdiffv2~ctestm1v2+chptreat+cortm1v2+female + age,df))
summary(m0a<-lm(tdiffv2~ctestm1v2+cortm1v2+female+age,df))
## effect on testosterone of power posing:
anova(m0,m0a)

### include sex and baseline hormone interaction
summary(m0<-lm(tdiffv2~ctestm1v2+chptreat+cortm1v2+female+female:ctestm1v2,df))
summary(m0a<-lm(tdiffv2~ctestm1v2+cortm1v2+female+female:ctestm1v2,df))
## effect on testosterone of power posing:
anova(m0,m0a)


### Effect goes away with interaction

#### Notice that when we remove cort2, the results change a lot
summary(m0<-lm(testm2v2~ctestm1v2+chptreat+cortm1v2+female,df))
summary(m0a<-lm(testm2v2~ctestm1v2+cortm1v2+female,df))
## effect on testosterone of power posing:
anova(m0,m0a)

### interaction plot
m1 <- lm(tdiff~ctestm1v2+cortm1v2+cortm2v2,df)
df$test_resid <- m1$residuals

interaction.plot(df$hptreat, df$female, df$test_resid,
                 xlab = 'Power Pose', 
                 ylab = 'Change in Testosterone 
                 Correcting for Covariates', 
                 main = 'Interaction Plot: Testosterone',
                 trace.label = '')

interaction.plot(df$hptreat, df$female, df$test_resid,
                 xlab = 'Power Pose', 
                 ylab = 'Change in Testosterone', 
                 main = 'Interaction Plot: Testosterone',
                 trace.label = '')


#### Diagnostic plots

m0<-lm(tdiffv2~ctestm1v2+chptreat+cortm1v2+cortm2v2+female,df)

plot(m0, main="Residual plot")

diff<-df$testm2v2-df$testm1v2
## make data frame with differences as DV:

treatment<-df$hptreat
diff_df<-data.frame(subj=1:length(treatment),diff=diff,treatment=treatment)

lowdiff<-subset(diff_df,treatment=="Low")
highdiff<-subset(diff_df,treatment=="High")

mean(lowdiff$diff)
var(lowdiff$diff)
sd(lowdiff$diff)
sd(lowdiff$diff)/sqrt(length(lowdiff$diff))
mean(highdiff$diff)
var(highdiff$diff)
sd(highdiff$diff)
sd(highdiff$diff)/sqrt(length(highdiff$diff))


### graph the results as compared with the paper
par(mfrow=c(1,1))
barplot(lowdiff$diff,main="Low",ylim=c(-50,50),ylab="change in testosterone")
barplot(highdiff$diff,main="High",ylim=c(-50,50),ylab="change in testosterone")
mean(lowdiff$diff)
mean(highdiff$diff)
#### This is not what figure 3 shows in the paper


#####################
### Cortisol test
#####################

df$ccortm1v2<-scale(df$cortm1v2,scale=F)

### check the variance
ggplot(df, aes(x=hptreat, y=cdiff, fill=female)) + 
  geom_boxplot() + labs(x = "Power Pose", y = 'Testosterone Level')
#### variance looks about the same
###### can we separate with men and women as well?

summary(m0<-lm(cortm2v2~ccortm1v2+chptreat+testm1v2+testm2v2+female,df))
summary(m0a<-lm(cortm2v2~ccortm1v2+testm1v2+testm2v2+female,df))
## effect on testosterone of power posing:
anova(m0,m0a)

## see what adding age does
summary(m0<-lm(cortm2v2~ccortm1v2+chptreat+testm1v2+testm2v2+
                 female+age,df))
summary(m0a<-lm(cortm2v2~ccortm1v2+testm1v2+testm2v2+
                  female+age,df))
## effect on testosterone of power posing:
anova(m0,m0a)

summary(m0<-lm(cortm2v2~ccortm1v2+chptreat+testm1v2+female,df))
summary(m0a<-lm(cortm2v2~ccortm1v2+testm1v2+female,df))
## effect on testosterone of power posing:
anova(m0,m0a)

### same as just using cdiff
summary(m0<-lm(cdiffv2~ccortm1v2+chptreat+testm1v2+testm2v2+female,df))
summary(m0a<-lm(cdiffv2~ccortm1v2+testm1v2+testm2v2+female,df))
## effect on testosterone of power posing:
anova(m0,m0a)

#### Now include an interaction

summary(m0<-lm(cdiffv2~ccortm1v2+chptreat+testm1v2+
                 testm2v2+female+female*chptreat,df))

summary(m0<-lm(cdiffv2~female*chptreat,df))
### Effect decreases with interaction
#### Interaction not really close to significant

### Interaction Plot
interaction.plot(df$hptreat, df$female, df$cort_corr,
                 xlab = 'Power Pose', 
                 ylab = 'Change in Cortisol 
              Correcting for Covariates', 
                 main = 'Corrected Interaction Plot',
                 trace.label = '')
interaction.plot(df$hptreat, df$female, df$cdiffv2,
                 xlab = 'Power Pose', 
                 ylab = 'Change in Cortisol', 
                 main = 'Interaction Plot',
                 trace.label = '')


#### Notice that when we remove test2, the results change a lot
summary(m0<-lm(cortm2v2~ccortm1v2+chptreat+testm1v2+female,df))
summary(m0a<-lm(cortm2v2~ccortm1v2+testm1v2+female,df))
## effect on testosterone of power posing:
anova(m0,m0a)

summary(m0<-lm(cortm2v2~ccortm1v2+chptreat+testm1v2+female+age,df))
summary(m0a<-lm(cortm2v2~ccortm1v2+testm1v2+female+age,df))
## effect on testosterone of power posing:
anova(m0,m0a)

summary(m0<-lm(cortm2v2~ccortm1v2+chptreat+testm1v2+female:ccortm1v2,df))
summary(m0a<-lm(cortm2v2~ccortm1v2+testm1v2+female:ccortm1v2,df))
## effect on testosterone of power posing:
anova(m0,m0a)


cor(df$testm1v2, df$cortm1v2)

### Is this the same as tdiff?
summary(m0<-lm(tdiffv2~ctestm1v2+chptreat+cortm1v2+female,df))
summary(m0a<-lm(tdiffv2~ctestm1v2+cortm1v2+female,df))
## effect on testosterone of power posing:
anova(m0,m0a)


#### Diagnostic plots
fit2 <-aov(cdiffv2~ccortm1v2+testm1v2+testm2v2+female+chptreat,data = df)
summary(fit2)
plot(fit2, main="Residual plot")
##these don't really look great

#### Test removing the leverage points
summary(m0<-lm(cortm2v2~ccortm1v2+chptreat+testm1v2+testm2v2+female,df36))
summary(m0a<-lm(cortm2v2~ccortm1v2+testm1v2+testm2v2+female,df36))
## effect on testosterone of power posing:
anova(m0,m0a)
## 3.3377

summary(m0<-lm(cortm2v2~ccortm1v2+chptreat+testm1v2+testm2v2+female,df37))
summary(m0a<-lm(cortm2v2~ccortm1v2+testm1v2+testm2v2+female,df37))
## effect on testosterone of power posing:
anova(m0,m0a)
##2.52

summary(m0<-lm(cortm2v2~ccortm1v2+chptreat+testm1v2+testm2v2+female,dfb))
summary(m0a<-lm(cortm2v2~ccortm1v2+testm1v2+testm2v2+female,dfb))
## effect on testosterone of power posing:
anova(m0,m0a)
## 2.52

cdiffv2<-df$cortm2v2-df$cortm1v2

summary(cdiff)

## make data frame with differences as DV:
treatment<-df$hptreat
diff_dfc<-data.frame(subj=1:length(treatment),diff=cdiffv2,treatment=treatment)

clowdiff<-subset(diff_dfc,treatment=="Low")
mean(clowdiff, na.rm = TRUE)
chighdiff<-subset(diff_dfc,treatment=="High")

### graph the results
op<-par(mfrow=c(2,2),pty="s")
boxplot(lowdiff$diff,main="Low",ylim=c(-50,50),ylab="change in testosterone")
boxplot(highdiff$diff,main="High",ylim=c(-50,50),ylab="change in testosterone")
barplot(lowdiff$diff,main="Low",ylim=c(-50,50),ylab="change in testosterone")
barplot(highdiff$diff,main="High",ylim=c(-50,50),ylab="change in testosterone")

par(mfrow=c(1,1))
barplot(clowdiff$diff,main="Low",ylim=c(-0.3,0.3),ylab="change in cortisol")
barplot(chighdiff$diff,main="High",ylim=c(-0.3,0.3),ylab="change in cortisol")

par(mfrow=c(1,1))
barplot(clowdiff$diff,main="Low",ylim=c(-0.3,0.3),ylab="change in cortisol")
barplot(chighdiff$diff,main="High",ylim=c(-0.3,0.3),ylab="change in cortisol")



mean(clowdiff$diff)
sd(clowdiff$diff)
sd(clowdiff$diff)/sqrt(length(clowdiff$diff))

mean(chighdiff$diff)
sd(chighdiff$diff)
sd(chighdiff$diff)/sqrt(length(chighdiff$diff))





