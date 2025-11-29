# Online class of Epidemiology Lab by KZ sir code rewrite by "Sobrul's Stat Talk"

## Example-15.2 by tibble function----------------------------------------------
library(tibble)
table<-tibble(Expose=rep(c("Exposed","Unexposed"),2),
              Disease=c(rep("Disease",2),rep("Non-disease",2)),
              count=c(4,1,2,6)
              )
#creating table
d_tab<-xtabs(count~Disease+Expose, data=table)  #Hierarchy right to left
d_tab
fisher.test(d_tab, alternative = "greater")

library(samplesizeCMH)  # after installing samplesizeCMH
odds.ratio(d_tab)
chisq.test(d_tab, correct = FALSE)

## Example-15.2 by matrix creation----------------------------------------------
data<- matrix(c(4,1,2,6),nrow=2,byrow=TRUE)
colnames(data) <- c("Exposed","Unexposed")
rownames(data) <- c("Disease", "Non-disease")
data
fisher.test(data, alternative = "greater")

library(samplesizeCMH)
odds.ratio(data)
chisq.test(data, correct = FALSE)


## Example-15.3 by tibble function----------------------------------------------
library(tibble)
table<-tibble(Expose=rep(c("Exposed","Unexposed"),2),
              Disease=c(rep("Disease",2),rep("Non-disease",2)),
              count=c(27,44,95,443)
              )
#creating table
d_tab<-xtabs(count~Disease+Expose, data=table)
fisher.test(d_tab, alternative = "greater")
chisq.test(d_tab, correct = FALSE)

library(epitools)   # after installing epitools
riskratio(d_tab, rev='both', method="wald")   #Disease wise
riskratio(t(d_tab), rev='both', method="wald", conf.level=0.95)  #Exposure wise

## Example-15.3 by matrix creation----------------------------------------------
data <- matrix(c(27,44,95,443),nrow=2,byrow=TRUE)
colnames(data) <- c("Exposed", "Unexposed")
rownames(data) <- c("Disease", "Non-disease")
data

fisher.test(data, alternative = "greater")
chisq.test(data, correct = FALSE)

library(epitools)   # after installing epitools
riskratio(data, rev='both', method="wald")   #Disease wise
riskratio(t(data), rev='both', method="wald", conf.level=0.95)  #Exposure wise

library(samplesizeCMH)
odds.ratio(data)


## Example-15.4 by tibble function----------------------------------------------
library(tibble)
table<-tibble(Expose=rep(c("Exposed","Unexposed"),2),
              Disease=c(rep("Disease",2),rep("Non-disease",2)),
              count=c(8,2,30,20)
              )
#creating table
d_tab<-xtabs(count~Disease+Expose, data=table)
odds.ratio(d_tab)
fisher.test(d_tab, alternative = "greater")
chisq.test(d_tab, correct = FALSE)

oddsratio(d_tab, rev='both', method="wald", correction = FALSE, conf.level = 0.95)   #Disease wise


## Example-15.5 by tibble function----------------------------------------------
library(tibble)
table<-tibble(Expose=rep(c("Exposed","Unexposed"),2),
              Disease=c(rep("Disease",2),rep("Non-disease",2)),
              count=c(27,44,95,443)
              )
#creating table
d_tab<-xtabs(count~Disease+Expose, data=table)
fisher.test(d_tab, alternative = "greater")
chisq.test(d_tab, correct = FALSE)

library(epitools)   # after installing epitools
riskratio(d_tab, rev='both', method="wald")   #Disease wise
riskratio(t(d_tab), rev='both', method="wald", conf.level=0.95)  #Exposure wise

## Example 15.5 by tibble function----------------------------------------------
library(tibble)
table<-tibble(Expose=rep(c("Exposed","Unexposed"),2),
              Disease=c(rep("Disease",2),rep("Non-disease",2)),
              count=c(8,2,30,20)
              )
#creating table
d_tab<-xtabs(count~Disease+Expose, data=table)
odds.ratio(d_tab)
fisher.test(d_tab, alternative = "greater")
chisq.test(d_tab, correct = FALSE)

oddsratio(d_tab, rev='both', method="wald", correction = FALSE, conf.level = 0.95)   #Disease wise


## Example 17.2 by tibble function----------------------------------------------
library(tibble)
table<-tibble(Expose=rep(c("Exposed","Unexposed"),8),
              Disease=rep(c(rep("Disease",2),rep("Non-disease",2)),4),
              ECG=c(rep("Age<55 & ECG=0",4),rep("AGE<55 & ECG=1",4),
                    rep("Age=55+ & ECG=0",4),rep("Age=55+ & ECG=1",4)),
              count=c(1,17,7,257,3,7,14,52,9,15,30,107,14,5,44,27)
              )
d_tab<-xtabs(count~Disease+Expose+ECG, data=table)
apply(d_tab,3,odds.ratio)    # 1=row wise, 2=column wise, 3=ECG wise
mantelhaen.test(d_tab, correct = FALSE, alternative = "greater")

library(epitools)
margin_table<-margin.table(d_tab, c(1,2))
chisq.test(margin_table)
odds.ratio(margin_table)
riskratio(margin_table, rev='both', method="wald") #Disease wise
riskratio(t(margin_table), rev='both', method="wald") #Expose wise

##Example 17.2 by dimension-----------------------------------------------------
data<- array(
  c(1,7,17,257,3,14,7,52,9,30,15,107,14,44,5,27),
  dim = c(2, 2, 4),
  dimnames = list(
    Disease<-c("Disease", "Non-disease"),
    Expose<-c("Expose", "Unexpose"),
    ECG_status<-c("AGE<55 & ECG=0", "AGE<55 & ECG=1", 
                  "AGE=55+ & ECG=0", "AGE=55+ & ECG=1")
                 )
             )
data

apply(data,3,odds.ratio)
mantelhaen.test(data,correct = FALSE)#TWO sided
mantelhaen.test(data,correct = FALSE,alternative = "greater")#One sided


margin_table<- margin.table(data,c(1,2))
margin_table

library(epitools)
riskratio(t(margin_table), rev="both",method="wald", conf.level = 0.95)


## Example 17.3 by tibble function----------------------------------------------
library(tibble)
table<-tibble(Expose=rep(c("Exposed","Unexposed"),4),
              Disease=rep(c(rep("Disease",2),rep("Non-disease",2)),2),
              Stratum=c(rep("Strata-1",4),rep("Strata-2",4)),
              count=c(15,5,85,95,5,15,95,85)
              )
d_tab<-xtabs(count~Disease+Expose+Stratum, data=table)
apply(d_tab,3,odds.ratio)  # 1=row wise, 2=column wise, 3=Stratum wise
mantelhaen.test(d_tab, correct = FALSE, alternative = "greater")

library(epitools)
margin_table<-margin.table(d_tab, c(1,2))
chisq.test(margin_table)
odds.ratio(margin_table)
riskratio(margin_table, rev='both', method="wald") #Disease wise
riskratio(t(margin_table), rev='both', method="wald") #Expose wise

## Example 17.3 by dimension----------------------------------------------------
Disease <- c("Disease", "Non-disease")
Expose <- c("Expose", "Unexpose")
Stratum = c("Stratum1","Stratum2")

data<-array(
             c(15,85,5,95,5,95,15,85),
             dim = c(2, 2, 2),
             dimnames = list(
             Disease<-c("Disease", "Non-disease"),
             Expose<-c("Expose", "Unexpose"),
             Stratum<-c("Stratum1","Stratum2")
                             )
            )
data

apply(data,3,odds.ratio)    #Simpson paradox

mantelhaen.test(data, correct = FALSE, alternative = "greater")

margin_table <- margin.table(data5,c(1,2))
margin_table
riskratio(t(margin_table), rev="both", method="wald")


# Example 17.4 (HW)


# Example 17.5
library(tibble)
table<-tibble(Expose=rep(c(rep("Exposed",2),rep("Unexposed",2)),2),
              Disease=rep(c("Disease","Non-disease"),4),
              Stratum=c(rep("Strata-1",4),rep("Strata-2",4)),
              count=c(7,3,0,10,5,5,1,9)
              )
d_tab<-xtabs(count~Expose+Disease+Stratum, data=table)
apply(d_tab,3,odds.ratio)    # 1=row wise, 2=column wise, 3=Stratum wise
mantelhaen.test(d_tab, correct = TRUE,alternative = "greater")




#######Logistic Regression #########
setwd ("C:\\Users\\user\\Desktop\\Nutrition_PropensityScore")
Library (foreign)
dat<- read.spss("KR_file_Child_nutrition_variable. sav", to.data.frame = TRUE)

colnames(dat)
dat_P<- dat[,c("hw70", "hw71", "hw72", "HAZcat", "WAZcat", "WHZcat", "Edu_cat", "Father_edu","v714","Wealth_cat", "No-household", "Media_cat", "bord_cat", "Twin_cat", "b4", "b8", "current_illness", "BMI_cat", "v013", "v024", "v025")]

head(dat_P)
names(dat_P)

dat_P1<-na.omit(dat_P)

# Creating new data set
dat_P1<- dat_P1[,c("HAZcat", "Edu_cat", "BMI_cat")]
table(dat_P1$HAZCat)
table(dat_P1$Edu_cat)
dat_P1$Edu_cat<- ifelse(dat_P1$Edu_cat==0, "No Education", "Formal Edu")

table(dat_P1$Edu_cat)

table(dat_P1$BMI_cat)

dat_P1$BMI_cat<- ifelse(dat_P1$BMI_cat=="18.5-24.9: Normal", "Normal", "NotNormal")
table(dat_P1$BMI_cat)

#Creating cross table
tab<-table(dat_P1$HAZcat, dat_P1$Edu_cat)
tab
library(samlesizeCMH)
odds.ratio(tab)
#odds ratio test
oddsratio(tab, rev="both")
#Odds Ratio using regression
res<- glm(HAZcat~Edu_cat, family=binomial(link=logit), data=dat_P1)
summary(res)
exp(coef(res))

tab<-table(dat_P1$HAZcat, dat_P1$BMI_cat)

# Creating another lair
da_tab<- xtabs(~HAZcat+Edu_cat+BMI_cat, family=binomial(link=logit), data=dat_p1)
da_tab
apply(da_tab, 3, odds.ratio)
#Odds Ratio using regression
res1<- glm(HAZcat~Edu_cat+BMI_cat, family=binomial(link=logit), data=dat_P1)
summary(res1)
exp(coef(res1))

