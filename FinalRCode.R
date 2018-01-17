# Ruiyang Chen
# Liya Wang
# Sascha Hagedorn
# Maximilian Ott
#SCM 651 Final Project

# Load required packages
library(dplyr)
library(car)
library(googleVis)
library(Metrics)

# We used the average prices to determine our three UPCs
# Two high and one low price UPC
# Preparation code
#oj.FL <- oj[which(oj$orange_high_movement_upc == "1630015121"),] 
#oj.HH <- oj[which(oj$orange_high_movement_upc == "3828154001"),] 
#oj.TROP <- oj[which(oj$orange_high_movement_upc == "4850000102"),] 

#oj.all <- rbind(oj.FL, oj.HH, oj.TROP)
#str(oj.all)
#oj.3 <- oj.all
#We did a split before 
#data loading with chosen upcs 
df_full <- read.csv("df_full.csv")
dfmas <- df_full
#
str(dfmas)
colnames(dfmas)
#
dfmas[, c(32,33,44)] <- sapply(dfmas[, c(32,33,44)], as.character)
dfmas[, c(43,29:31)] <- sapply(dfmas[, c(43,29:31)], as.factor)

#
##### QUeestions #####

###1.How does the demand for a brand depend on price? What is the price elasticity of demand of a brand?
lm1 <- lm(logMOVE ~ orange_high_movement_BRAND + logPRICE + orange_high_movement_BRAND*logPRICE + Feat*orange_high_movement_BRAND + Feat + QTY + AGE9 + AGE60 + ETHNIC + EDUC + NOCAR + INCOME + HHSINGLE + HHLARGE + WORKWOM + HVAL150 + SINGLE + RETIRED + UNEMP + NWHITE + POVERTY + DRTIME5 + SSTRDIST + SSTRVOL + CPDIST5 + CPWVOL5, data=dfmas)
summary1 <- summary(lm1)
summary1


###2. Is price elasticity different for different brands?
# Test for
# orange_high_movement_BRANDHH:logPRICE = 0
# orange_high_movement_BRANDTROPICANA PURE PREM:logPRICE = 0
c1 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0)
c2 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0)
r1 <- c(0,0)
h1 <- rbind(c1,c2)

linearHypothesis(lm1,h1,rhs=r1)

## 3. How does demand depend on whether the product is on sale (Feat =1)? 
## Is this dependence same for all brands?
# Effect of Feat
summary1


# Test for
#orange_high_movement_BRANDHH:Feat1 = 0
#orange_high_movement_BRANDTROPICANA PURE PREM:Feat1 = 0

r1 <- c(0,0)

#prepare the matrix for the left hand side
c1 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0)
c2 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)

h1 <- rbind(c1,c2)

linearHypothesis(lm1,h1,rhs=r1)
##### 5.What demographic factors affect demand? #####
democoeff <- summary1$coefficients[7:26]
demolabels = colnames(dfmas)[c(9:28)]
dfdemocoeff <- data.frame("Label" = demolabels, "Coefficients" = democoeff)
dfdemocoeff <- dfdemocoeff[order(-dfdemocoeff$Coefficients),]
Column <- gvisColumnChart(dfdemocoeff)
# Googlevis column plot
plot(Column)
# DF for Demographics
dfdemocoeff
# Hypothesis tests for demographics
# full model
l5 <- lm(logMOVE ~ orange_high_movement_BRAND+ orange_high_movement_BRAND * logPRICE
         +QTY+logPRICE+Feat+Feat*orange_high_movement_BRAND+AGE9+AGE60+ETHNIC+EDUC+NOCAR
         +INCOME+HHSINGLE+HHLARGE+WORKWOM+HVAL150+SINGLE+RETIRED+UNEMP+NWHITE+POVERTY+DRTIME5+SSTRDIST+SSTRVOL+CPDIST5
         +CPWVOL5,data = dfmas)

summary(l5)
#
# Age9 Age60
l5.1 <- lm(logMOVE ~ orange_high_movement_BRAND+ orange_high_movement_BRAND * logPRICE
           +QTY+logPRICE+Feat+Feat*orange_high_movement_BRAND+ETHNIC+EDUC+NOCAR
           +INCOME+HHSINGLE+HHLARGE+WORKWOM+HVAL150+SINGLE+RETIRED+UNEMP+NWHITE+POVERTY+DRTIME5+SSTRDIST+SSTRVOL+CPDIST5
           +CPWVOL5,data = dfmas)

anova(l5,l5.1) # dependent on Age
#
# Ethnic NWHITE
l5.2 <- lm(logMOVE ~ orange_high_movement_BRAND+ orange_high_movement_BRAND * logPRICE
           +QTY+logPRICE+Feat+Feat*orange_high_movement_BRAND+AGE9+AGE60+EDUC+NOCAR
           +INCOME+HHSINGLE+HHLARGE+WORKWOM+HVAL150+SINGLE+RETIRED+UNEMP+POVERTY+DRTIME5+SSTRDIST+SSTRVOL+CPDIST5
           +CPWVOL5,data = dfmas)

anova(l5,l5.2) # dependent on Ethnic factors
#
# Education
l5.3 <- lm(logMOVE ~ orange_high_movement_BRAND+ orange_high_movement_BRAND * logPRICE
           +QTY+logPRICE + Feat + Feat * orange_high_movement_BRAND+AGE9+AGE60+ETHNIC+NOCAR
           +INCOME+HHSINGLE+HHLARGE+WORKWOM+HVAL150+SINGLE+RETIRED+UNEMP+NWHITE+POVERTY+DRTIME5+SSTRDIST+SSTRVOL+CPDIST5
           +CPWVOL5,data = dfmas)

anova(l5,l5.3) # dependent on education
#
# Income

l5.4 <- lm(logMOVE ~ orange_high_movement_BRAND+ orange_high_movement_BRAND * logPRICE
           +QTY+logPRICE+Feat+Feat*orange_high_movement_BRAND+AGE9+AGE60+ETHNIC+EDUC+NOCAR
           +HHSINGLE+HHLARGE+WORKWOM+HVAL150+SINGLE+RETIRED+UNEMP+NWHITE+ POVERTY+DRTIME5+SSTRDIST+SSTRVOL+CPDIST5
           +CPWVOL5,data = dfmas)

anova(l5,l5.4) # not dependent on income
#
# Poverty

l5.5 <- lm(logMOVE ~ orange_high_movement_BRAND+ orange_high_movement_BRAND * logPRICE
           +QTY+logPRICE+Feat+Feat*orange_high_movement_BRAND+AGE9+AGE60+ETHNIC+EDUC+NOCAR+ INCOME
           +HHSINGLE+HHLARGE+WORKWOM+HVAL150+SINGLE+RETIRED+UNEMP+NWHITE+DRTIME5+SSTRDIST+SSTRVOL+CPDIST5
           +CPWVOL5,data = dfmas)

anova(l5,l5.5) # not dependent on poverty
### General Statistics
### Log Price Desc. Statistics
pricevary <- dfmas %>%
  group_by(orange_high_movement_BRAND) %>%
  summarise(avglogprice = mean(logPRICE),
            sdlogp = sd(logPRICE), 
            minlp = min(logPRICE),
            maxlp = max(logPRICE),
            medlp = median(logPRICE),
            rangelp = max(logPRICE) - min(logPRICE)) %>%
  arrange(desc(avglogprice))
pricevary
# Price Variation over Time
# FL
dffl <- dfmas %>%
  filter(orange_high_movement_BRAND == "FL NAT HOMESQ") %>%
  select(c(36,40)) %>%
  group_by(WEEK) %>%
  summarise(avgp = mean(PRICE)) %>%
  arrange(WEEK)
# TROP
dftrop <- dfmas %>%
  filter(orange_high_movement_BRAND == "TROPICANA PURE PREM") %>%
  select(c(36,40)) %>%
  group_by(WEEK) %>%
  summarise(avgp = mean(PRICE)) %>%
  arrange(WEEK) 
# HH
dfhh <- dfmas %>%
  filter(orange_high_movement_BRAND == "HH") %>%
  select(c(36,40)) %>%
  group_by(WEEK) %>%
  summarise(avgp = mean(PRICE)) %>%
  arrange(WEEK)
# Plotting
# FL
Linefl <- gvisLineChart(dffl)
plot(Linefl)
# HH
Linehh <- gvisLineChart(dfhh)
plot(Linehh)
# TROP
Linetrop <- gvisLineChart(dftrop)
plot(Linetrop)
# Movement over Time
# FL
dfflm <- dfmas %>%
  filter(orange_high_movement_BRAND == "FL NAT HOMESQ") %>%
  select(c(36,37)) %>%
  group_by(WEEK) %>%
  summarise(avgm = mean(MOVE)) %>%
  arrange(WEEK)
# TROP
dftropm <- dfmas %>%
  filter(orange_high_movement_BRAND == "TROPICANA PURE PREM") %>%
  select(c(36,37)) %>%
  group_by(WEEK) %>%
  summarise(avgm = mean(MOVE)) %>%
  arrange(WEEK) 
# HH
dfhhm <- dfmas %>%
  filter(orange_high_movement_BRAND == "HH") %>%
  select(c(36,37)) %>%
  group_by(WEEK) %>%
  summarise(avgm = mean(MOVE)) %>%
  arrange(WEEK)
# Plotting
# FL
Lineflm <- gvisLineChart(dfflm)
plot(Lineflm)
# HH
Linehhm <- gvisLineChart(dfhhm)
plot(Linehhm)
# TROP
Linetropm <- gvisLineChart(dftropm)
plot(Linetropm)
#
##### Develop a model, and test how it performs on a validation sample. 
#####For example, you can break your data randomly into two parts, estimation sample and validation sample, using Access (standard practice is to use two-thirds as the estimation sample, and other one-third as the validation sample). 
#####Estimate the model using the estimation sample, and assess how well the model predicts the dependent variable(s) in the validation sample.  #####

## Creating Training and Testing Sets 
nrows <- nrow(dfmas)
random.index <- sample(1:nrows, replace =  FALSE)
cutPoint <- floor(nrows/3*2)
oj.train <- dfmas[random.index[1:cutPoint],]
oj.test <- dfmas[random.index[(cutPoint + 1):nrows],]

test <- oj.test
train <- oj.train
#write.csv(oj.train, file = "oj.train.csv")

#train <- file.choose()
#test <- file.choose()

#oj.train <- read.csv(train, header = TRUE, stringsAsFactors = FALSE)
#oj.test <- read.csv(test, header = TRUE, stringsAsFactors = FALSE)

#write.csv(oj.test, file = "oj.test.csv")

#remove income and poverty which have not large significant

### predict function

predict_model <- function(model.train,test)
{ df <- test
df$model.predict <- predict(model.train,test)
df$error <- df$logMOVE-df$model.predict
return(df)
}

lm2 <- lm(logMOVE ~ orange_high_movement_BRAND + logPRICE + orange_high_movement_BRAND*logPRICE + Feat*orange_high_movement_BRAND + Feat + QTY + AGE9 + AGE60 + ETHNIC + EDUC + NOCAR + HHSINGLE + HHLARGE + WORKWOM + HVAL150 + SINGLE + RETIRED + UNEMP + NWHITE + DRTIME5 + SSTRDIST + SSTRVOL + CPDIST5 + CPWVOL5, data=dfmas)
#
pred1 <- predict(lm1, test)
pred2 <- predict(lm2, test)
#
msedf1 <- data.frame("predicted" = pred1, "actual" = test$logMOVE)

print(mse(msedf1$actual, msedf1$predicted))
print(mse(msedf2$actual, msedf2$predicted))

min_max_accuracy1 <- mean(apply(msedf1, 1, min) / apply(msedf1, 1, max))
print(min_max_accuracy1)
min_max_accuracy2 <- mean(apply(msedf2, 1, min) / apply(msedf2, 1, max))
print(min_max_accuracy2)

#####ksvm model


model.ksvm.train <- ksvm(logMOVE ~ orange_high_movement_BRAND+ orange_high_movement_BRAND * logPRICE
                         +QTY+logPRICE+Feat+Feat*orange_high_movement_BRAND+AGE9+AGE60+ETHNIC+EDUC+NOCAR
                         +HHSINGLE+HHLARGE+WORKWOM+HVAL150+SINGLE+RETIRED+UNEMP+NWHITE+DRTIME5+SSTRDIST+SSTRVOL+CPDIST5
                         +CPWVOL5,data = oj.train)

model.ksvm.predict <- predict_model(model.ksvm.train, oj.test)

#Test the model on the testing dataset, and compute the Root Mean Squared Error
#0.7164434
rmse(oj.test$logMOVE,model.ksvm.predict$model.predict)




#####svm model


model.svm.train <- svm(logMOVE ~ orange_high_movement_BRAND+ orange_high_movement_BRAND * logPRICE
                       +QTY+logPRICE+Feat+Feat*orange_high_movement_BRAND+AGE9+AGE60+ETHNIC+EDUC+NOCAR
                       +HHSINGLE+HHLARGE+WORKWOM+HVAL150+SINGLE+RETIRED+UNEMP+NWHITE+DRTIME5+SSTRDIST+SSTRVOL+CPDIST5
                       +CPWVOL5,data = oj.train)

model.svm.predict <- predict_model(model.svm.train, oj.test)

#Test the model on the testing dataset, and compute the Root Mean Squared Error
#0.7146418
rmse(oj.test$logMOVE,model.svm.predict$model.predict)

# Converting Feat to numeric
trainfeat <- train
testfeat <- test
trainfeat[, c(43)] <- sapply(trainfeat[, c(43)], as.numeric)
testfeat[, c(43)] <- sapply(testfeat[, c(43)], as.numeric)
# Full GLM Model 
GLM1 <- glm(Feat ~ logMOVE + orange_high_movement_BRAND + logPRICE + QTY + AGE9 + AGE60 + ETHNIC + EDUC + NOCAR + INCOME + 
              HHSINGLE + HHLARGE + WORKWOM + HVAL150 + SINGLE + RETIRED + 
              UNEMP + NWHITE + POVERTY + DRTIME5 + SSTRDIST + SSTRVOL + 
              CPDIST5 + CPWVOL5, family=binomial(logit), data=trainfeat)
summary(GLM1)
# GLM Model without workwom
GLM1.1 <- glm(Feat ~ logMOVE + orange_high_movement_BRAND + logPRICE + QTY + AGE9 + AGE60 + ETHNIC + EDUC + NOCAR + INCOME + 
                HHSINGLE + HHLARGE + HVAL150 + SINGLE + RETIRED + 
                UNEMP + NWHITE + POVERTY + DRTIME5 + SSTRDIST + SSTRVOL + 
                CPDIST5 + CPWVOL5, family=binomial(logit), data=trainfeat)
anova(GLM1,GLM1.1) #workwom is not significant
# GLM Model without Retired
GLM1.2 <- glm(Feat ~ logMOVE + orange_high_movement_BRAND + logPRICE + QTY + AGE9 + AGE60 + ETHNIC + EDUC + NOCAR + INCOME + 
                HHSINGLE + HHLARGE + HVAL150 + SINGLE + WORKWOM +
                UNEMP + NWHITE + POVERTY + DRTIME5 + SSTRDIST + SSTRVOL + 
                CPDIST5 + CPWVOL5, family=binomial(logit), data=trainfeat)
anova(GLM1,GLM1.2) #retired is not significant
# GLM Model without unemp
GLM1.3 <- glm(Feat ~ logMOVE + orange_high_movement_BRAND + logPRICE + QTY + AGE9 + AGE60 + ETHNIC + EDUC + NOCAR + INCOME + 
                HHSINGLE + HHLARGE + HVAL150 + SINGLE + WORKWOM + RETIRED + NWHITE + POVERTY + DRTIME5 + SSTRDIST + SSTRVOL + 
                CPDIST5 + CPWVOL5, family=binomial(logit), data=trainfeat)
anova(GLM1,GLM1.3) #unemp is not significant
# New Full Model
GLM2 <- glm(Feat ~ logMOVE + orange_high_movement_BRAND + logPRICE + QTY + AGE9 + AGE60 + ETHNIC + EDUC + NOCAR + INCOME + 
              HHSINGLE + HHLARGE + HVAL150 + SINGLE + NWHITE + POVERTY + DRTIME5 + SSTRDIST + SSTRVOL + 
              CPDIST5 + CPWVOL5, family=binomial(logit), data=trainfeat)
summary(GLM2)
# GLM Model without nocar
GLM2.1 <- glm(Feat ~ logMOVE + orange_high_movement_BRAND + logPRICE + QTY + AGE9 + AGE60 + ETHNIC + EDUC + INCOME + 
                HHSINGLE + HHLARGE + HVAL150 + SINGLE + NWHITE + POVERTY + DRTIME5 + SSTRDIST + SSTRVOL + 
                CPDIST5 + CPWVOL5, family=binomial(logit), data=trainfeat)
anova(GLM2,GLM2.1) #nocar is not significant
# New Full Model
GLM3 <- glm(Feat ~ logMOVE + orange_high_movement_BRAND + logPRICE + QTY + AGE9 + AGE60 + ETHNIC + EDUC + INCOME + 
              HHSINGLE + HHLARGE + HVAL150 + SINGLE + NWHITE + POVERTY + DRTIME5 + SSTRDIST + SSTRVOL + 
              CPDIST5 + CPWVOL5, family=binomial(logit), data=trainfeat)
summary(GLM3)
#
colnames(testfeat)
# Prediction
preditsale <- predict(GLM3, test, type = "response")
head(preditsale)
# Defining Treshold
preditsalef <- ifelse(preditsale > 0.5, 1, 0) 
head(preditsalef)
# Creating accuracy table
accuracytable <- table(preditsalef,test[,43])
accuracytable
# Calculating accuracy
accuracy <- (accuracytable[1,1]+accuracytable[2,2])/sum(accuracytable)
accuracy
            
# Preparing sub-dfs and dfs for joining
colnames(dfmas)
dfsub <- dfmas
dfsubfl <- subset(dfsub, orange_high_movement_BRAND == "FL NAT HOMESQ")
dfsubflj <- dfsubfl %>% 
  select(c(32,34,37,38,40,41))
dfsubhh <- subset(dfsub, orange_high_movement_BRAND == "HH")
dfsubhhj <- dfsubhh %>% 
  select(c(32,34,37,38,40,41))
dfsubtrop <- subset(dfsub, orange_high_movement_BRAND == "TROPICANA PURE PREM")
dfsubtropj <- dfsubtrop %>% 
  select(c(32,34,37,38,40,41))
#Join HH, STOREB, TROP to FL 
colnames(dfsubfl)
colnames(dfsubhhj)
# HH
dfsubfl1 <- dfsubfl %>%
  inner_join(dfsubhhj, by = "STOREWEEK", suffix = c("_fl", "_hh")) %>%
  rename(orange_high_movement_BRAND = orange_high_movement_BRAND_fl, PRICE = PRICE_fl, logPRICE = logPRICE_fl, logMOVE = logMOVE_fl, MOVE = MOVE_fl)
# TROP )
colnames(dfsubfl1)
colnames(dfsubtropj)
dfsubfl2 <- dfsubfl1 %>%
  inner_join(dfsubtropj, by = "STOREWEEK", suffix = c("_fl", "_trop"))
View(dfsubfl2)
dfcrossbrand <- dfsubfl2
#without incomt and poverty for crossbrand, and QTY
#FL
lm4 <- lm(logMOVE_fl ~ logPRICE_fl + logPRICE_hh + logPRICE_trop, data=dfcrossbrand)
summary(lm4)
#without incomt and poverty for crossbrand, and QTY
#hh
lm5 <- lm(logMOVE_hh ~ logPRICE_hh + logPRICE_fl + logPRICE_trop, data=dfcrossbrand)
summary(lm5)
#
#without incomt and poverty for crossbrand, and QTY
#hh
lm6 <- lm(logMOVE_trop ~ logPRICE_trop + logPRICE_hh + logPRICE_fl, data=dfcrossbrand)
summary(lm6)
# DF for price startegy and variable costs
VC.calc <- dfmas %>%
  group_by(orange_high_movement_BRAND) %>%
  summarise(avgprice = mean(PRICE),
            avgprofit = sd(PROFIT),
            maxprice = max(PRICE),
            minprice = min(PRICE),
            avgmove = mean(MOVE), 
            maxmove = max(MOVE),
            minmove = min(MOVE))
VC.calc
#VC for FL
am_fl <- 2.69*10.85/100
cat("absolute margin fl", am_fl)
print("#")
vcfl <- 2.69 - am_fl
cat("VC fl", vcfl)
#
#VC for HH
am_HH <- 1.69*15.80/100
cat("absolute margin HH", am_HH)
print("#")
vcHH <- 1.69 - am_HH
cat("VC HH", vcHH)
#
#VC for TROP
am_trop <- 2.70*12.17/100
cat("absolute margin trop", am_trop)
print("#")
vctrop <- 2.70 - am_trop
cat("VC trop", vctrop)

#View(dfmas)
unique(dfmas$orange_high_movement_BRAND)
