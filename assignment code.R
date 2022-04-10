##ACTL4001 Assignment
#------------------------
#Installing relevant packages
library(readxl)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(glmnet)
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(vip)
library(RColorBrewer)
library(forecast)
library(xts)
library(data.table)
library(tidyr)
library(mice)

#Creating file path variables for data
economicdata <- paste(dirname(rstudioapi::getSourceEditorContext()$path),'/2022-student-research-case-study-economic-data.xlsx',sep="")

playerdata <- paste(dirname(rstudioapi::getSourceEditorContext()$path),'/2022-student-research-case-study-player-data.xlsx',sep="")

footballdata <- paste(dirname(rstudioapi::getSourceEditorContext()$path),'/2022-student-research-case-study-football-soccer-data.xlsx',sep="")
#------------------------------------
### -------- READING IN DATA -------- ###
  #Reading in economic data
gdp <- read_excel(economicdata, sheet = 'Rarita Economic', range = 'B12:F22')
gni <- read_excel(economicdata, sheet = 'Rarita Economic', range = 'H12:L22')
pop <- read_excel(economicdata, sheet = 'Rarita Economic', range = 'B26:F36')
popden <- read_excel(economicdata, sheet = 'Rarita Economic', range = 'H26:L36')
health <- read_excel(economicdata, sheet = 'Rarita Economic', range = 'B40:F50')
housesaving <- read_excel(economicdata, sheet = 'Rarita Economic', range = 'H40:L50')
conversionrate <- read_excel(economicdata, sheet = 'Rarita Economic', range = 'B54:D59')
spotrate <- read_excel(economicdata, sheet = 'Rarita Spot Rates', range = 'B12:P72')
inflationrate <- read_excel(economicdata, sheet = 'Rarita Inflation Rates', range = 'B11:C41')
globalgdp <- read_excel(economicdata, sheet = 'Other Countries GDP', range = 'B12:G32')

  #Reading in player data
lshooting <- read_excel(playerdata, sheet = 'League Shooting', range = 'B12:AA5566')
lpassing <- read_excel(playerdata, sheet = 'League Passing', range = 'B12:AF5566')
ldefense <- read_excel(playerdata, sheet = 'League Defense', range = 'B12:AG5566')
lgoal <- read_excel(playerdata, sheet = 'League Goalkeeping', range = 'B12:AB425')
rank2020 <- read_excel(playerdata, sheet = 'Tournament Results', range = 'B11:C27')
rank2021 <- read_excel(playerdata, sheet = 'Tournament Results', range = 'E11:F35')
tshooting <- read_excel(playerdata, sheet = 'Tournament Shooting', range = 'B12:Z2027', guess_max = 3000)
tpassing <- read_excel(playerdata, sheet = 'Tournament Passing', range = 'B12:AE500')
tdefense <- read_excel(playerdata, sheet = 'Tournament Defense', range = 'B12:AF500')
tgoal <- read_excel(playerdata, sheet = 'Tournament Goalkeeping', range = 'B12:AA141')
salary2020 <- read_excel(playerdata, sheet = '2020 Salaries', range = 'B12:G2744')
salary2021 <- read_excel(playerdata, sheet = '2021 Salaries', range = 'B12:G2834')


  
  #Reading in Football Revenue
revenue <- read_excel(footballdata, sheet = 'Revenue', range = 'B13:V34', col_names = FALSE)
a<-c()
for (i in 2:5){
  for (j in 0:4){
    b<-i+j*4
    a<-c(a, b)
  }
}
revenue<-revenue[,c(1,as.double(a))]
colnames(revenue)<-c("",rep(c('2020','2019','2018','2017','2016'), 4))
names(revenue)<-paste(names(revenue), revenue[1,], sep="_")
revenue[-1,]
revenue[]<-lapply(revenue, as.character)
revenue<-revenue[-1,]
rev<-revenue[-6,]

  #Reading in Football Expenses
expenses <- read_excel(footballdata, sheet = 'Expense', range = 'B13:Q34', col_names = FALSE)
d<-c()
for (i in 2:4){
  for (j in 0:4){
    e<-i+j*3
    d<-c(d, e)
  }
}
expenses<-expenses[,c(1,as.double(d))]
colnames(expenses)<-c("",rep(c('2020','2019','2018','2017','2016'),3))
names(expenses)<-paste(names(expenses), expenses[1,], sep="_")
expenses[-1,]
expenses[]<-lapply(expenses, as.character)
expenses<-expenses[-1,]
  #Reading in Football Media
attendance <- read_excel(footballdata, sheet = 'Attendance', range = 'B13:C34')
media <- read_excel(footballdata, sheet = 'Social Media', range = 'B13:G34')

#---------------------------------------------
### -------- CLEANING DATA -------- ###
  #Replacing NA in 'Social Media' data
media <- media %>%
  mutate(Tiktok = as.numeric(Tiktok)) %>%
  replace(is.na(.),0)


  #Removing 'Eastern Sleboube' from revenue and expenses due to missing expense data
remove<-data.frame(which(is.na(expenses), arr.ind = T))
remove1<-c(remove$row)
remove1<-unique(remove1)
revenue<-revenue[-remove1,]
expenses<-expenses[-remove1,]


  #Taking absolute value of all variables
temp <- lgoal[c("Player","Nation","Pos","Squad","League")]
lgoal <- abs(lgoal[,!names(lgoal) %in% c("Player","Nation","Pos","Squad","League")])
lgoal <- data.frame(temp,lgoal)

temp1 <- tgoal[c("Player","Nation","Pos","League")]
tgoal <- abs(tgoal[,!names(tgoal) %in% c("Player","Nation","Pos","League")])
tgoal <- data.frame(temp1,tgoal)

temp2 <- lpassing[c("Player","Nation","Pos","Squad","League")]
lpassing <- abs(lpassing[,!names(lpassing) %in% c("Player","Nation","Pos","Squad","League")])
lpassing <- data.frame(temp2,lpassing)

temp3 <- tpassing[c("Player","Nation","Pos","League")]
tpassing <- abs(tpassing[,!names(tpassing) %in% c("Player","Nation","Pos","League")])
tpassing <- data.frame(temp3,tpassing)

temp5 <- ldefense[c("Player","Nation","Pos","Squad","League")]
ldefense <- abs(ldefense[,!names(ldefense) %in% c("Player","Nation","Pos","Squad","League")])
ldefense <- data.frame(temp5,ldefense)

temp6 <- tdefense[c("Player","Nation","Pos","League")]
tdefense <- abs(tdefense[,!names(tdefense) %in% c("Player","Nation","Pos","League")])
tdefense <- data.frame(temp6,tdefense)

temp_lshooting = lshooting[c("Player", "Nation", "Pos", "Squad", "League")]
lshooting = abs(lshooting[,!names(lshooting) %in% c("Player", "Nation", "Pos", "Squad", "League")])
lshooting = data.frame(temp_lshooting, lshooting)

temp_tshooting = tshooting[c("Player", "Nation", "Pos", "League")]
tshooting = abs(tshooting[,!names(tshooting) %in% c("Player", "Nation", "Pos", "League")])
tshooting = data.frame(temp_tshooting, tshooting)

  #Removing certain columns from shooting data due to excess amounts of missing values
remove = c("Standard.Dist", "Standard.FK", "Expected.xG", "Expected.npxG", "Expected.npxG.Sh", "Expected.G.xG", "Expected.np.G.xG")
lshooting = lshooting[, (!names(lshooting) %in% remove)]
tshooting = tshooting[, (!names(tshooting) %in% remove)]

  #Recalculating 'Performance Save%' and 'Performance CS%' for goalkeeping data
tgoal <- tgoal %>%
  mutate(Performance.Save.=Performance.Saves/Performance.SoTA,Performance.CS.=Performance.CS/(`W`+`D`+`L`))
lgoal <- lgoal %>%
  mutate(Performance.Save.=Performance.Saves/Performance.SoTA,Performance.CS.=Performance.CS/(`W`+`D`+`L`))
    #Removing outlier entries (considered to be those entries >1.05, no longer due to rounding errors)
tgoal <- tgoal %>%
  filter(Performance.Save.<1.05 & Performance.CS.<1.05)
lgoal <- lgoal %>%
  filter(Performance.Save.<1.05 & Performance.CS.<1.05)

  #Recalculating 'Total Cmp%', 'Short Cmp%', 'Medium Cmp%','Long Cmp%' and 'A-xA' for passing data
tpassing <- tpassing %>%
  mutate(Total.Cmp.=Total.Cmp/Total.Att,Short.Cmp.=Short.Cmp/Short.Att,
         Medium.Cmp.=Medium.Cmp/Medium.Att,Long.Cmp.=Long.Cmp/Long.Att,A.xA=Ast-xA)
lpassing <- lpassing %>%
  mutate(Total.Cmp.=Total.Cmp/Total.Att,Short.Cmp.=Short.Cmp/Short.Att,
         Medium.Cmp.=Medium.Cmp/Medium.Att,Long.Cmp.=Long.Cmp/Long.Att,A.xA=Ast-xA)
    #Removing outlier entries (considered to be those entries >1.05, no longer due to rounding errors)
tpassing <- tpassing %>%
  filter(Total.Cmp.<1.05 & Short.Cmp.<1.05 & Medium.Cmp.<1.05 & Long.Cmp.<1.05)
lpassing <- lpassing %>%
  filter(Total.Cmp.<1.05 & Short.Cmp.<1.05 & Medium.Cmp.<1.05 & Long.Cmp.<1.05)

  #Recalculating Vs Dribbles Tkl% and Pressures %
ldefense <- ldefense %>%
  mutate(Vs.Dribbles.Tkl.=Vs.Dribbles.Tkl/Vs.Dribbles.Att,Pressures..=Pressures.Succ/Pressures.Press)
tdefense <- tdefense %>%
  mutate(Vs.Dribbles.Tkl.=Vs.Dribbles.Tkl/Vs.Dribbles.Att,Pressures..=Pressures.Succ/Pressures.Press)

tdefense <- tdefense %>%
  filter(Vs.Dribbles.Tkl.<1.05 & Pressures..<1.05)
ldefense <- ldefense %>%
  filter(Vs.Dribbles.Tkl.<1.05 & Pressures..<1.05)

  #Shooting data changes
  #Remove outliers for % variables (considered to be those entries >1.05, no longer due to rounding errors)
lshooting <- lshooting %>%
  filter(Standard.SoT.<100.05)
tshooting <- tshooting %>%
  filter(Standard.SoT.<100.05)

  #Remove NA values
lshooting = lshooting %>%
  drop_na(Standard.SoT., Standard.G.Sh, Standard.G.SoT)

tshooting = tshooting %>%
  drop_na(Standard.SoT., Standard.G.Sh, Standard.G.SoT)

#-------------------------------------
  #Creating duplicate data sets for later use
tgoal1<-tgoal  #i stored the tgoal and lgoal as another variable before adding columns to make the combined data, keep the calculations incase they might useful
lgoal1<-lgoal[,-4]

tpassing1 <- tpassing
lpassing1 <- lpassing[,-5] #had to remove League column in league data and rename existing Squad column to league for later merge
lpassing1 <- lpassing1 %>%
  rename('League'='Squad')

tdefense1 <- tdefense
ldefense1 <- ldefense[,-5] #had to remove League column in league data and rename existing Squad column to league for later merge
ldefense1 <- ldefense1 %>%
  rename('League'='Squad')

tshooting1 <- tshooting
lshooting1 <- lshooting[,-5]
lshooting1 <- lshooting1 %>%
  rename('League'='Squad')

  #Adding tournament place column to tournament data
    #Goalkeeping
tgoal[ ,'Tournament Place']<-NA
tgoal2020<- tgoal %>% filter(Year==2020)
tgoal2021<- tgoal %>% filter(Year==2021)

tgoal2020$`Tournament Place` = rank2020$`2020 Tournament Place`[match(tgoal2020$Nation, rank2020$Country)]
tgoal2021$`Tournament Place` = rank2021$`2021 Tournament Place`[match(tgoal2021$Nation, rank2021$Country)]

tgoalkek <-rbind(tgoal2020, tgoal2021)
tgoalkek$`Tournament Place`[is.na(tgoalkek$`Tournament Place`)]<-25

    #Passing
tpassing[ ,'Tournament Place'] <- NA
tpassing2020 <- tpassing %>% filter(Year==2020)
tpassing2021 <- tpassing %>% filter(Year==2021)

tpassing2020$`Tournament Place` = rank2020$`2020 Tournament Place`[match(tpassing2020$Nation, rank2020$Country)]
tpassing2021$`Tournament Place` = rank2021$`2021 Tournament Place`[match(tpassing2021$Nation, rank2021$Country)]

tpassingcor <- rbind(tpassing2020, tpassing2021)
tpassingcor$`Tournament Place`[is.na(tpassingcor$`Tournament Place`)]<-25

    #Defense
tdefense[ ,'Tournament Place'] <- NA
tdefense2020 <- tdefense %>% filter(Year==2020)
tdefense2021 <- tdefense %>% filter(Year==2021)

tdefense2020$`Tournament Place` = rank2020$`2020 Tournament Place`[match(tdefense2020$Nation, rank2020$Country)]
tdefense2021$`Tournament Place` = rank2021$`2021 Tournament Place`[match(tdefense2021$Nation, rank2021$Country)]

tdefensecor <- rbind(tdefense2020, tdefense2021)
tdefensecor$`Tournament Place`[is.na(tdefensecor$`Tournament Place`)]<-25

    #Shooting
tshooting[ ,'Tournament Place'] <- NA
tshooting2020 <- tshooting %>% filter(Year==2020)
tshooting2021 <- tshooting %>% filter(Year==2021)

tshooting2020$`Tournament Place` = rank2020$`2020 Tournament Place`[match(tshooting2020$Nation, rank2020$Country)]
tshooting2021$`Tournament Place` = rank2021$`2021 Tournament Place`[match(tshooting2021$Nation, rank2021$Country)]

tshootingcor <- rbind(tshooting2020, tshooting2021)
tshootingcor$`Tournament Place`[is.na(tshootingcor$`Tournament Place`)]<-25

  #Correlation
correlation<-cor(tgoalkek[,unlist(lapply(tgoalkek, is.numeric))])
ggcorrplot(correlation[,23:1])

correlation1 <- cor(tpassingcor[,unlist(lapply(tpassingcor, is.numeric))])
ggcorrplot(correlation1[,27:1])

correlation2 <- cor(tdefensecor[,unlist(lapply(tdefensecor, is.numeric))])
ggcorrplot(correlation2[,28:1])


  #Combining tournament and league data
cgoal<-rbind(lgoal1, tgoal1)

cpass <- rbind(lpassing1, tpassing1)

cdefense <- rbind(ldefense1,tdefense1)

cshooting1 <- rbind(lshooting1,tshooting1)

  #Merging passing and defending data sets
cpassdef <- merge(cpass,cdefense,by=c("Player","Nation","Pos","League","Age"))
cpassdef <- cpassdef %>%
  rename('X90s'='X90s.x','Year'='Year.x') %>%
  select(-c('X90s.y')) 
cpassdef <- cpassdef %>% mutate(`Tackle+Pressure%` = `Vs.Dribbles.Tkl.` + `Pressures..`)

  select(-c('X90s.y')) %>%
  mutate(`Tackle+Pressure%` = `Vs.Dribbles.Tkl.` + `Pressures..`)


  #Merging passing, defending and shooting
cstats <- merge(cpassdef,cshooting1,by=c("Player","Nation","Pos","League","Age"))
cstats <- cstats %>%
  select(-c('Year.y')) %>%
  rename('X90s'='X90s.x','Year'='Year.x') %>%
  mutate(`Standard.SoT.`=`Standard.SoT.`/100)

cmf <- cstats %>%
  filter(Pos %in% c('DFMF','MFDF','MF','FWMF','MFFW'))

cdef <- cpassdef %>%
  filter(Pos %in% c('DF','DFMF','MFDF','DFFW','FWDF'))

  #Filtering columns for modeling
cgoalmodel <- cgoal %>%
  select(Age,Playing.Time.Min,Performance.GA,Performance.SoTA,Performance.Saves,Performance.Save.,W,D,L,
         Performance.CS.,Penalty.Kicks.PKA,Penalty.Kicks.PKsv,Penalty.Kicks.PKm)

cmfmodel <- cmf %>%
  select(Age,X90s,Total.Cmp.,Total.TotDist,Total.PrgDist,Short.Cmp.,Medium.Cmp.,Long.Cmp.,Ast,A.xA,KP,X1.3,PPA,
         CrsPA,Prog,Tackles.TklW,Tackles.Def.3rd,Tackles.Mid.3rd,Tackles.Att.3rd,Vs.Dribbles.Tkl.,Pressures..,
         Blocks.Blocks,Blocks.Pass,Int,Clr, `Tackle+Pressure%`,Standard.SoT.,Standard.G.Sh,Standard.G.SoT,Performance.PK,Performance.PKatt)


#Making a training set and testing set
set.seed(0)

  #Making goalkeeping training/testing set on 2020/2021
train <- which(cgoal$Year == 2020)
test <- which(cgoal$Nation != 'Rarita' & cgoal$Year == 2021)
testset<-cgoalmodel[test,]
trainset<-cgoalmodel[train,]
Rarita <- which(cgoal$Nation == 'Rarita' & cgoal$Year == 2021)
Raritaset<-cgoalmodel[Rarita,]

train2<-sample(nrow(cgoal), nrow(cgoal)*0.7)


  #Making midfield training/testing set on 2020/2021
set.seed(0)
train1 <- which(cmf$Year == 2020)
test1 <- which(cmf$Nation != 'Rarita' & cmf$Year == 2021)
testset1 <- cmfmodel[test1,]
trainset1 <- cmfmodel[train1,]
Rarita1 <- which(cmf$Nation == 'Rarita' & cmf$Year == 2021)
Raritaset1 <- cmfmodel[Rarita1,]


mf.p.trainset <- trainset1 %>% select(Age,X90s,Total.Cmp.,Total.TotDist,Total.PrgDist,Short.Cmp.,Medium.Cmp.,Long.Cmp.,Ast,A.xA,KP,X1.3,PPA,
                                      CrsPA,Prog)
mf.p.testset <- testset1 %>% select(Age,X90s,Total.Cmp.,Total.TotDist,Total.PrgDist,Short.Cmp.,Medium.Cmp.,Long.Cmp.,Ast,A.xA,KP,X1.3,PPA,
                                      CrsPA,Prog)
mf.p.Raritaset <- Raritaset1 %>% select(Age,X90s,Total.Cmp.,Total.TotDist,Total.PrgDist,Short.Cmp.,Medium.Cmp.,Long.Cmp.,Ast,A.xA,KP,X1.3,PPA,
                                      CrsPA,Prog)
mf.d.trainset <- trainset1 %>% select(Age,X90s,Tackles.TklW,Tackles.Def.3rd,Tackles.Mid.3rd,Tackles.Att.3rd,Vs.Dribbles.Tkl.,Pressures..,
                                      Blocks.Blocks,Blocks.Pass,Int,Clr, `Tackle+Pressure%`)
mf.d.testset <- testset1 %>% select(Age,X90s,Tackles.TklW,Tackles.Def.3rd,Tackles.Mid.3rd,Tackles.Att.3rd,Vs.Dribbles.Tkl.,Pressures..,
                                      Blocks.Blocks,Blocks.Pass,Int,Clr, `Tackle+Pressure%`)
mf.d.Raritaset <- Raritaset1 %>% select(Age,X90s,Tackles.TklW,Tackles.Def.3rd,Tackles.Mid.3rd,Tackles.Att.3rd,Vs.Dribbles.Tkl.,Pressures..,
                                      Blocks.Blocks,Blocks.Pass,Int,Clr, `Tackle+Pressure%`)
mf.s.trainset <- trainset1 %>% select(Age,X90s,Standard.SoT.,Standard.G.Sh,Standard.G.SoT,Performance.PK,Performance.PKatt)
mf.s.testset <- testset1 %>% select(Age,X90s,Standard.SoT.,Standard.G.Sh,Standard.G.SoT,Performance.PK,Performance.PKatt)
mf.s.Raritaset <- Raritaset1 %>% select(Age,X90s,Standard.SoT.,Standard.G.Sh,Standard.G.SoT,Performance.PK,Performance.PKatt)

#---------Goalkeepers----------------
### ------ Goalkeeping Models ------ ###
set.seed(0)
  #Regression
regressiongoal <- lm(Performance.Save. ~., data = trainset)
summary(regressiongoal)
    #Regression MSE calculation
regpredictgoal <- predict(regressiongoal, testset)
actualgoal <- testset$Performance.Save.
MSEreg<-mean((actualgoal-regpredictgoal)^2)


  #Lasso Regression
grid <- 10^seq(10, -5, length = 100)
lassodata <- model.matrix(Performance.Save. ~ . , trainset)[,-6]
lassogoal <- glmnet(lassodata, trainset$Performance.Save., alpha = 1, lambda = grid)
plot(lassogoal, xvar = "lambda")
    #Determining optimal lambda value for ridge regression
cvlasso <- cv.glmnet(lassodata, trainset$Performance.Save., alpha = 1)
plot(cvlasso, xvar = "lambda")
bestlam <- cvlasso$lambda.min
    #Lasso regression MSE calculation
lassotestdata <- model.matrix(Performance.Save. ~ ., testset)[,-6]
lassopredictgoal<-predict(cvlasso, newx = lassotestdata)
MSElasso <- mean((actualgoal-lassopredictgoal)^2)


  #Regression Tree
treegoal <- rpart(Performance.Save. ~ ., trainset, cp = 0.001, method = "anova")
rpart.plot(treegoal, box.palette = "GnRd", yesno = 2, split.border.col = 1, split.round = 1)
plotcp(treegoal)
    #Regression tree MSE calculation
treepredictgoal<-predict(treegoal, testset)
MSEtree <- mean((actualgoal-treepredictgoal)^2)


  #Random Forest
rfgoal <- randomForest(Performance.Save. ~ ., data = trainset, mtry = sqrt(ncol(trainset)), importance = TRUE)
    #Variable importance for random forest model
vip(rfgoal, num_features = ncol(trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(trainset) - 1)))+
  theme_classic()
    #Random forest MSE calculation
rfpredictgoal<-predict(rfgoal, testset)
MSErf<-mean((actualgoal-rfpredictgoal)^2)


  #Bagging
baggoal <- randomForest(Performance.Save. ~ ., data = trainset, mtry = (ncol(trainset)-1),importance = TRUE)
    #Variable importance for bagging
vip(baggoal, num_features = ncol(trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(trainset) - 1)))+
  theme_classic()
    #Bagging MSE calculation
bagpredictgoal <- predict(baggoal, testset)
MSEbag<-mean((actualgoal-bagpredictgoal)^2)


  #Boosting
boostgoal <- gbm(Performance.Save. ~ ., data = trainset, 
                  n.trees = 1000, interaction.depth = 5, cv.folds = 10)
    #Variable importance for boosting
vip(boostgoal, num_features = ncol(trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(trainset) - 1)))+
  theme_classic()
    #Boosting MSE calculation
bestboost <- gbm.perf(boostgoal, method = "cv")
boostpredictgoal <- predict(boostgoal, newdata = testset, n.trees = bestboost)
MSEboost <- mean((actualgoal-boostpredictgoal)^2)

  #Best model
GKmse <- data.frame(MSEreg,MSElasso,MSEtree,MSErf,MSEbag,MSEboost)
    #Running boosting model on 2021 Raritan goalkeepers
raritangk <- cgoal[Rarita,]$Player
gkpredict <- predict(boostgoal, Raritaset, n.trees = bestboost)
gklist <- data.frame(raritangk,gkpredict)
colnames(gklist) <- c("Player","Score")
gklist <- arrange(gklist, -Score)


#----------Midfielders----------------
### ------ Midfielder Models ------ ###
#---Passing stats for Midfielders
set.seed(0)
  #Regression
regressionmf.p <- lm(Total.Cmp. ~ Age + X90s + Total.TotDist + Total.PrgDist + Short.Cmp.+ 
                     Medium.Cmp.+ Long.Cmp.+ Ast + A.xA + KP + X1.3 + PPA + CrsPA + Prog, data = mf.p.trainset)
                   
summary(regressionmf.p)
    #Regression MSE calculation
regpredictmf.p <- predict(regressionmf.p, mf.p.testset)
actualmf.p <- mf.p.testset$Total.Cmp.
MSEreg.p<-mean((actualmf.p-regpredictmf.p)^2)


  #Lasso Regression
grid <- 10^seq(10, -5, length = 100)
lassodata.p <- model.matrix(Total.Cmp. ~ Age + X90s + Total.TotDist + Total.PrgDist + Short.Cmp.+ 
                            Medium.Cmp.+ Long.Cmp.+ Ast + A.xA + KP + X1.3 + PPA + CrsPA + Prog, mf.p.trainset)[,-3] #-3 removes the Total.Cmp. column, change if different variable is used
lassomf.p <- glmnet(lassodata.p, mf.p.trainset$Total.Cmp., alpha = 1, lambda = grid)
plot(lassomf.p, xvar = "lambda")
    #Determining optimal lambda value for ridge regression
cvlasso.p <- cv.glmnet(lassodata.p, mf.p.trainset$Total.Cmp., alpha = 1)
plot(cvlasso.p, xvar = "lambda")
bestlam.p <- cvlasso.p$lambda.min
    #Lasso regression MSE calculation
lassotestdata.p <- model.matrix(Total.Cmp. ~ Age + X90s + Total.TotDist + Total.PrgDist + Short.Cmp.+ 
                                Medium.Cmp.+ Long.Cmp.+ Ast + A.xA + KP + X1.3 + PPA + CrsPA + Prog, mf.p.testset)[,-3]
lassopredictmf.p<-predict(cvlasso.p, newx = lassotestdata.p)
MSElasso.p <- mean((actualmf.p-lassopredictmf.p)^2)


  #Regression Tree
treemf.p <- rpart(Total.Cmp. ~ Age + X90s + Total.TotDist + Total.PrgDist + Short.Cmp.+ 
                  Medium.Cmp.+ Long.Cmp.+ Ast + A.xA + KP + X1.3 + PPA + CrsPA + Prog, mf.p.trainset, cp = 0.001, method = "anova")
rpart.plot(treemf.p, box.palette = "GnRd", yesno = 2, split.border.col = 1, split.round = 1)
plotcp(treemf.p)
    #Regression tree MSE calculation
treepredictmf.p<-predict(treemf.p, mf.p.testset)
MSEtree.p <- mean((actualmf.p-treepredictmf.p)^2)


  #Random Forest
rfmf.p <- randomForest(Total.Cmp. ~ Age + X90s + Total.TotDist + Total.PrgDist + Short.Cmp.+ 
                       Medium.Cmp.+ Long.Cmp.+ Ast + A.xA + KP + X1.3 + PPA + CrsPA + Prog, data = mf.p.trainset,
                       mtry = sqrt(ncol(mf.p.trainset)), importance = TRUE)
    #Variable importance for random forest model
vip(rfmf.p, num_features = ncol(mf.p.trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(mf.p.trainset) - 1)))+
  theme_classic()
    #Random forest MSE calculation
rfpredictmf.p<-predict(rfmf.p, mf.p.testset)
MSErf.p<-mean((actualmf.p-rfpredictmf.p)^2)


  #Bagging
bagmf.p <- randomForest(Total.Cmp. ~ Age + X90s + Total.TotDist + Total.PrgDist + Short.Cmp.+ 
                        Medium.Cmp.+ Long.Cmp.+ Ast + A.xA + KP + X1.3 + PPA + CrsPA + Prog, data = mf.p.trainset, 
                        mtry = (ncol(mf.p.trainset)-1),importance = TRUE)
    #Variable importance for bagging
vip(bagmf.p, num_features = ncol(mf.p.trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(mf.p.trainset) - 1)))+
  theme_classic()
    #Bagging MSE calculation
bagpredictmf.p <- predict(bagmf.p, mf.p.testset)
MSEbag.p<-mean((actualmf.p-bagpredictmf.p)^2)


  #Boosting
boostmf.p <- gbm(Total.Cmp. ~ Age + X90s + Total.TotDist + Total.PrgDist + Short.Cmp.+ 
                 Medium.Cmp.+ Long.Cmp.+ Ast + A.xA + KP + X1.3 + PPA + CrsPA + Prog, data = mf.p.trainset, 
                 n.trees = 1000, interaction.depth = 5, cv.folds = 10)
    #Variable importance for boosting
vip(boostmf.p, num_features = ncol(mf.p.trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(mf.p.trainset) - 1)))+
  theme_classic()
    #Boosting MSE calculation
bestboost.p <- gbm.perf(boostmf.p, method = "cv")
boostpredictmf.p <- predict(boostmf.p, newdata = mf.p.testset, n.trees = bestboost.p)
MSEboost.p <- mean((actualmf.p-boostpredictmf.p)^2)

  #Best model
MFPmse <- data.frame(MSEreg.p,MSElasso.p,MSEtree.p,MSErf.p,MSEbag.p,MSEboost.p)
    #Running boosting model on 2021 Raritan Midfielders (Passing)
raritanmf.p <- cmf[Rarita1,]$Player
mfpredict.p <- predict(boostmf.p, mf.p.Raritaset, n.trees = bestboost.p)
mflist.p <- data.frame(raritanmf.p,mfpredict.p)
colnames(mflist.p) <- c("Player","Passing_Score")

mflist.p <- mflist.p %>% mutate(`Passing_Points` = rank(mflist.p[,2]))
mflist.p <- mflist.p %>% arrange(-Passing_Points) %>%
  mutate(Passing_Points = Passing_Points*0.4)

mflist.p <- mflist.p %>% mutate(Passing_Points = rank(mflist.p[,2]))
mflist.p <- mflist.p %>% arrange(-Passing_Points) %>%
  mutate(Passing_Points = Passing_Points*0.4)




#---Defensive Stats for Midfielders
set.seed(0)
regressionmf.d <- lm(`Tackle+Pressure%` ~ Age + X90s + Tackles.TklW + Tackles.Def.3rd + Tackles.Mid.3rd + 
                        Tackles.Att.3rd + Blocks.Blocks + Blocks.Pass + Int + Clr, data = mf.d.trainset)

summary(regressionmf.d)
#Regression MSE calculation
regpredictmf.d <- predict(regressionmf.d, mf.d.testset)
actualmf.d <- mf.d.testset$`Tackle+Pressure%`
MSEreg.d<-mean((actualmf.d-regpredictmf.d)^2)


#Lasso Regression
grid <- 10^seq(10, -5, length = 100)
lassodata.d <- model.matrix(`Tackle+Pressure%` ~ Age + X90s + Tackles.TklW + Tackles.Def.3rd + Tackles.Mid.3rd + 
                            Tackles.Att.3rd + Blocks.Blocks + Blocks.Pass + Int + Clr, data = mf.d.trainset)[,-13] #-3 removes the Tackle+Pressure% column, change if different variable is used
lassomf.d <- glmnet(lassodata.d, mf.d.trainset$`Tackle+Pressure%`, alpha = 1, lambda = grid)
plot(lassomf.d, xvar = "lambda")
#Determining optimal lambda value for ridge regression
cvlasso.d <- cv.glmnet(lassodata.d, mf.d.trainset$`Tackle+Pressure%`, alpha = 1)
plot(cvlasso.d, xvar = "lambda")
bestlam.d <- cvlasso.d$lambda.min
#Lasso regression MSE calculation
lassotestdata.d <- model.matrix(`Tackle+Pressure%` ~ Age + X90s + Tackles.TklW + Tackles.Def.3rd + Tackles.Mid.3rd + 
                                Tackles.Att.3rd + Blocks.Blocks + Blocks.Pass + Int + Clr, mf.d.testset)[,-13]
lassopredictmf.d<-predict(cvlasso.d, newx = lassotestdata.d)
MSElasso.d <- mean((actualmf.d-lassopredictmf.d)^2)


#Regression Tree
treemf.d <- rpart(`Tackle+Pressure%` ~ Age + X90s + Tackles.TklW + Tackles.Def.3rd + Tackles.Mid.3rd + 
                  Tackles.Att.3rd + Blocks.Blocks + Blocks.Pass + Int + Clr, mf.d.trainset, cp = 0.001, method = "anova")
rpart.plot(treemf.d, box.palette = "GnRd", yesno = 2, split.border.col = 1, split.round = 1)
plotcp(treemf.d)
#Regression tree MSE calculation
treepredictmf.d<-predict(treemf.d, mf.d.testset)
MSEtree.d <- mean((actualmf.d-treepredictmf.d)^2)


#Random Forest
rfmf.d <- randomForest(`Tackle+Pressure%` ~ Age + X90s + Tackles.TklW + Tackles.Def.3rd + Tackles.Mid.3rd + 
                       Tackles.Att.3rd + Blocks.Blocks + Blocks.Pass + Int + Clr, data = mf.d.trainset, 
                       mtry = sqrt(ncol(mf.d.trainset)), importance = TRUE)
#Variable importance for random forest model
vip(rfmf.d, num_features = (ncol(mf.d.trainset))-1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))((ncol(mf.d.trainset))-1)))+
  theme_classic()
#Random forest MSE calculation
rfpredictmf.d<-predict(rfmf.d, mf.d.testset)
MSErf.d<-mean((actualmf.d-rfpredictmf.d)^2)


#Bagging
bagmf.d <- randomForest(`Tackle+Pressure%` ~., data = mf.d.trainset, mtry = (ncol(mf.d.trainset)-1),importance = TRUE)
                       
#Variable importance for bagging
vip(bagmf.d, num_features = ncol(mf.d.trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(mf.d.trainset) - 1)))+
  theme_classic()
#Bagging MSE calculation
bagpredictmf.d <- predict(bagmf.d, mf.d.testset)
MSEbag.d<-mean((actualmf.d-bagpredictmf.d)^2)


#Boosting
boostmf.d <- gbm(`Tackle+Pressure%` ~ ., data = mf.d.trainset, 
               n.trees = 1000, interaction.depth = 5, cv.folds = 10)
#Variable importance for boosting
vip(boostmf.d, num_features = ncol(mf.d.trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(mf.d.trainset) - 1)))+
  theme_classic()
#Boosting MSE calculation
bestboost.d <- gbm.perf(boostmf.d, method = "cv")
boostpredictmf.d <- predict(boostmf.d, newdata = mf.d.testset, n.trees = bestboost.d)
MSEboost.d <- mean((actualmf.d-boostpredictmf.d)^2)



  #Best model
MFDmse <- data.frame(MSEreg.d,MSElasso.d,MSEtree.d,MSErf.d,MSEbag.d,MSEboost.d)
    #Running boosting model on 2021 Raritan Midfielders (Defense)
raritanmf.d <- cmf[Rarita1,]$Player
mfpredict.d <- predict(boostmf.d, mf.d.Raritaset, n.trees = bestboost.d)
mflist.d <- data.frame(raritanmf.d,mfpredict.d)
colnames(mflist.d) <- c("Player","Defense_Score")

mflist.d <- mflist.d %>% mutate(`Defense_Points` = rank(mflist.d[,2]))
mflist.d <- mflist.d %>% arrange(-Defense_Points) %>% 
  mutate(Defense_Points = Defense_Points*0.3)

mflist.d <- mflist.d %>% mutate(Defense_Points = rank(mflist.d[,2]))
mflist.d <- mflist.d %>% arrange(-Defense_Points) %>% 
  mutate(Defense_Points = Defense_Points*0.3)



#---Shooting Stats for Midfielders
set.seed(0)
regressionmf.s <- lm(Standard.G.SoT ~ ., data = mf.s.trainset)
summary(regressionmf.s)
#Regression MSE calculation
regpredictmf.s <- predict(regressionmf.d, mf.d.testset)
actualmf.s <- mf.s.testset$Standard.G.SoT
MSEreg.s<-mean((actualmf.s-regpredictmf.s)^2)



#Lasso Regression
grid <- 10^seq(10, -5, length = 100)
lassodata.s <- model.matrix(Standard.G.SoT ~ ., data = mf.s.trainset)[,-5]
lassomf.s <- glmnet(lassodata.s, mf.s.trainset$Standard.G.SoT, alpha = 1, lambda = grid)
plot(lassomf.s, xvar = "lambda")
#Determining optimal lambda value for ridge regression
cvlasso.s <- cv.glmnet(lassodata.s, mf.s.trainset$Standard.G.SoT, alpha = 1)
plot(cvlasso.s, xvar = "lambda")
bestlam.s <- cvlasso.s$lambda.min
#Lasso regression MSE calculation
lassotestdata.s <- model.matrix(Standard.G.SoT ~ ., mf.s.testset)[,-5]
lassopredictmf.s<-predict(cvlasso.s, newx = lassotestdata.s)
MSElasso.s <- mean((actualmf.s-lassopredictmf.s)^2)


#Regression Tree
treemf.s <- rpart(Standard.G.SoT ~ ., mf.s.trainset, cp = 0.001, method = "anova")
rpart.plot(treemf.s, box.palette = "GnRd", yesno = 2, split.border.col = 1, split.round = 1)
plotcp(treemf.s)
#Regression tree MSE calculation
treepredictmf.s<-predict(treemf.s, mf.s.testset)
MSEtree.s <- mean((actualmf.s-treepredictmf.s)^2)



midfielderstats <- data.frame(mflist.p, mflist.d)
midfielderstats <- midfielderstats %>% mutate(Total_Points=Passing_Points + Defense_Points)

#Random Forest
rfmf.s <- randomForest(Standard.G.SoT ~ ., data = mf.s.trainset,
                       mtry = sqrt(ncol(mf.s.trainset)), importance = TRUE)
#Variable importance for random forest model
vip(rfmf.s, num_features = (ncol(mf.s.trainset))-1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))((ncol(mf.s.trainset))-1)))+
  theme_classic()
#Random forest MSE calculation
rfpredictmf.s<-predict(rfmf.s, mf.s.testset)
MSErf.s<-mean((actualmf.s-rfpredictmf.s)^2)


#Bagging
bagmf.s <- randomForest(Standard.G.SoT ~., data = mf.s.trainset, mtry = (ncol(mf.s.trainset)-1),importance = TRUE)

#Variable importance for bagging
vip(bagmf.s, num_features = ncol(mf.s.trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(mf.s.trainset) - 1)))+
  theme_classic()
#Bagging MSE calculation
bagpredictmf.s <- predict(bagmf.s, mf.s.testset)
MSEbag.s<-mean((actualmf.s-bagpredictmf.s)^2)


#Boosting
boostmf.s <- gbm(Standard.G.SoT ~ ., data = mf.s.trainset, 
                 n.trees = 1000, interaction.depth = 5, cv.folds = 10)
#Variable importance for boosting
vip(boostmf.s, num_features = ncol(mf.s.trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(mf.s.trainset) - 1)))+
  theme_classic()
#Boosting MSE calculation
bestboost.s <- gbm.perf(boostmf.s, method = "cv")
boostpredictmf.s <- predict(boostmf.s, newdata = mf.s.testset, n.trees = bestboost.s)
MSEboost.s <- mean((actualmf.s-boostpredictmf.s)^2)


  #Best model
MFSmse <- data.frame(MSEreg.s,MSElasso.s,MSEtree.s,MSErf.s,MSEbag.s,MSEboost.s)
    #Running boosting model on 2021 Raritan Midfielders (Shooting)
raritanmf.s <- cmf[Rarita1,]$Player
mfpredict.s <- predict(boostmf.s, mf.s.Raritaset, n.trees = bestboost.s)
mflist.s <- data.frame(raritanmf.s,mfpredict.s)
colnames(mflist.s) <- c("Player","Shooting_Score")
mflist.s <- mflist.s %>% mutate(Shooting_Points = rank(mflist.s[,2]))
mflist.s <- mflist.s %>% arrange(-Shooting_Points) %>% 
  mutate(Shooting_Points = Shooting_Points*0.3)


#COMBINING THE SCORES FROM ALL 3 CATEGORIES FOR Midfielders
midfielderstats <- merge(mflist.p, mflist.d,by=c('Player'))
midfielderstats <- midfielderstats %>% 
  mutate(Total_Points=Passing_Points + Defense_Points) %>% 
  arrange(-Total_Points)

midfielderstats <- merge(mflist.s,midfielderstats,by=c('Player'))
midfielderstats <- midfielderstats %>%
  mutate(Total_Points=Total_Points + Shooting_Points) %>%
  arrange(-Total_Points)









#------Strikers------

### Faybian - Forwards(Strikers) ###
lshooting <- read_excel(playerdata, sheet = 'League Shooting', range = 'B12:AA5566')
tshooting <- read_excel(playerdata, sheet = 'Tournament Shooting', range = 'B12:Z2027', guess_max = 3000)

## Data Exploration and Cleaning ##
# 5554 entries
# NA's in Standard SoT%, Standard G/Sh, Standard G/SoT, Standard Dist, and Expected npxG/Sh
# Some columns have negative values which doesn't make sense
# Also a lot of the minimum values are -0.10 which may suggest some error throughout the data
summary(lshooting)

# 2015 entries
# NA's in Standard SoT%, Standard G/Sh, Standard G/Sot, Standard Dist, Standard FK, Expected xG, Expected npxG, Expected npxG/Sh, Expected G-xG, Expected np:G-xG
summary(tshooting)

# Change DFFW to FWDF and MFFW to FWMF to keep data consistent
lshooting = lshooting %>%
  mutate(Pos = replace(Pos, Pos == "DFFW", "FWDF")) %>%
  mutate(Pos = replace(Pos, Pos == "MFFW", "FWMF"))

tshooting = tshooting %>%
  mutate(Pos = replace(Pos, Pos == "DFFW", "FWDF")) %>%
  mutate(Pos = replace(Pos, Pos == "MFFW", "FWMF"))

# Choosing forwards so remove any player that doesn't play in the forward position
forwards = c("FW", "FWDF", "FWMF")
lshooting = lshooting %>%
  filter(Pos %in% forwards)
tshooting = tshooting %>%
  filter(Pos %in% forwards)

# Take the absolute value of negative values
temp_lshooting = lshooting[c("Player", "Nation", "Pos", "Squad", "League")]
lshooting = abs(lshooting[,!names(lshooting) %in% c("Player", "Nation", "Pos", "Squad", "League")])
lshooting = data.frame(temp_lshooting, lshooting)

temp_tshooting = tshooting[c("Player", "Nation", "Pos", "League")]
tshooting = abs(tshooting[,!names(tshooting) %in% c("Player", "Nation", "Pos", "League")])
tshooting = data.frame(temp_tshooting, tshooting)

# Split the data into 2020 and 2021
lshooting_20 = lshooting %>%
  filter(Year == 2020)
tshooting_20 = tshooting %>%
  filter(Year == 2020)
lshooting_21 = lshooting %>%
  filter(Year == 2021)
tshooting_21 = tshooting %>%
  filter(Year == 2021)

# 932 entries - No significant % of data missing
summary(lshooting_20)
# 979 entries - No significant % of data missing
summary(lshooting_21)
# 430 entries - No values in Standard.Dist, Standard.FK, Expected.xG, Expected.npXG, Expected.npxG.sh, Expected.G.xG, Expected.np.G.xG. Remove these variables in both lshooting and tshooting then filter again.
summary(tshooting_20)
# 161 entries - No significant % of data missing
summary(tshooting_21)

# Remove the variables indicated above
remove = c("Standard.Dist", "Standard.FK", "Expected.xG", "Expected.npxG", "Expected.npxG.Sh", "Expected.G.xG", "Expected.np.G.xG")
lshooting = lshooting[, (!names(lshooting) %in% remove)]
tshooting = tshooting[, (!names(tshooting) %in% remove)]

# Remove outliers for % variables (considered to be those entries >1.05, no longer due to rounding errors)
lshooting <- lshooting %>%
  filter(Standard.SoT.<100.05)
tshooting <- tshooting %>%
  filter(Standard.SoT.<100.05)

# Remove NA values
library(tidyr)
lshooting = lshooting %>%
  drop_na(Standard.SoT., Standard.G.Sh, Standard.G.SoT)

tshooting = tshooting %>%
  drop_na(Standard.SoT., Standard.G.Sh, Standard.G.SoT)

# Remove Squad from lshooting
lshooting = lshooting[,-4]

# Combined data for correlation
cshooting = rbind(lshooting, tshooting)

# Correlation
shooting_cor = cor(cshooting[,unlist(lapply(cshooting, is.numeric))])
ggcorrplot(shooting_cor[,14:1])

# Distribution of data
dens_Age = density(na.omit(cshooting$Age))
plot(dens_Age, col = "blue", lwd = 2, xlab = "Age", main = "Distribution of Age")

dens_90s = density(na.omit(cshooting$X90s))
plot(dens_90s, col = "blue", lwd = 2, xlab = "Minutes played divided by 90", main = "Distribution of Time Played")

dens_Gls = density(na.omit(cshooting$Gls))
plot(dens_Gls, col = "blue", lwd = 2, xlab = "Goals", main = "Distribution of Goals")

dens_Standard.Sh = density(na.omit(cshooting$Standard.Sh))
plot(dens_Standard.Sh, col = "blue", lwd = 2, xlab = "Shots total", main = "Distribution of Shots Total")

# Select variables for modelling - 
cshootingmodel <- cshooting %>%
  select(Age, X90s, Gls, Standard.Sh, Standard.SoT, Standard.SoT., Standard.G.Sh, Standard.G.Sh, Standard.G.SoT, Performance.PK, Performance.PKatt)

# Use 2020 data as training set, and 2021 data as test set
s.train <- which(cshooting$Year == 2020)
s.test <- which(cshooting$Nation != 'Rarita' & cshooting$Year == 2021)
s.testset<-cshootingmodel[s.test,]
s.trainset<-cshootingmodel[s.train,]
s.Rarita <- which(cshooting$Nation == 'Rarita' & cshooting$Year == 2021)
s.Raritaset<-cshootingmodel[s.Rarita,]

## Modelling ##
set.seed(0)
# Regression
s.regression <- lm(Standard.G.SoT~., data = s.trainset)
summary(s.regression)
#Regression MSE calculation
s.regpredict <- predict(s.regression, s.testset)
s.actual <- s.testset$Standard.G.SoT
s.MSEreg<-mean((s.actual-s.regpredict)^2)

#Lasso Regression
lambda_seq <- 10^seq(10, -5, length = 100)
s.lasso <- glmnet(as.matrix(s.trainset[,-8]), s.trainset$Standard.G.SoT, alpha = 1, lambda = lambda_seq)
plot(s.lasso, xvar = "lambda")
#Determining optimal lambda value for ridge regression
s.cvlasso <- cv.glmnet(as.matrix(s.trainset[,-8]), s.trainset$Standard.G.SoT, alpha = 1)
plot(s.cvlasso)
s.bestlam <- s.cvlasso$lambda.min
#Lasso regression MSE calculation
s.lassopredict<-predict(s.cvlasso, as.matrix(s.testset[,-8]))
s.MSElasso <- mean((s.actual-s.lassopredict)^2)

#Regression Tree
s.tree <- rpart(Standard.G.SoT~., s.trainset, cp = 0.001, method = "anova")
rpart.plot(s.tree, box.palette = "GnRd", yesno = 2, split.border.col = 1, split.round = 1)
plotcp(s.tree)
#Regression tree MSE calculation
s.treepredict<-predict(s.tree, s.testset)
s.MSEtree <- mean((s.actual-s.treepredict)^2)

#Random Forest
s.rf <- randomForest(Standard.G.SoT~., data = s.trainset, mtry = sqrt(ncol(s.trainset)), importance = TRUE)
#Variable importance for random forest model
vip(s.rf, num_features = ncol(s.trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(s.trainset) - 1)))+
  theme_classic()
#Random forest MSE calculation
s.rfpredict<-predict(s.rf, s.testset)
s.MSErf<-mean((s.actual-s.rfpredict)^2)

#Bagging
s.bag <- randomForest(Standard.G.SoT~., data = s.trainset, mtry = (ncol(s.trainset)-1),importance = TRUE)
#Variable importance for bagging
vip(s.bag, num_features = ncol(s.trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(s.trainset) - 1)))+
  theme_classic()
#Bagging MSE calculation
s.bagpredict <- predict(s.bag, s.testset)
s.MSEbag<-mean((s.actual-s.bagpredict)^2)

#Boosting
s.boost <- gbm(Standard.G.SoT~., data = s.trainset, 
               n.trees = 1000, interaction.depth = 5, cv.folds = 10)
#Variable importance for boosting
vip(s.boost, num_features = ncol(s.trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(s.trainset) - 1)))+
  theme_classic()
#Boosting MSE calculation
s.bestboost <- gbm.perf(s.boost, method = "cv")
s.boostpredict <- predict(s.boost, newdata = s.testset, n.trees = s.bestboost)
s.MSEboost <- mean((s.actual-s.boostpredict)^2)

  #Best Model
Smse <- data.frame(s.MSEreg,s.MSElasso,s.MSEtree,s.MSErf,s.MSEbag,s.MSEboost)
    #Running bagging model on 2021 Raritan Strikers
s.raritan <- cshooting[s.Rarita,]$Player
s.predict <- predict(s.bag, s.Raritaset, n.trees = s.bestboost)
s.list <- data.frame(s.raritan, s.predict)
colnames(s.list) <- c("Player","Score")
s.list <- arrange(s.list, -Score)

















temp5 <- ldefense[c("Player","Nation","Pos","Squad","League")]
ldefense <- abs(ldefense[,!names(ldefense) %in% c("Player","Nation","Pos","Squad","League")])
ldefense <- data.frame(temp5,ldefense)

temp6 <- tdefense[c("Player","Nation","Pos","League")]
tdefense <- abs(tdefense[,!names(tdefense) %in% c("Player","Nation","Pos","League")])
tdefense <- data.frame(temp6,tdefense)

# Calculating Vs Dribbles Tkl% and Pressures %
ldefense <- ldefense %>%
  mutate(Vs.Dribbles.Tkl.=Vs.Dribbles.Tkl/Vs.Dribbles.Att,Pressures..=Pressures.Succ/Pressures.Press)
tdefense <- tdefense %>%
  mutate(Vs.Dribbles.Tkl.=Vs.Dribbles.Tkl/Vs.Dribbles.Att,Pressures..=Pressures.Succ/Pressures.Press)

tdefense <- tdefense %>%
  filter(Vs.Dribbles.Tkl.<1.05 & Pressures..<1.05)
ldefense <- ldefense %>%
  filter(Vs.Dribbles.Tkl.<1.05 & Pressures..<1.05)











#-------------- Defender Models -------

cdefdefmodel <- cdef %>%
  select(Age,X90s,Tackles.TklW,Tackles.Def.3rd,Vs.Dribbles.Tkl.,Pressures..,Pressures.Def.3rd,
         Blocks.Blocks,Blocks.ShSv,Blocks.Pass,Int,Tkl.Int,Clr,Err)
cdefpassmodel <- cdef %>%
  select(Age,X90s,Total.Cmp.,Total.TotDist,Total.PrgDist,Short.Cmp.,Medium.Cmp.,Long.Cmp.,Ast,A.xA,KP,X1.3,PPA,
         CrsPA, Prog)

d.train <- which(cdef$Year == 2020)
d.test <- which(cdef$Nation != 'Rarita' & cdef$Year == 2021)

dd.testset <- cdefdefmodel[d.test,]
dd.trainset <- cdefdefmodel[d.train,]

dp.testset <- cdefpassmodel[d.test,]
dp.trainset <- cdefpassmodel[d.train,]

d.Rarita <- which(cdef$Nation == 'Rarita' & cdef$Year == 2021)
dd.Raritaset <- cdefdefmodel[d.Rarita,]
dp.Raritaset <- cdefpassmodel[d.Rarita,]

set.seed(0)
#Regression
#Regression MSE calculation

regressiondefdef <- lm(Pressures.. ~ ., data = dd.trainset)
regressiondefpass <- lm(Total.Cmp. ~ ., data = dp.trainset)

actualdefdef <- dd.testset$Pressures..
actualdefpass <- dp.testset$Total.Cmp.

regpredict.dd <- predict(regressiondefdef, dd.testset)
regpredict.dp <- predict(regressiondefpass, dp.testset)

MSEreg.dd <- mean((actualdefdef-regpredict.dd)^2)
MSEreg.dp <- mean((actualdefpass-regpredict.dp)^2)

summary(regressiondefdef)
summary(regressiondefpass)

#Lasso Regression
grid <- 10^seq(10, -5, length = 100)
lassodata.dd <- model.matrix(Pressures.. ~ ., dd.trainset)[,-6] #NOT SURE
lasso.dd <- glmnet(lassodata.dd, dd.trainset$Pressures.., alpha = 1, lambda = grid)
plot(lasso.dd, xvar = "lambda")

grid <- 10^seq(10, -5, length = 100)
lassodata.dp <- model.matrix(Total.Cmp. ~ ., dp.trainset)[,-6] #NOT SURE
lasso.dp <- glmnet(lassodata.dp, dp.trainset$Total.Cmp., alpha = 1, lambda = grid)
plot(lasso.dp, xvar = "lambda")

#Determining optimal lambda value for ridge regression
cvlasso.dd <- cv.glmnet(lassodata.dd, dd.trainset$Pressures.., alpha = 1)
plot(cvlasso.dd, xvar = "lambda")
bestlam.dd <- cvlasso.dd$lambda.min

cvlasso.dp <- cv.glmnet(lassodata.dp, dp.trainset$Total.Cmp., alpha = 1)
plot(cvlasso.dp, xvar = "lambda")
bestlam.dp <- cvlasso.dp$lambda.min

#Lasso regression MSE calculation
lassotestdata.dd <- model.matrix(Pressures.. ~ ., dd.testset)[,-6] #NOT SURE
lassotestdata.dp <- model.matrix(Total.Cmp. ~ ., dp.testset)[,-6] #NOT SURE

lassopredict.dd <- predict(cvlasso.dd, newx = lassotestdata.dd)
lassopredict.dp <- predict(cvlasso.dp, newx = lassotestdata.dp)

MSElasso.dd <- mean((actualdefdef-lassopredict.dd)^2)
MSElasso.dp <- mean((actualdefpass-lassopredict.dp)^2)

  #Tree
treegoal.dd <- rpart(Pressures.. ~ ., dd.trainset, cp = 0.001, method = "anova")
treegoal.dp <- rpart(Total.Cmp. ~ ., dp.trainset, cp = 0.001, method = "anova")

#Regression Tree
rpart.plot(treegoal.dd, box.palette = "GnRd", yesno = 2, split.border.col = 1, split.round = 1)
plotcp(treegoal.dd)
rpart.plot(treegoal.dp, box.palette = "GnRd", yesno = 2, split.border.col = 1, split.round = 1)
plotcp(treegoal.dp)

#Regression tree MSE calculation
treepredict.dd <- predict(treegoal.dd, dd.testset)
treepredict.dp <- predict(treegoal.dp, dp.testset)

MSEtree.dd <- mean((actualdefdef-treepredict.dd)^2)
MSEtree.dp <- mean((actualdefpass-treepredict.dp)^2)

#Random Forest
rf.dd <- randomForest(Pressures..  ~ ., data = dd.trainset, mtry = sqrt(ncol(dd.trainset)), importance = TRUE)
rf.dp <- randomForest(Total.Cmp.  ~ ., data = dp.trainset, mtry = sqrt(ncol(dp.trainset)), importance = TRUE)

vip(rf.dd, num_features = ncol(dd.trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(dd.trainset) - 1)))+
  theme_classic()
vip(rf.dp, num_features = ncol(dp.trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(dp.trainset) - 1)))+
  theme_classic()

rfpredict.dd<-predict(rf.dd, dd.testset)
MSErf.dd<-mean((actualdefdef-rfpredict.dd)^2)
rfpredict.dp<-predict(rf.dp, dp.testset)
MSErf.dp<-mean((actualdefpass-rfpredict.dp)^2)

#Bagging
bag.dd <- randomForest(Pressures.. ~., data = dd.trainset, mtry=(ncol(dd.trainset)-1), importance = TRUE)
bag.dp <- randomForest(Total.Cmp. ~., data = dp.trainset, mtry=(ncol(dp.trainset)-1), importance = TRUE)

#Variable importance for bagging
vip(bag.dd, num_features = ncol(dd.trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(dd.trainset) - 1)))+
  theme_classic()

vip(bag.dp, num_features = ncol(dp.trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(dp.trainset) - 1)))+
  theme_classic()

#Bagging MSE calculation
bagpredict.dd <- predict(bag.dd, dd.testset)
bagpredict.dp <- predict(bag.dp, dp.testset)

MSEbag.dd <- mean((actualdefdef-bagpredict.dd)^2)
MSEbag.dp <- mean((actualdefpass-bagpredict.dp)^2)

#Boosting
boost.dd <- gbm(Pressures.. ~ ., data = dd.trainset, 
                 n.trees = 1000, interaction.depth = 5, cv.folds = 10)
boost.dp <- gbm(Total.Cmp. ~ ., data = dp.trainset, 
                n.trees = 1000, interaction.depth = 5, cv.folds = 10)

vip(boost.dd, num_features = ncol(dd.trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(dd.trainset) - 1)))+
  theme_classic()
vip(boost.dp, num_features = ncol(dp.trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(dp.trainset) - 1)))+
  theme_classic()

bestboost.dd <- gbm.perf(boost.dd, method = "cv")
bestboost.dp <- gbm.perf(boost.dp, method = "cv")

boostpredict.dd <- predict(boost.dd, newdata = dd.testset, n.trees = bestboost.dd)
boostpredict.dp <- predict(boost.dp, newdata = dp.testset, n.trees = bestboost.dp)

MSEboost.dd <- mean((actualdefdef-boostpredict.dd)^2)
MSEboost.dp <- mean((actualdefpass-boostpredict.dp)^2)

  #Best model
DDmse <- data.frame(MSEreg.dd,MSElasso.dd,MSEtree.dd,MSErf.dd,MSEbag.dd,MSEboost.dd)
DPmse <- data.frame(MSEreg.dp,MSElasso.dp,MSEtree.dp,MSErf.dp,MSEbag.dp,MSEboost.dp)
  
#Running prediction on Raritan players - Defenders (Defense)
raritandef.d <- cdef[d.Rarita,]$Player
defpredict.d <- predict(rf.dd, dd.Raritaset, n.trees = bestboost.dd)
deflist.d <- data.frame(raritandef.d,defpredict.d)
colnames(deflist.d) <- c("Player","Defense_Score")
deflist.d <- deflist.d %>% mutate(Defense_Points = rank(deflist.d[,2]))
deflist.d <- deflist.d %>% arrange(-Defense_Points) %>% 
  mutate(Defense_Points = Defense_Points*0.75)

  #Running prediction on Raritan players - Defenders (Passing)
raritandef.p <- cdef[d.Rarita,]$Player
defpredict.p <- predict(boost.dp, dp.Raritaset, n.trees = bestboost.dp)
deflist.p <- data.frame(raritandef.p,defpredict.p)
colnames(deflist.p) <- c("Player","Passing_Score")
deflist.p <- deflist.p %>% mutate(Passing_Points = rank(deflist.p[,2]))
deflist.p <- deflist.p %>% arrange(-Passing_Points) %>% 
  mutate(Passing_Points = Passing_Points*0.25)

  #Combining passing and defending points for defenders
defenderstats <- merge(deflist.p, deflist.d,by=c('Player'))
defenderstats <- defenderstats %>% 
  mutate(Total_Points=Passing_Points + Defense_Points) %>% 
  arrange(-Total_Points)


#-----------Economic Stuff------------
  #Forecasting Raritan GDP
    #East Rarita
      #Changing GDP table to a suitable format for forecasting
eastgdp <- gdp %>%
  select(c('East Rarita'))
eastgdpts <- ts(eastgdp,frequency = 1,start=2011)

eastgdpfit <- auto.arima(eastgdpts)
autoplot(forecast(eastgdpfit,h=10))+
  ggtitle('Forecasted Eastern Rarita GDP from 2021-2030')+
  xlab('Year')+
  ylab('GDP')+
  theme_bw()

    #Central Rarita
      #Changing GDP table to a suitable format for forecasting
centralgdp <- gdp %>%
  select(c('Central Rarita'))
centralgdpts <- ts(centralgdp,frequency = 1,start=2011)

centralgdpfit <- ets(centralgdpts,model='AAN')
autoplot(forecast(centralgdpfit,h=10))+
  ggtitle('Forecasted Central Rarita GDP from 2021-2030')+
  xlab('Year')+
  ylab('GDP')+
  theme_bw()

    #West Rarita
      #Changing GDP table to a suitable format for forecasting
westgdp <- gdp %>%
  select(c('West Rarita'))
westgdpts <- ts(westgdp,frequency = 1,start=2011)

westgdpfit <- auto.arima(westgdpts)
autoplot(forecast(westgdpfit,h=10))+
  ggtitle('Forecasted Western Rarita GDP from 2021-2030')+
  xlab('Year')+
  ylab('GDP')+
  theme_bw()

    #Rarita
      #Changing GDP table to a suitable format for forecasting
rgdp <- gdp %>%
  select(c('Rarita'))
rgdpts <- ts(rgdp,frequency = 1,start=2011)

rgdpfit <- auto.arima(rgdpts)
autoplot(forecast(rgdpfit,h=10)) +
  ggtitle('Forecasted Raritan GDP from 2021-2030') +
  xlab('Year') +
  ylab('GDP') +
  theme_bw()


  #Forecasting Raritan GNI
    #East Rarita
      #Changing GNI table to a suitable format for forecasting
eastgni <- gni %>%
  select(c('East Rarita'))
eastgnits <- ts(eastgni,frequency = 1,start=2011)

eastgnifit <- ets(eastgnits,model='AAN')
autoplot(forecast(eastgnifit,h=10))+
  ggtitle('Forecasted Eastern Rarita GNI from 2021-2030')+
  xlab('Year')+
  ylab('GNI')+
  theme_bw()

    #Central Rarita
      #Changing GNI table to a suitable format for forecasting
centralgni <- gni %>%
  select(c('Central Rarita'))
centralgnits <- ts(centralgni,frequency = 1,start=2011)

centralgnifit <- ets(centralgnits,model='AAN')
autoplot(forecast(centralgnifit,h=10))+
  ggtitle('Forecasted Central Rarita GNI from 2021-2030')+
  xlab('Year')+
  ylab('GNI')+
  theme_bw()

    #West Rarita
      #Changing GNI table to a suitable format for forecasting
westgni <- gni %>%
  select(c('West Rarita'))
westgnits <- ts(westgni,frequency = 1,start=2011)

westgnifit <- auto.arima(westgnits)
autoplot(forecast(westgnifit,h=10))+
  ggtitle('Forecasted Western Rarita GNI from 2021-2030')+
  xlab('Year')+
  ylab('GNI')+
  theme_bw()

    #Rarita
      #Changing GNI table to a suitable format for forecasting
rgni <- gni %>%
  select(c('Rarita'))
rgnits <- ts(rgni/100000000000,frequency = 1,start=2011)
rgnits2 <- ts(rgni,frequency=1,start=2011)

rgnifit <- auto.arima(rgnits)
plot(forecast(rgnifit,h=10))
rgnifit2 <- ets(rgnits2,model='AAN')
autoplot(forecast(rgnifit2,h=10)) +
  ggtitle('Forecasted Raritan GNI from 2021-2030')+
  xlab('Year')+
  ylab('GNI')+
  theme_bw()
  

  #Forecasting Raritan revenue data
raritarevenue <- revenue %>%
  filter(`_Nation` == 'Rarita')
raritarevenue <- transpose(raritarevenue)
raritarevenue <- as.numeric(raritarevenue[-1,])
raritarevenue <- rev(raritarevenue)

    #Total revenue
totalrevenue <- raritarevenue[16:20]
totalrevenuets <- ts(totalrevenue,frequency = 1, start=2016)

totalrevenuefit <- auto.arima(totalrevenuets)
totalrevenuefit2 <- ets(totalrevenuets,model='AAN')
plot(forecast(totalrevenuefit,h=10))
autoplot(forecast(totalrevenuefit2,h=10))+
  ggtitle('Forecasted Raritan revenue from 2021-2030')+
  xlab('Year')+
  ylab('Revenue')+
  theme_bw()

    #Matchday revenue
matchdayrevenue <- raritarevenue[11:15]
matchdayrevenuets <- ts(matchdayrevenue,frequency=1,start=2016)

matchdayrevenuefit <- auto.arima(matchdayrevenuets)
matchdayrevenuefit2 <- ets(matchdayrevenuets,model='AAN')
plot(forecast(matchdayrevenuefit,h=10))
autoplot(forecast(matchdayrevenuefit2,h=10))+
  ggtitle('Forecasted matchday revenue from 2021-2030')+
  xlab('Year')+
  ylab('Revenue')+
  theme_bw()

    #Broadcast revenue
broadcastrevenue <- raritarevenue[6:10]
broadcastrevenuets <- ts(broadcastrevenue,frequency=1,start=2016)

broadcastrevenuefit <- auto.arima(broadcastrevenuets)
broadcastrevenuefit2 <- ets(broadcastrevenuets,model='AAN')
plot(forecast(broadcastrevenuefit,h=10))
autoplot(forecast(broadcastrevenuefit2,h=10))+
  ggtitle('Forecasted broadcast revenue from 2021-2030')+
  xlab('Year')+
  ylab('Revenue')+
  theme_bw()

    #Commercial revenue
commercialrevenue <- raritarevenue[1:5]
commercialrevenuets <- ts(commercialrevenue,frequency=1,start=2016)

commercialrevenuefit <- auto.arima(commercialrevenuets)
commercialrevenuefit2 <- ets(commercialrevenuets,model='AAN')
plot(forecast(commercialrevenuefit,h=10))
autoplot(forecast(commercialrevenuefit2,h=10))+
  ggtitle('Forecasted commercial revenue from 2021-2030')+
  xlab('Year')+
  ylab('Revenue')+
  theme_bw()


  #Forecasting Raritan expense data
raritaexp <- expenses %>%
  filter(`_Nation` == 'Rarita')
raritaexp <- transpose(raritaexp)
raritaexp <- as.numeric(raritaexp[-1,])
raritaexp <- rev(raritaexp)

    #Total expenses
totalexp <- raritaexp[11:15]
totalexpts <- ts(totalexp,frequency = 1, start=2016)

totalexpfit <- auto.arima(totalexpts)
totalexpfit2 <- ets(totalexpts,model='AAN')
plot(forecast(totalexpfit,h=10))
autoplot(forecast(totalexpfit2,h=10))+
  ggtitle('Forecasted Raritan expenses from 2021-2030')+
  xlab('Year')+
  ylab('Expenses')+
  theme_bw()

    #Staff costs
staffcost <- raritaexp[6:10]
staffcostts <- ts(staffcost,frequency=1,start=2016)

staffcostfit <- auto.arima(staffcostts)
staffcostfit2 <- ets(staffcostts,model='AAN')
plot(forecast(staffcostfit,h=10))
autoplot(forecast(staffcostfit2,h=10))+
  ggtitle('Forecasted staff costs from 2021-2030')+
  xlab('Year')+
  ylab('Staff Costs')+
  theme_bw()

    #Other expenses
otherexp <- raritaexp[1:5]
otherexpts <- ts(otherexp,frequency=1,start=2016)

otherexpfit <- auto.arima(otherexpts)
otherexpfit2 <- ets(otherexpts,model='AAN')
plot(forecast(otherexpfit,h=10))
autoplot(forecast(otherexpfit2,h=10))+
  ggtitle('Forecasted other expenses from 2021-2030')+
  xlab('Year')+
  ylab('Other Expenses')+
  theme_bw()


  #Forecasted profit/loss
frevenue <- forecast(totalrevenuefit2,h=10)
fexp <- forecast(totalexpfit2,h=10)
profitlosspc <- frevenue$mean - fexp$mean #per capita forecast

raritapop <- pop %>%
  select(c('Rarita'))
raritapopts <- ts(raritapop,frequency = 1,start=2011)

raritapopfit <- auto.arima(raritapopts,model='AAN')
fpop <- forecast(raritapopfit,h=10)

profitloss <- profitlosspc*fpop$mean



#------------Prediction of Competitiveness-------------

####---------The Selected Team

goalkeepers <- gklist[1:3,1]
midfielders <- midfielderstats[1:6,1]
defenders <- defenderstats[1:8,1]
strikers <- s.list[c(1,2,4:7),1]




gk <- cgoal[Rarita,] %>% filter(Player %in% goalkeepers) %>% select(-c('Year'))
mf <- cmf[Rarita1,] %>% filter(Player %in% midfielders) 
def <- cdef[d.Rarita,] %>% filter(Player %in% defenders)%>% mutate(Born = Born.x)
strk <- cshooting[s.Rarita,] %>% filter(Player %in% strikers) 


RaritaStats <- bind_rows(gk,mf,def,strk) %>% select(Player, Nation, Pos, Age, Performance.Save., Total.Cmp., `Tackle+Pressure%`, Standard.G.SoT, Pressures..)
teamlist <- c(RaritaStats$Player)

#Take overall team average
RaritaTeam <- RaritaStats %>% select(-c("Player","Nation","Pos"))
RaritaTeam <- data.frame(colMeans(RaritaTeam, na.rm=T))
RaritaTeam <- data.frame(t(RaritaTeam))
rownames(RaritaTeam)<-'Rarita'

#https://www.datasciencemadesimple.com/get-mean-of-a-column-in-r-2/ this might be a cleaner waythan what i did?

Tournanmentstats <- bind_rows(tgoal2021, tpassing2021, tdefense2021, tshooting2021) 

Tournanmentstats <- Tournanmentstats %>% mutate(`Tackle+Pressure%` = `Vs.Dribbles.Tkl.` + `Pressures..`) %>%
  select(Player, Nation, Pos, Age, Performance.Save., Total.Cmp., `Tackle+Pressure%`, Standard.G.SoT, Pressures.., `Tournament Place`)
Tournanmentstats <- Tournanmentstats %>% filter(`Tournament Place`<25) %>% group_by(`Nation`)



Teams <- data.frame(aggregate(Tournanmentstats[,5:10], list(Tournanmentstats$Nation), mean, na.rm=T))
rownames(Teams) <- Teams$Group.1
Teams <- Teams[,-1]

#imputating the missing goalkeeper data for Dosqaly: doesn't make any sense to play without a goalkeeper
library('mice')
pred <- quickpred(Teams)
impute <- mice(Teams, pred = pred, seed=0, m = 3, maxit = 5)
Teams <- complete(impute,1)
DosqalyGK<-impute$imp$Performance.Save.$`1`

#------Modelling on Teams data to Predict probability of Raritan team winning-----------

place.reg <- lm(Tournament.Place~., data=Teams)
summary(place.reg)

place.rf <- randomForest(Tournament.Place~., data = Teams, importance = TRUE, mtry = sqrt(ncol(Teams)), na.action = na.exclude)
place.bag <- randomForest(Tournament.Place~., data = Teams, importance = TRUE, mtry = ncol(Teams)-1, na.action = na.exclude)

#predicted tournament ranking if played in 2021

place <- round(predict(place.rf, newdata=RaritaTeam, type = "response"),0) #Predicted tournament place 



#ASSUMPTION, randomness in players skill as they age, multiply by random number, with mean 1, sd = 0.05 up to age 30, after that decrease by 5%
#, after age 35 (avg retirement age) decrease 10% to put extreme decline on those who naturally would've retired at this point
#Retirements not accounted for
Tournanmentstats1 <- Tournanmentstats[,-10]

tstats2021 <- bind_rows(RaritaStats, Tournanmentstats1)

##-----------2022 Tournament stats----------
set.seed(0)
tstats2022 <- tstats2021 %>% mutate(Age=Age+1) %>% mutate(Year=2022)
for(i in 1:nrow(tstats2022)){
  AGE<-tstats2022[i,]$Age
  if(AGE<=30){
    tstats2022[i,5:9]<-tstats2022[i,5:9]*rnorm(1,1,0.05)
  } else if(AGE>30 & AGE <= 35) {
    tstats2022[i,5:9]<-tstats2022[i,5:9]*0.95
  } else {
    tstats2022[i,5:9]<-tstats2022[i,5:9]*0.9
  }
}
Teams2022 <- data.frame(aggregate(tstats2022[,5:9], list(tstats2022$Nation), mean, na.rm=T))
rownames(Teams2022) <- Teams2022$Group.1
Teams2022 <- Teams2022[,-1]
Teams2022[is.na(Teams2022)]<-DosqalyGK
place2022 <- data.frame(predict(place.reg, newdata=Teams2022, type = "response"))
place2022 <- place2022 %>% mutate(Place = rank(place2022[,1])) %>%  select(Place)

##-----------2023 Tournament stats----------
set.seed(0)
tstats2023 <- tstats2022 %>% mutate(Age=Age+1) %>% mutate(Year=2023)
for(i in 1:nrow(tstats2023)){
  AGE<-tstats2023[i,]$Age
  if(AGE<=30){
    tstats2023[i,5:9]<-tstats2023[i,5:9]*rnorm(1,1,0.05)
  } else if(AGE>30 & AGE <= 35) {
    tstats2023[i,5:9]<-tstats2023[i,5:9]*0.95
  } else {
    tstats2023[i,5:9]<-tstats2023[i,5:9]*0.9
  }
}
Teams2023 <- data.frame(aggregate(tstats2023[,5:9], list(tstats2023$Nation), mean, na.rm=T))
rownames(Teams2023) <- Teams2023$Group.1
Teams2023 <- Teams2023[,-1]
Teams2023[is.na(Teams2023)]<-DosqalyGK
place2023 <- data.frame(predict(place.reg, newdata=Teams2023, type = "response"))
place2023 <- place2023 %>% mutate(Place = rank(place2023[,1])) %>% select(Place)

##-----------2024 Tournament stats----------
set.seed(0)
tstats2024 <- tstats2023 %>% mutate(Age=Age+1) %>% mutate(Year=2024)
for(i in 1:nrow(tstats2024)){
  AGE<-tstats2024[i,]$Age
  if(AGE<=30){
    tstats2024[i,5:9]<-tstats2024[i,5:9]*rnorm(1,1,0.05)
  } else if(AGE>30 & AGE <= 35) {
    tstats2024[i,5:9]<-tstats2024[i,5:9]*0.95
  } else {
    tstats2024[i,5:9]<-tstats2024[i,5:9]*0.9
  }
}
Teams2024 <- data.frame(aggregate(tstats2024[,5:9], list(tstats2024$Nation), mean, na.rm=T))
rownames(Teams2024) <- Teams2024$Group.1
Teams2024 <- Teams2024[,-1]
Teams2024[is.na(Teams2024)]<-DosqalyGK
place2024 <- data.frame(predict(place.reg, newdata=Teams2024, type = "response"))
place2024 <- place2024 %>% mutate(Place = rank(place2024[,1])) %>% select(Place)

##-----------2025 Tournament stats----------
set.seed(0)
tstats2025 <- tstats2024 %>% mutate(Age=Age+1) %>% mutate(Year=2025)
for(i in 1:nrow(tstats2025)){
  AGE<-tstats2025[i,]$Age
  if(AGE<=30){
    tstats2025[i,5:9]<-tstats2025[i,5:9]*rnorm(1,1,0.05)
  } else if(AGE>30 & AGE <= 35) {
    tstats2025[i,5:9]<-tstats2025[i,5:9]*0.95
  } else {
    tstats2025[i,5:9]<-tstats2025[i,5:9]*0.9
  }
}
Teams2025 <- data.frame(aggregate(tstats2025[,5:9], list(tstats2025$Nation), mean, na.rm=T))
rownames(Teams2025) <- Teams2025$Group.1
Teams2025 <- Teams2025[,-1]
Teams2025[is.na(Teams2025)]<-DosqalyGK
place2025 <- data.frame(predict(place.reg, newdata=Teams2025, type = "response"))
place2025 <- place2025 %>% mutate(Place = rank(place2025[,1])) %>% select(Place)

##-----------2026 Tournament stats----------
set.seed(0)
tstats2026 <- tstats2025 %>% mutate(Age=Age+1) %>% mutate(Year=2026)
for(i in 1:nrow(tstats2026)){
  AGE<-tstats2026[i,]$Age
  if(AGE<=30){
    tstats2026[i,5:9]<-tstats2026[i,5:9]*rnorm(1,1,0.05)
  } else if(AGE>30 & AGE <= 35) {
    tstats2026[i,5:9]<-tstats2026[i,5:9]*0.95
  } else {
    tstats2026[i,5:9]<-tstats2026[i,5:9]*0.9
  }
}
Teams2026 <- data.frame(aggregate(tstats2026[,5:9], list(tstats2026$Nation), mean, na.rm=T))
rownames(Teams2026) <- Teams2026$Group.1
Teams2026 <- Teams2026[,-1]
Teams2026[is.na(Teams2026)]<-DosqalyGK
place2026 <- data.frame(predict(place.reg, newdata=Teams2026, type = "response"))
place2026 <- place2026 %>% mutate(Place = rank(place2026[,1])) %>% select(Place)

##-----------2027 Tournament stats----------
set.seed(0)
tstats2027 <- tstats2026 %>% mutate(Age=Age+1) %>% mutate(Year=2027)
for(i in 1:nrow(tstats2027)){
  AGE<-tstats2027[i,]$Age
  if(AGE<=30){
    tstats2027[i,5:9]<-tstats2027[i,5:9]*rnorm(1,1,0.05)
  } else if(AGE>30 & AGE <= 35) {
    tstats2027[i,5:9]<-tstats2027[i,5:9]*0.95
  } else {
    tstats2027[i,5:9]<-tstats2027[i,5:9]*0.9
  }
}
Teams2027 <- data.frame(aggregate(tstats2027[,5:9], list(tstats2027$Nation), mean, na.rm=T))
rownames(Teams2027) <- Teams2027$Group.1
Teams2027 <- Teams2027[,-1]
Teams2027[is.na(Teams2027)]<-DosqalyGK
place2027 <- data.frame(predict(place.reg, newdata=Teams2027, type = "response"))
place2027 <- place2027 %>% mutate(Place = rank(place2027[,1])) %>% select(Place)

##-----------2028 Tournament stats----------
set.seed(0)
tstats2028 <- tstats2027 %>% mutate(Age=Age+1) %>% mutate(Year=2028)
for(i in 1:nrow(tstats2028)){
  AGE<-tstats2028[i,]$Age
  if(AGE<=30){
    tstats2028[i,5:9]<-tstats2028[i,5:9]*rnorm(1,1,0.05)
  } else if(AGE>30 & AGE <= 35) {
    tstats2028[i,5:9]<-tstats2028[i,5:9]*0.95
  } else {
    tstats2028[i,5:9]<-tstats2028[i,5:9]*0.9
  }
}
Teams2028 <- data.frame(aggregate(tstats2028[,5:9], list(tstats2028$Nation), mean, na.rm=T))
rownames(Teams2028) <- Teams2028$Group.1
Teams2028 <- Teams2028[,-1]
Teams2028[is.na(Teams2028)]<-DosqalyGK
place2028 <- data.frame(predict(place.reg, newdata=Teams2028, type = "response"))
place2028 <- place2028 %>% mutate(Place = rank(place2028[,1])) %>% select(Place)

##-----------2029 Tournament stats----------
set.seed(0)
tstats2029 <- tstats2028 %>% mutate(Age=Age+1) %>% mutate(Year=2029)
for(i in 1:nrow(tstats2029)){
  AGE<-tstats2029[i,]$Age
  if(AGE<=30){
    tstats2029[i,5:9]<-tstats2029[i,5:9]*rnorm(1,1,0.05)
  } else if(AGE>30 & AGE <= 35) {
    tstats2029[i,5:9]<-tstats2029[i,5:9]*0.95
  } else {
    tstats2029[i,5:9]<-tstats2029[i,5:9]*0.9
  }
}
Teams2029 <- data.frame(aggregate(tstats2029[,5:9], list(tstats2029$Nation), mean, na.rm=T))
rownames(Teams2029) <- Teams2029$Group.1
Teams2029 <- Teams2029[,-1]
Teams2029[is.na(Teams2029)]<-DosqalyGK
place2029 <- data.frame(predict(place.reg, newdata=Teams2029, type = "response"))
place2029 <- place2029 %>% mutate(Place = rank(place2029[,1])) %>% select(Place)

##-----------2030 Tournament stats----------
set.seed(0)
tstats2030 <- tstats2029 %>% mutate(Age=Age+1) %>% mutate(Year=2030)
for(i in 1:nrow(tstats2030)){
  AGE<-tstats2030[i,]$Age
  if(AGE<=30){
    tstats2030[i,5:9]<-tstats2030[i,5:9]*rnorm(1,1,0.05)
  } else if(AGE>30 & AGE <= 35) {
    tstats2030[i,5:9]<-tstats2030[i,5:9]*0.95
  } else {
    tstats2030[i,5:9]<-tstats2030[i,5:9]*0.9
  }
}
Teams2030 <- data.frame(aggregate(tstats2030[,5:9], list(tstats2030$Nation), mean, na.rm=T))
rownames(Teams2030) <- Teams2030$Group.1
Teams2030 <- Teams2030[,-1]
Teams2030[is.na(Teams2030)]<-DosqalyGK
place2030 <- data.frame(predict(place.reg, newdata=Teams2030, type = "response"))
place2030 <- place2030 %>% mutate(Place = rank(place2030[,1])) %>% select(Place)

##-----------2031 Tournament stats----------
set.seed(0)
tstats2031 <- tstats2030 %>% mutate(Age=Age+1) %>% mutate(Year=2031)
for(i in 1:nrow(tstats2031)){
  AGE<-tstats2031[i,]$Age
  if(AGE<=30){
    tstats2031[i,5:9]<-tstats2031[i,5:9]*rnorm(1,1,0.05)
  } else if(AGE>30 & AGE <= 35) {
    tstats2031[i,5:9]<-tstats2031[i,5:9]*0.95
  } else {
    tstats2031[i,5:9]<-tstats2031[i,5:9]*0.9
  }
}
Teams2031 <- data.frame(aggregate(tstats2031[,5:9], list(tstats2031$Nation), mean, na.rm=T))
rownames(Teams2031) <- Teams2031$Group.1
Teams2031 <- Teams2031[,-1]
Teams2031[is.na(Teams2031)]<-DosqalyGK
place2031 <- data.frame(predict(place.reg, newdata=Teams2031, type = "response"))
place2031 <- place2031 %>% mutate(Place = rank(place2031[,1])) %>% select(Place)

##-----------2032 Tournament stats----------
set.seed(0)
tstats2032 <- tstats2031 %>% mutate(Age=Age+1) %>% mutate(Year=2032)
for(i in 1:nrow(tstats2032)){
  AGE<-tstats2032[i,]$Age
  if(AGE<=30){
    tstats2032[i,5:9]<-tstats2032[i,5:9]*rnorm(1,1,0.05)
  } else if(AGE>30 & AGE <= 35) {
    tstats2032[i,5:9]<-tstats2032[i,5:9]*0.95
  } else {
    tstats2032[i,5:9]<-tstats2032[i,5:9]*0.9
  }
}
Teams2032 <- data.frame(aggregate(tstats2032[,5:9], list(tstats2032$Nation), mean, na.rm=T))
rownames(Teams2032) <- Teams2032$Group.1
Teams2032 <- Teams2032[,-1]
Teams2032[is.na(Teams2032)]<-DosqalyGK
place2032 <- data.frame(predict(place.reg, newdata=Teams2032, type = "response"))
place2032 <- place2032 %>% mutate(Place = rank(place2032[,1])) %>% select(Place)

#------forecast of placements for next 10 years------
set.seed(0)
place10years <- cbind(place2022,place2023,place2024,place2025,place2026,place2027,place2028,place2029,place2030,place2031,place2032)
years <- c("2022","2023", "2024", "2025","2026","2027","2028","2029","2030","2031","2032")
yay <- data.frame(years, t(place10years))
#plotting the forecast places
par(mar=c(5.1, 4.1, 4.1, 9.1), xpd=FALSE)
matplot(yay[,-1], ann=TRUE, lty=1, type='l', lwd=1,ylim = c(1,25), xlim = c(1,11), col = c('deepskyblue','firebrick1','greenyellow','green4','darkgoldenrod2', 'orchid4','lemonchiffon4','lemonchiffon3','royalblue4','tan4',
                                                                                'turquoise4','saddlebrown','olivedrab','plum','mistyrose3','slategrey','violet','darksalmon','lightcoral','lightseagreen','powderblue',
                                                                                'lightgreen','palegoldenrod','sienna2','yellow3'),
        xaxt = "n", yaxt = "n", xlab = 'Year', ylab = 'Tournament Place',main = 'Forecast of Tournament Placements for Next 10 Years', las=2)
grid()
axis(1, at=1:11, labels = years)
axis(2, at = 1:25, labels = c(1:25), las=2)
par(mar=c(5.1, 4.1, 4.1, 9.1), xpd=TRUE)
legend("topright", inset = c(-0.33,0), legend=colnames(yay[,-1]), lty=1, cex=0.6, col = c('deepskyblue','firebrick1','greenyellow','green4','darkgoldenrod2', 'orchid4','lemonchiffon4','lemonchiffon3','royalblue4','tan4',
                                                                                         'turquoise4','saddlebrown','olivedrab','plum','mistyrose3','slategrey','violet','darksalmon','lightcoral','lightseagreen','powderblue',
                                                                                         'lightgreen','palegoldenrod','sienna2','yellow3'))
#forecasting Rarita's team probability of winning whole tournament
#to find actual probability of winning (coming first), make column with yes or no for winning and then do modelling on that


TeamsWin <- Teams %>% mutate(First = ifelse(Teams$Tournament.Place == 1, '1', '0')) %>% select(-c('Tournament.Place'))

#This would be the predicted probability of coming first in 2021

#classification tree model
tree <- rpart(First~., TeamsWin, cp=0.01)
predict(tree, newdata = RaritaTeam, type = 'prob')

#random forest
rf <- randomForest(factor(First)~., data = TeamsWin, importance = TRUE, mtry = sqrt(ncol(TeamsWin)), na.action = na.exclude)
predict(rf, newdata = RaritaTeam, type = 'prob')

#bag
bag <- randomForest(factor(First)~., data = TeamsWin, importance = TRUE, mtry = ncol(TeamsWin)-1, na.action = na.exclude)
predict(bag, newdata = RaritaTeam, type = 'prob')

#rf is best

#------2022 Rarita win probability------
set.seed(0)
RaritaTeam2022<-tstats2022 %>% filter(Nation=='Rarita')
RaritaTeam2022 <- RaritaTeam2022 %>% select(-c("Player","Nation","Pos","Year"))
RaritaTeam2022 <- data.frame(colMeans(RaritaTeam2022, na.rm=T))
RaritaTeam2022 <- data.frame(t(RaritaTeam2022))
rownames(RaritaTeam2022)<-'Rarita'
win.prob2022 <- data.frame(predict(rf, newdata = RaritaTeam2022, type = 'prob'))$X1

#------2023 Rarita win probability------
set.seed(0)
RaritaTeam2023<-tstats2023 %>% filter(Nation=='Rarita')
RaritaTeam2023 <- RaritaTeam2023 %>% select(-c("Player","Nation","Pos","Year"))
RaritaTeam2023 <- data.frame(colMeans(RaritaTeam2023, na.rm=T))
RaritaTeam2023 <- data.frame(t(RaritaTeam2023))
rownames(RaritaTeam2023)<-'Rarita'
win.prob2023 <- data.frame(predict(rf, newdata = RaritaTeam2023, type = 'prob'))$X1

#------2024 Rarita win probability------
set.seed(0)
RaritaTeam2024<-tstats2024 %>% filter(Nation=='Rarita')
RaritaTeam2024 <- RaritaTeam2024 %>% select(-c("Player","Nation","Pos","Year"))
RaritaTeam2024 <- data.frame(colMeans(RaritaTeam2024, na.rm=T))
RaritaTeam2024 <- data.frame(t(RaritaTeam2024))
rownames(RaritaTeam2024)<-'Rarita'
win.prob2024 <- data.frame(predict(rf, newdata = RaritaTeam2024, type = 'prob'))$X1

#------2025 Rarita win probability------
set.seed(0)
RaritaTeam2025<-tstats2025 %>% filter(Nation=='Rarita')
RaritaTeam2025 <- RaritaTeam2025 %>% select(-c("Player","Nation","Pos","Year"))
RaritaTeam2025 <- data.frame(colMeans(RaritaTeam2025, na.rm=T))
RaritaTeam2025 <- data.frame(t(RaritaTeam2025))
rownames(RaritaTeam2025)<-'Rarita'
win.prob2025 <- data.frame(predict(rf, newdata = RaritaTeam2025, type = 'prob'))$X1

#------2026 Rarita win probability------
set.seed(0)
RaritaTeam2026<-tstats2026 %>% filter(Nation=='Rarita')
RaritaTeam2026 <- RaritaTeam2026 %>% select(-c("Player","Nation","Pos","Year"))
RaritaTeam2026 <- data.frame(colMeans(RaritaTeam2026, na.rm=T))
RaritaTeam2026 <- data.frame(t(RaritaTeam2026))
rownames(RaritaTeam2026)<-'Rarita'
win.prob2026 <- data.frame(predict(rf, newdata = RaritaTeam2026, type = 'prob'))$X1

#------2027 Rarita win probability------
set.seed(0)
RaritaTeam2027<-tstats2027 %>% filter(Nation=='Rarita')
RaritaTeam2027 <- RaritaTeam2027 %>% select(-c("Player","Nation","Pos","Year"))
RaritaTeam2027 <- data.frame(colMeans(RaritaTeam2027, na.rm=T))
RaritaTeam2027 <- data.frame(t(RaritaTeam2027))
rownames(RaritaTeam2027)<-'Rarita'
win.prob2027 <- data.frame(predict(rf, newdata = RaritaTeam2027, type = 'prob'))$X1

#------2028 Rarita win probability------
set.seed(0)
RaritaTeam2028<-tstats2028 %>% filter(Nation=='Rarita')
RaritaTeam2028 <- RaritaTeam2028 %>% select(-c("Player","Nation","Pos","Year"))
RaritaTeam2028 <- data.frame(colMeans(RaritaTeam2028, na.rm=T))
RaritaTeam2028 <- data.frame(t(RaritaTeam2028))
rownames(RaritaTeam2028)<-'Rarita'
win.prob2028 <- data.frame(predict(rf, newdata = RaritaTeam2028, type = 'prob'))$X1

#------2029 Rarita win probability------
set.seed(0)
RaritaTeam2029<-tstats2029 %>% filter(Nation=='Rarita')
RaritaTeam2029 <- RaritaTeam2029 %>% select(-c("Player","Nation","Pos","Year"))
RaritaTeam2029 <- data.frame(colMeans(RaritaTeam2029, na.rm=T))
RaritaTeam2029 <- data.frame(t(RaritaTeam2029))
rownames(RaritaTeam2029)<-'Rarita'
win.prob2029 <- data.frame(predict(rf, newdata = RaritaTeam2029, type = 'prob'))$X1

#------2030 Rarita win probability------
set.seed(0)
RaritaTeam2030<-tstats2030 %>% filter(Nation=='Rarita')
RaritaTeam2030 <- RaritaTeam2030 %>% select(-c("Player","Nation","Pos","Year"))
RaritaTeam2030 <- data.frame(colMeans(RaritaTeam2030, na.rm=T))
RaritaTeam2030 <- data.frame(t(RaritaTeam2030))
rownames(RaritaTeam2030)<-'Rarita'
win.prob2030 <- data.frame(predict(rf, newdata = RaritaTeam2030, type = 'prob'))$X1

#------2031 Rarita win probability------
set.seed(0)
RaritaTeam2031<-tstats2031 %>% filter(Nation=='Rarita')
RaritaTeam2031 <- RaritaTeam2031 %>% select(-c("Player","Nation","Pos","Year"))
RaritaTeam2031 <- data.frame(colMeans(RaritaTeam2031, na.rm=T))
RaritaTeam2031 <- data.frame(t(RaritaTeam2031))
rownames(RaritaTeam2031)<-'Rarita'
win.prob2031 <- data.frame(predict(rf, newdata = RaritaTeam2031, type = 'prob'))$X1

#------2032 Rarita win probability------
set.seed(0)
RaritaTeam2032<-tstats2032 %>% filter(Nation=='Rarita')
RaritaTeam2032 <- RaritaTeam2032 %>% select(-c("Player","Nation","Pos","Year"))
RaritaTeam2032 <- data.frame(colMeans(RaritaTeam2032, na.rm=T))
RaritaTeam2032 <- data.frame(t(RaritaTeam2032))
rownames(RaritaTeam2032)<-'Rarita'
win.prob2032 <- data.frame(predict(rf, newdata = RaritaTeam2032, type = 'prob'))$X1

#-----Probabilities for next 10 years----
set.seed(0)
prob10years <- cbind(win.prob2022,win.prob2023,win.prob2024,win.prob2025,win.prob2026,
                     win.prob2027,win.prob2028,win.prob2029,win.prob2030,win.prob2031,win.prob2032)
yay2 <- data.frame(years, t(prob10years))
par(mar=c(5.1, 4.1, 4.1, 9.1), xpd=FALSE)
matplot(yay2[,-1], ann=TRUE, lty=6, type='l', lwd=2, col='deepskyblue',
        xaxt = "n",  xlab = 'Year', ylab = 'Probability',main = 'Probability of Winning Tournament', las=2)
grid()
axis(1, at=1:11, labels = years)
legend("topleft", legend="Rarita", lty=1, cex=0.8, col = c('deepskyblue'))



#------------Player Salary---------

Raritasalary <-  salary2021 %>% filter(Country == 'Rarita') %>% filter(`Player Name` %in% teamlist)
                            
                            
                        
Totalsalary <- sum(Raritasalary$`Annualized Salary`)