
![Untitled](https://user-images.githubusercontent.com/100133925/161897010-6bde2114-3aa7-4917-9f8c-294887aeac92.png)


# Objective Analysis

This study aims to find the best Raritan soccer team from the combination of players available. We will be selecting and establishing a competitive team that will rank within the top ten nations of the FSA within the next five years, as well as a high probability of achieving an FSA championship within the next ten years. Utilising regression and other modelling techniques, the players were ranked position-wise and chosen for the team. Whilst creating the team, there were data limitations and assumptions which are highlighted throughout the report. The economic impacts of the chosen team is examined, including revenue and expenses on Rarita. A risk analysis on Rarita regarding its economic, political, and reputational status is also provided. Finally, there is an implementation plan with key metrics to support and monitor the progress of the proposed team. 


---
![sad-boy-crying](https://user-images.githubusercontent.com/100133925/161882942-fea4f1ae-72ba-41d4-8778-28c4f18e594f.gif)
![sad-boy-crying](https://user-images.githubusercontent.com/100133925/161882942-fea4f1ae-72ba-41d4-8778-28c4f18e594f.gif)
![sad-boy-crying](https://user-images.githubusercontent.com/100133925/161882942-fea4f1ae-72ba-41d4-8778-28c4f18e594f.gif)
![sad-boy-crying](https://user-images.githubusercontent.com/100133925/161882942-fea4f1ae-72ba-41d4-8778-28c4f18e594f.gif)
![sad-boy-crying](https://user-images.githubusercontent.com/100133925/161882942-fea4f1ae-72ba-41d4-8778-28c4f18e594f.gif)
![sad-boy-crying](https://user-images.githubusercontent.com/100133925/161882942-fea4f1ae-72ba-41d4-8778-28c4f18e594f.gif)
![sad-boy-crying](https://user-images.githubusercontent.com/100133925/161882942-fea4f1ae-72ba-41d4-8778-28c4f18e594f.gif)
![sad-boy-crying](https://user-images.githubusercontent.com/100133925/161882942-fea4f1ae-72ba-41d4-8778-28c4f18e594f.gif)

```{r, global_options, include=FALSE}
library(knitr)
library(formatR)
knitr::opts_chunk$set(message=FALSE, tidy.opts=list(width.cutoff=60), tidy=TRUE) 
```

## Preliminary Tasks

### --------Installing and loading required packages-------- 


install.packages(readxl)  
install.packages(dplyr)  
install.packages(ggplot2)  
install.packages(ggcorrplot)  
install.packages(glmnet)  
install.packages(tree)  
install.packages(rpart)  
install.packages(rpart.plot)  
install.packages(randomForest)  
install.packages(gbm)  
install.packages(vip)  
install.packages(RColorBrewer)  
install.packages(forecast)  
install.packages(xts)  
install.packages(data.table)  
install.packages(tidyr)  
install.packages(mice)  

```{r setup, include=FALSE}
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
```
  
    
### --------Setting the working directory and loading in the football related data--------  

```{r, warning=FALSE, results='hide', message=FALSE}


setwd('C:/Users/james/Documents/UNSW/year 4/Term 1/ACTL4001/Assignment/data')

economicdata <- '2022-student-research-case-study-economic-data.xlsx'
playerdata <- '2022-student-research-case-study-player-data.xlsx'
footballdata <- '2022-student-research-case-study-football-soccer-data.xlsx'
#------------------------------------
### -------- READING IN DATA -------- 

  #-----Reading in economic data
gdp <- read_excel(economicdata, sheet = 'Rarita Economic', range = 'B12:F22')
gni <- read_excel(economicdata, sheet = 'Rarita Economic', range = 'H12:L22')
pop <- read_excel(economicdata, sheet = 'Rarita Economic', range = 'B26:F36')
popden <- read_excel(economicdata, sheet = 'Rarita Economic', range = 'H26:L36')
health <- read_excel(economicdata, sheet = 'Rarita Economic', range = 'B40:F50')
housesaving <- read_excel(economicdata, sheet = 'Rarita Economic', range = 'H40:L50')
conversionrate <- read_excel(economicdata, sheet = 'Rarita Economic', range = 'B54:D59')
spotrate <- read_excel(economicdata, sheet = 'Rarita Spot Rates', range = 'B12:P72')
inflationrate <- read_excel(economicdata, sheet = 'Rarita Inflation Rates', 
                            range = 'B11:C41')
globalgdp <- read_excel(economicdata, sheet = 'Other Countries GDP', range = 'B12:G32')


 #-----Reading in player data
lshooting <- read_excel(playerdata, sheet = 'League Shooting', range = 'B12:AA5566')
lpassing <- read_excel(playerdata, sheet = 'League Passing', range = 'B12:AF5566')
ldefense <- read_excel(playerdata, sheet = 'League Defense', range = 'B12:AG5566')
lgoal <- read_excel(playerdata, sheet = 'League Goalkeeping', range = 'B12:AB425')
rank2020 <- read_excel(playerdata, sheet = 'Tournament Results', range = 'B11:C27')
rank2021 <- read_excel(playerdata, sheet = 'Tournament Results', range = 'E11:F35')
tshooting <- read_excel(playerdata, sheet = 'Tournament Shooting', range = 'B12:Z2027', 
                        guess_max = 3000)
tpassing <- read_excel(playerdata, sheet = 'Tournament Passing', range = 'B12:AE500')
tdefense <- read_excel(playerdata, sheet = 'Tournament Defense', range = 'B12:AF500')
tgoal <- read_excel(playerdata, sheet = 'Tournament Goalkeeping', range = 'B12:AA141')
salary2020 <- read_excel(playerdata, sheet = '2020 Salaries', range = 'B12:G2744')
salary2021 <- read_excel(playerdata, sheet = '2021 Salaries', range = 'B12:G2834')
  
  #-----Reading in Football Revenue
revenue <- read_excel(footballdata, sheet = 'Revenue', range = 'B13:V34', 
                      col_names = FALSE)
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

  #-----Reading in Football Expenses
expenses <- read_excel(footballdata, sheet = 'Expense', range = 'B13:Q34', 
                       col_names = FALSE)
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
  
  #-----Reading in Football Media
attendance <- read_excel(footballdata, sheet = 'Attendance', range = 'B13:C34')
media <- read_excel(footballdata, sheet = 'Social Media', range = 'B13:G34')

```
  
  
###-------- Cleaning Data ---------

```{r}

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
lpassing <- abs(lpassing[,!names(lpassing) %in% c("Player","Nation","Pos","Squad",
                                                  "League")])
lpassing <- data.frame(temp2,lpassing)

temp3 <- tpassing[c("Player","Nation","Pos","League")]
tpassing <- abs(tpassing[,!names(tpassing) %in% c("Player","Nation","Pos","League")])
tpassing <- data.frame(temp3,tpassing)

temp5 <- ldefense[c("Player","Nation","Pos","Squad","League")]
ldefense <- abs(ldefense[,!names(ldefense) %in% c("Player","Nation","Pos","Squad",
                                                  "League")])
ldefense <- data.frame(temp5,ldefense)

temp6 <- tdefense[c("Player","Nation","Pos","League")]
tdefense <- abs(tdefense[,!names(tdefense) %in% c("Player","Nation","Pos","League")])
tdefense <- data.frame(temp6,tdefense)

temp_lshooting = lshooting[c("Player", "Nation", "Pos", "Squad", "League")]
lshooting = abs(lshooting[,!names(lshooting) %in% c("Player", "Nation", "Pos", "Squad", 
                                                    "League")])
lshooting = data.frame(temp_lshooting, lshooting)

temp_tshooting = tshooting[c("Player", "Nation", "Pos", "League")]
tshooting = abs(tshooting[,!names(tshooting) %in% c("Player", "Nation", "Pos", "League")])
tshooting = data.frame(temp_tshooting, tshooting)

  #Removing certain columns from shooting data due to excess amounts of missing values
remove = c("Standard.Dist", "Standard.FK", "Expected.xG", "Expected.npxG", "Expected.npxG.Sh",
           "Expected.G.xG", "Expected.np.G.xG")
lshooting = lshooting[, (!names(lshooting) %in% remove)]
tshooting = tshooting[, (!names(tshooting) %in% remove)]

  #Recalculating 'Performance Save%' and 'Performance CS%' for goalkeeping data
tgoal <- tgoal %>%
  mutate(Performance.Save.=Performance.Saves/Performance.SoTA,
         Performance.CS.=Performance.CS/(`W`+`D`+`L`))
lgoal <- lgoal %>%
  mutate(Performance.Save.=Performance.Saves/Performance.SoTA,
         Performance.CS.=Performance.CS/(`W`+`D`+`L`))
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
library(tidyr)
lshooting = lshooting %>%
  drop_na(Standard.SoT., Standard.G.Sh, Standard.G.SoT)

tshooting = tshooting %>%
  drop_na(Standard.SoT., Standard.G.Sh, Standard.G.SoT)
```

### Congrats on completing the [2022 SOA Research Challenge](https://www.soa.org/research/opportunities/2022-student-research-case-study-challenge/)!

>Now it's time to build your own website to showcase your work.  
>To create a website on GitHub Pages to showcase your work is very easy.

This is written in markdown language. 
>
* Click [4001 link](https://classroom.github.com/a/ggiq0YzO) to accept your group assignment.
* Click [5100 link](https://classroom.github.com/a/uVytCqDv) to accept your group assignment 

#### Follow the [guide doc](Doc1.pdf) to submit your work. 
---
>Be creative! Feel free to link to embed your [data](player_data_salaries_2020.csv), [code](sample-data-clean.ipynb), [image](ACC.png) here

More information on GitHub Pages can be found [here](https://pages.github.com/)
![](Actuarial.gif)

