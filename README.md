

James Wang, Ashkon Mostofi, Yichen Li, Faybian Chow :trollface:

---

# Objective Analysis :computer:

This study aims to find the best Raritan soccer team from the combination of players available. We will be selecting and establishing a competitive team that will rank within the top ten nations of the FSA within the next five years, as well as a high probability of achieving an FSA championship within the next ten years. Utilising regression and other modelling techniques, the players were ranked position-wise and chosen for the team. Whilst creating the team, there were data limitations and assumptions which are highlighted throughout the report. The economic impacts of the chosen team is examined, including revenue and expenses on Rarita. A risk analysis on Rarita regarding its economic, political, and reputational status is also provided. Finally, there is an implementation plan with key metrics to support and monitor the progress of the proposed team. 


<img src="https://user-images.githubusercontent.com/100133925/161908801-c0bca66d-0d7b-4eb0-a368-cbfa7d98c699.gif" width = "100">

---

## Required packages

```{r}
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

```
![amazon-delivery](https://user-images.githubusercontent.com/100133925/162554254-2893de6c-70ac-40c6-b21d-e58f0b264043.gif)

The entire R code used for this case challenge can be obtained [here](assignment%20code.R).

---

## Data Cleaning

<img src="https://user-images.githubusercontent.com/100133925/162569542-4428cec3-a430-4716-939d-101a08172e4d.gif" width = "200">

The following summarises our data cleaning process for all data sets prior to analysis:
- Replace NA in 'Social Media' data.
- Remove 'Eastern Sleboube' from revenue and expenses due to missing expense data.
- Take the absolute value of all variables excluding 'Player', 'Nation', 'Pos', 'Squad', and 'League' for all player data sets. The R-code for league goalkeeping data is shown below.

```{r}
temp <- lgoal[c("Player","Nation","Pos","Squad","League")]
lgoal <- abs(lgoal[,!names(lgoal) %in% c("Player","Nation","Pos","Squad","League")])
lgoal <- data.frame(temp,lgoal)
```

The following changes in player data include the league and tournament data sets.
### Shooting Data
- Remove certain variables including 'Standard Dist', 'Standard FK', 'Expected xG', 'Expected npxG', 'Expected npxG/Sh', 'Expected G-xG', 'Expected np:G-xG' from shooting data due to excessive amounts of missing values.
- Remove outlier entries (considered to be those entries >1.05, as they are no longer due to rounding errors) for 'Standard SoT%'.
- Remove any remaining NA values in 'Standard SoT%', 'Standard G/Sh', and 'Standard G/SoT'.

### Passing Data
- Recalculate 'Total Cmp%', 'Short Cmp%', 'Medium Cmp%','Long Cmp%' and 'A-xA'
- Remove outlier entries (same condition as aobve) for the recalculated variables excluding 'A-xA'.

### Defense Data
- Recalculate 'Vs Dribbles Tkl%' and 'Pressures %'
- Remove outlier entries (same condition as aobve) for the recalculated variables.

### Goalkeeping Data
- Recalculate 'Performance Save%' and 'Performance CS%
- Remove outlier entries (same condition as aobve) for the recalculated variables.

### Correlation Matrix
Duplicate league and tournament data sets for shooting, passing, defense, and goalkeeping were created to maintain the original version for later use. The duplicate datasets were modified to visualise the correlation between the variables themselves, but also to visualise the correlation between the variables and tournament placement.
- Tournament placement was added to the tournament data sets for the corresponding years (2020 and 2021).
- Replace any NA in 'Tournament Place' with 25, assuming that NA means the team did not qualify so they placed last.

An example of the R-code for correlation visualisation using goalkeeping data is shown below

```{r}
tgoal[ ,'Tournament Place']<-NA
tgoal2020<- tgoal %>% filter(Year==2020)
tgoal2021<- tgoal %>% filter(Year==2021)

tgoal2020$`Tournament Place` = rank2020$`2020 Tournament Place`[match(tgoal2020$Nation, rank2020$Country)]
tgoal2021$`Tournament Place` = rank2021$`2021 Tournament Place`[match(tgoal2021$Nation, rank2021$Country)]

tgoalkek <-rbind(tgoal2020, tgoal2021)
tgoalkek$`Tournament Place`[is.na(tgoalkek$`Tournament Place`)]<-25

correlation<-cor(tgoalkek[,unlist(lapply(tgoalkek, is.numeric))])
ggcorrplot(correlation[,23:1])
```

### Further Cleaning

<img src="https://user-images.githubusercontent.com/100197722/162580875-2cb1d98c-3785-4287-9f44-0e11ddfff41c.gif" width = "200">

- The original tournament and league data sets were combined for shooting, passing, defense, and goalkeeping.
- Since the defenders were selected based on their ability to pass and defend (refer to the table in Team Selection section), a new data set was created by merging the passing and defending data sets.
- This data set was then filtered for any player that can perform in the defender position.
- Similarly, the midfielders were selected based on their ability to shoot, defend, and pass, so a new data set was created by merging the passing, defending, and shooting data sets.
- This data set was then filtered for any player that can perform in the midfielder position.

At this point, there are four data sets, combined (tournament and league) data sets for shooting and goalkeeping, and the two data sets described above.

### Selecting Variables for Modelling
- Variables were selected from each of the four data sets based on the correlation matrix.
- The R-code for variable selection using goalkeeping data is shown below.

```{r}
cgoalmodel <- cgoal %>%
  select(Age,Playing.Time.Min,Performance.GA,Performance.SoTA,Performance.Saves,Performance.Save.,W,D,L,
         Performance.CS.,Penalty.Kicks.PKA,Penalty.Kicks.PKsv,Penalty.Kicks.PKm)
```

### Train and Test Data Split
- The final data sets were filtered by year. 2020 was used for the training set and 2021 was used for the test set excluding Raritan players.
- Another data set was created for the Raritan players. This data set was used to predict the performance of each Raritan player after selecting the final model.

---

# Modelling 

<img src="https://user-images.githubusercontent.com/100133925/162569585-4a4a04b6-515d-476a-82de-df682d5d19c3.gif" width ="200">


Several models were used and to demonstrate the modelling process, we will look at goalkeepers specifically. Linear regression, lasso regression, regression trees, random forests, bagging and boosting. 

### Linear Regression
Multiple linear regression describes the relationship between a response variable and multiple predictor variables. The regression is performed on the football player data to predict which player in each position is the most effective. The other statistics except the one chosen to evaluate player talent, is used as the predictors. 
```{r}
  #Regression
regressiongoal <- lm(Performance.Save. ~., data = trainset)
summary(regressiongoal)
    #Regression MSE calculation
regpredictgoal <- predict(regressiongoal, testset)
actualgoal <- testset$Performance.Save.
MSEreg<-mean((actualgoal-regpredictgoal)^2)
```



### Lasso Regression

The lasso is an extension on regression models which utilises a shrinkage penalty to tend coefficients towards zero. Therefore, lasso performs variable selection because predictor variables are ignored due to the coefficient estimate becoming zero. 

```{r} 
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
```

### Regression Trees

The algorithm implemented to build a classification tree is as follows (James et.al, 2013); 
1.	Use recursive binary splitting to grow a tree based on the training data.
2.	Stop this process only when each terminal node has less than some minimum number of observations. 
3.	Prune the large tree to get the best subtrees, as a function of ??.
4.	Apply K-fold cross validation methods to pick the best ??, using a variety of different K-folds.  
5.	Return the subtree that relates to the best value of ?? in step 4. 

```{r} 
  #Regression Tree
treegoal <- rpart(Performance.Save. ~ ., trainset, cp = 0.001, method = "anova")
rpart.plot(treegoal, box.palette = "GnRd", yesno = 2, split.border.col = 1, split.round = 1)
plotcp(treegoal)
    #Regression tree MSE calculation
treepredictgoal<-predict(treegoal, testset)
MSEtree <- mean((actualgoal-treepredictgoal)^2)
```


### Random Forests

Random forests are an extension of regression trees, in which a multitude of individual trees are constructed and work together in union to make a more precise prediction.

```{r} 
  #Random Forest
rfgoal <- randomForest(Performance.Save. ~ ., data = trainset, mtry = sqrt(ncol(trainset)), importance = TRUE)
    #Variable importance for random forest model
vip(rfgoal, num_features = ncol(trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(trainset) - 1)))+
  theme_classic()
    #Random forest MSE calculation
rfpredictgoal<-predict(rfgoal, testset)
MSErf<-mean((actualgoal-rfpredictgoal)^2)
```

### Bagging
Bagging is an extension of the simple classification tree whereby it creates several (B number of them) different bootstrapped training data sets. The model is then trained on the bth bootstrapped set and then averaged to achieve the following equation:
![image](https://user-images.githubusercontent.com/100172902/162569395-506ac8c8-c257-484b-a22f-0b7596b74ada.png)


```{r} 
  #Bagging
baggoal <- randomForest(Performance.Save. ~ ., data = trainset, mtry = (ncol(trainset)-1),importance = TRUE)
    #Variable importance for bagging
vip(baggoal, num_features = ncol(trainset) - 1, aesthetics =
      list(fill = colorRampPalette(rev(brewer.pal(5, "Blues")))(ncol(trainset) - 1)))+
  theme_classic()
    #Bagging MSE calculation
bagpredictgoal <- predict(baggoal, testset)
MSEbag<-mean((actualgoal-bagpredictgoal)^2)
```

### Boosting

The boosting extension of classification trees involves slowly fitting more trees to the current residuals rather than the outcome Y. This addition will update the residuals and gradually make improvements to the model in places where it may not perform well.

```{r} 
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
```

### Comparing Models

| Mean-Squared Error | Value |
| :---: | :---: |
| Linear Regression | 0.006062906 |
| Lasso Regression | 0.01102361 |
| Regression Trees | 0.006640703 |
| Random Forests | 0.006299187 |
| Bagging | 0.003587406|
| Boosting | 0.002787388 |

The table above shows the calculated mean-squared errors for each model on predicting the best goalkeepers and the worst model appeared to be the lasso regression.

<img src="https://user-images.githubusercontent.com/100133925/161882942-fea4f1ae-72ba-41d4-8778-28c4f18e594f.gif" width = "100">

However, boosting had the lowest MSE and therefore was the best predictive model for selecting goalkeepers for the national team. The process was repeated for each position, with slight adjustments. 

---

# Team Selection :soccer:

<img src="https://user-images.githubusercontent.com/100133925/162569750-cfb25f59-c78b-4eae-bbd0-1f523322ed17.gif" width = "100">

By examining FIFA teams, we found that the most effective team entails a formation that includes 4 defenders, 3 midfielders, 3 forwards and 1 goalkeeper. Since there is a loaning fee for international players, the team only includes the best players from the Raritan nation due to financial reasons. As per FIFA 2022 World Cup regulations, a squad of **23** must be chosen with **3 goalkeepers**. An extra substitute for all other 10 positions is also included to round out the 23-player squad. To choose the best players in each role, a wide variety of modelling techniques were implemented. These include linear regression, lasso regression, regression trees, bagging, random forest and boosting. 

After comparing all the models by examining their mean-squared-error, the best model was chosen for each statistic (defending, passing, shooting, goalkeeping) and implemented to rank the players. Players are given different points based on how well they performed in each statistic, and they are ranked accordingly, and the highest-rated players are selected for the Raritan Team. 


 use <br /> to make a new line  in a table





| Position | Number of players (Main/Substitute) | Player Statistic(s) included | Weighting Split | Statistic(s) to Measure |
| :---: | :---: | :---: | :---: | :---: |
| Goalkeeper | 1/2	| Goalkeeping	| 100% Goalkeeping |	Save % |
| Forward	| 3/3	| Shooting	| 100% Shooting	 | Goals per shot-on-target |
| Midfielder | 3/3 | Shooting <br /> Defending <br /> Passing | 30% Shooting <br /> 30% Defending <br /> 40% Passing | Goals per shot-on-target <br /> Dribblers tackled % <br /> Pressures % <br /> Total pass completion % |
| Defender | 4/4 | Defending <br /> Passing | 75% Defending, <br /> 25% Passing | Pressures % <br /> Total pass completion % |



## National Team ???? <img src ="https://user-images.githubusercontent.com/100133925/162569704-910f1c71-8985-4fa5-aa57-20f67cbab79c.gif" width ="50">

After selecting the best predictive model, the following table displays the squad of **23** chosen for the Raritan National Team. This is the squad that will suit up for the international tournament. 

| Player Name | Age | Annual Salary (???) | Position | Starter/Substitute |
| :---:  | :---:  | :---:  | :---:  | :---:  |
| B. Ampofo | 32 | ??? 3,840,000 | Goalkeeper | Starter |
| A. Omar | 33 | ??? 7,320,000 | Goalkeeper | Substitute |
| F. Ithungu | 28 | ??? 1,530,000 | Goalkeeper | Substitute |
| L. Mandala | 30 | ??? 7,220,000 | Forward | Starter |
| L. Tarigan | 31 | ??? 6,400,000 | Forward | Starter |
| H. Vos | 26 | ??? 4,940,000 | Forward | Starter |
| D. Lehner | 32 | ??? 6,730,000 | Forward | Substitute |
| N. Yamashita | 19 | ??? 8,270,000 | Forward | Substitute |
| Z. Zziwa | 23 | ??? 9,120,000 | Forward | Substitute |
| O. Tshuma | 25 | ??? 28,050,000 | Midfielder | Starter |
| P. Villa | 20 | ??? 7,820,000 | Midfielder | Starter |
| O. Wanjala | 23 | ??? 1,750,000 | Midfielder | Starter |
| F. Chin | 22 | ??? 1,340,000 | Midfielder | Substitute |
| G. Namuganza | 31 | ??? 29,660,000 | Midfielder | Substitute |
| E. Nakanjako | 20 | ??? 10,750,000 | Midfielder | Substitute |
| H. Zare | 29 | ??? 15,430,000 | Defender | Starter |
| N. Terzi | 22 | ??? 5,000,000 | Defender | Starter |
| C. Tukamushaba | 25 | ??? 970,000 | Defender | Starter |
| W. Yeoh | 29 | ??? 560,000 | Defender | Starter |
| T. Okoro | 26 | ??? 730,000 | Defender | Substitute |
| M. Ludwig | 32 | ??? 11,910,000 | Defender | Substitute |
| T. Larsson | 20 | ??? 1,140,000 | Defender | Substitute |
| H. Azizi | 22 | ??? 5,870,000 | Defender | Substitute |

---

# Team Competitiveness 

![200w](https://user-images.githubusercontent.com/100133925/162569761-41b8e94f-a08e-4696-84f2-79aa84f780b7.gif)


The tournament place data was appended onto the tournament stats for each team, and several regression models were tested to predict the tournament outcomes for the next ten years. The linear regression model had more variance explained than the regression tree, random forest and bagging models and was therefore chosen to model tournament placements. It is important to note that players may increase or decrease in skill levels each year, so randomness is applied to players??? statistics, which results in an improvement or decline. The average player peak is 30 years old (Dendir, 2016), therefore after the age of 30, they start to decline in ability by 5% per year. The average retirement age for soccer players is 35 (Kalen et.al, 2019) however, an assumption was that retirements were not accounted for. A more drastic decline of 10% was annexed to players aged over 35 to account for the severe decrease in skill beyond age 35.

The graph below displays the projected tournament placements spanning from 2022-2032 for the top 25 nations with the addition of the newly formed Raritan National Team. The selected Raritan Team shows promising signs, with a predicted winning tournament in 2022, 2023, and 2024 which demonstrates the strength of the selected squad. Although the team was initially built for the short-term, it is still predicted to remain competitive for the next 10 years, being constantly in the top 5 rated national teams.

<img src = "https://user-images.githubusercontent.com/100133925/162393161-d256d08d-34a3-49de-8789-83a7f0831883.png" width = "700">

To predict the probability of winning the tournament, tournament results were changed to ???Yes??? for a first-place result and ???No??? for every other result, to build a model to predict the probability of winning the tournament i.e., coming first. The random forest model demonstrated the highest predictive capability and thus was chosen for this assigned task. The figure below shows the time series of the probability of winning the tournament and it can be seen that in 2032, the chance was the highest out of all years. Achieving a 25% predicted probability of winning is quite high considering the logistics involved in a soccer tournament. Only the best teams make it out of group stages to play in the knock-out stages and must win every single match to hoist the trophy.

<img src="https://user-images.githubusercontent.com/100133925/162393272-6a2828cc-2206-4eb9-a5ba-550985007fef.png" width="500">

Through examining the forecasted tournament rankings and the chances of winning, it is evident that the selected Raritan National Soccer Team exemplifies a competitive nature in the present but also for the mid to long-term future.

![true-correct](https://user-images.githubusercontent.com/100133925/162554315-23047a86-386f-4e68-ae6d-4e5120aa4302.gif)


---

# Economic Impact 
<img src="https://user-images.githubusercontent.com/100133925/161907758-35255d7b-4bf2-4e00-8092-d57d08d45568.gif" width = "150">

To assess the impact of the proposed implementation plan on the Raritan economy, GDP, GNI, revenues, expenses and profit/loss were all forecast over the next 10 years using a variety of models. All graphs shown are on a per-capita basis, the profit/loss table gives an overall national value. The dark and light blue ranges give 80 and 95% confidence intervals respectively. Note that multiple models were tested for each forecast, however only the best model is shown here.

The forecast for Raritan GDP was performed using an ARIMA(0,1,0) model with drift. The impact on the GDP of the individual Raritan provinces (East, West and Central) was predicted to be similar in nature to the effect on the overall Raritan GDP. The R-code for this forecast and the produced graph are shown below.

```{r}
#Rarita GDP
  #Changing GDP table to a suitable format for forecasting
rgdp <- gdp %>%
  select(c('Rarita')) #extracting Rarita GDP data from table
rgdpts <- ts(rgdp,frequency = 1,start=2011)

rgdpfit <- auto.arima(rgdpts) #auto ARIMA function fitted an ARIMA(0,1,0) with drift
autoplot(forecast(rgdpfit,h=10)) + 
  ggtitle('Forecasted Raritan GDP from 2021-2030') +
  xlab('Year') +
  ylab('GDP') +
  theme_bw()

```
![image](https://user-images.githubusercontent.com/63340904/162554875-41cdaa78-dc92-43ad-adad-5f2238a76287.png)

The code for the provincial forecasts was similar in nature to the above code, with the exception of Central Rarita which was forecast using an ETS(AAN) model.

Raritan GNI was forecast in a similar manner to GDP, however here an ETS(AAN) model was used. Similar benefits were forecast for the individual provinces. Relevant code and graph are provided below.

```{r}
#Rarita GNI
  #Changing GNI table to a suitable format for forecasting
rgni <- gni %>%
  select(c('Rarita')) #extracting Rarita GNI data from table
rgnits2 <- ts(rgni,frequency=1,start=2011)

rgnifit2 <- ets(rgnits2,model='AAN')
autoplot(forecast(rgnifit2,h=10)) +
  ggtitle('Forecasted Raritan GNI from 2021-2030')+
  xlab('Year')+
  ylab('GNI')+
  theme_bw()

```
![image](https://user-images.githubusercontent.com/63340904/162555044-399936b2-0931-4ea5-8f95-85c8ebb91b05.png)

Ten-year forecasts for Raritan football assocation revenue and expenses were both calculated. Confidence intervals here showed much greater variance than the forecasted economic indices. This is likely due to the extreme potential for variance in a team's performance internationally which may impact forecasted revenues and expenses. However, as previously mentioned, the team is predicted to be competitive and thus these negative predictions are not expected to be realised. Both revenues and expenses were forecasted using ETS(AAN) models, the code and graphs are provided below.

```{r}
#Forecasting Raritan revenue data
raritarevenue <- revenue %>%
  filter(`_Nation` == 'Rarita')   #extracting Raritan data from international revenue table
raritarevenue <- transpose(raritarevenue)
raritarevenue <- as.numeric(raritarevenue[-1,])
raritarevenue <- rev(raritarevenue)

  #Total revenue
totalrevenue <- raritarevenue[16:20]
totalrevenuets <- ts(totalrevenue,frequency = 1, start=2016)

totalrevenuefit2 <- ets(totalrevenuets,model='AAN')
autoplot(forecast(totalrevenuefit2,h=10))+
  ggtitle('Forecasted Raritan revenue from 2021-2030')+
  xlab('Year')+
  ylab('Revenue')+
  theme_bw()

```
![image](https://user-images.githubusercontent.com/63340904/162555059-9cb4021f-95f9-4b47-bda6-06a6e0d7e620.png)

```{r}
#Forecasting Raritan expense data
raritaexp <- expenses %>%
  filter(`_Nation` == 'Rarita')   #extracting Raritan data from international expense table
raritaexp <- transpose(raritaexp)
raritaexp <- as.numeric(raritaexp[-1,])
raritaexp <- rev(raritaexp)

  #Total expenses
totalexp <- raritaexp[11:15]
totalexpts <- ts(totalexp,frequency = 1, start=2016)

totalexpfit2 <- ets(totalexpts,model='AAN')
autoplot(forecast(totalexpfit2,h=10))+
  ggtitle('Forecasted Raritan expenses from 2021-2030')+
  xlab('Year')+
  ylab('Expenses')+
  theme_bw()

```
![image](https://user-images.githubusercontent.com/63340904/162555149-7027d89b-bd3c-4a26-96fc-14b605881d61.png)

Finally, our forecasted revenues and expenses were multiplied by the forecasted population to obtain total profit/loss seen below. The relevant R-code for this process is also shown.

```{r}
#Forecasted profit/loss per capita
frevenue <- forecast(totalrevenuefit2,h=10) #revenue per capita forecast
fexp <- forecast(totalexpfit2,h=10) #expense per capita forecast
profitlosspc <- frevenue$mean - fexp$mean #profit/loss per capita forecast

#Forecasted population
raritapop <- pop %>%
  select(c('Rarita'))
raritapopts <- ts(raritapop,frequency = 1,start=2011)

raritapopfit <- auto.arima(raritapopts)
fpop <- forecast(raritapopfit,h=10)

#Forecasted total annual profit/loss
profitloss <- profitlosspc*fpop$mean

```

| Year | Revenue (per capita) | expenses (per capita) | Population | Profit/Loss |
| :---:  | :---:  | :---:  | :---:  | :---:  |
| 2021 | 183.54 | 151.95 | 12,627,874 | 398,810,805 |
| 2022 | 187.93 | 155.21 | 12,683,786 | 414,939,131 |
| 2023 | 192.32 | 158.48 | 12,739,699 | 431,194,082 |
| 2024 | 196.72 | 161.74 | 12,795,612 | 447,575,660 |
| 2025 | 201.11 | 165.00 | 12,851,525 | 464,083,863 |
| 2026 | 205.50 | 168.26 | 12,907,438 | 480,718,692 |
| 2027 | 209.90 | 171.52 | 12,963,351 | 497,480,147 |
| 2028 | 214.29 | 174.78 | 13,019,263 | 514,368,228 |
| 2029 | 218.69 | 178.05 | 13,075,176 | 531,382,935 |
| 2030 | 223.08 | 181.31 | 13,131,089 | 548,524,267 |

## Effects of Competitive Team
Building a competitive soccer team has major impacts on the economy and can generate vast amounts of revenue. For one, displaying a high level of soccer talent on the international stage will garner attention in the domestic Raritan League. From the SOA data, provided by Valani Global, the 2021 Tournament Place tended to show competitive teams with high league attendance, social media follower or both. For example, the nation of Southern Ristan, placing 6th in 2021, has an average league attendance of 66,984 making it one of the highest, but also has the most Facebook, Twitter, and Instagram followers. Similarly, Nganion, which placed 3rd in 2021, hosts 72,400 spectators on average in their league and has the second-most Facebook, Twitter, and Instagram followers. 

## Additional Economic Impacts/Considerations
To pay for the national team, the Raritan government provided a one-time lump sum of ??? 995,000,000. The use of this initial funding and additional sources of funding are discussed in detail in the implementation plan.

As discussed prior, performing at a high level internationally can have major positive impacts on Rarita. Fielding a strong international squad can garner new attention towards the Raritan Football League, increasing attendance and overall revenue. This increased attention is not limited to the football industry alone, the Raritan tourism industry may see an increase in revenue because of the team's success (Doidge et.al, 2019).

---

# Implementation Plan 
The table for the team members can be found above.

The team comprises of 23 players (11 starters, 12 substitutes) and has a total salary of ???176,350,000 annually. 
The primary source of revenue for the team will be the ???995,000,000 allocation from the government. Given our annual salary will use up this funding over the 10 years, additional sources of funding will come from sponsorships, discussed below, and reinvesting a portion of annual profits into the team.


![image](https://user-images.githubusercontent.com/63340904/162564597-59287a44-3362-487d-aee8-967db3bec310.png)

This proposal is an outline of the timeline for the first year of the team launch. It examines the formation of the team and sources of revenues which includes liaising with sponsors, setting up merchandise for sale to fans and increased social media presence to gain awareness and support for the team. 
For the later 9 years, monitoring individual players/main team progress occurs from January-October since we have assumed that tournaments are held in November. Liaising with sponsors will occur constantly and setting up merchandise will occur from January to October of each year to gain revenue. Meanwhile, social media promotion will always be held 3 months in advance of the tournament for each year.


---

# Assumptions

<img src="https://user-images.githubusercontent.com/100133925/162569816-731aed33-a8b4-4956-bb41-6874b3a33f2a.gif" width="250">

## Key Team Assumptions
- Team formation would be 3 forwards, 3 midfielders, 4 defenders and 1 goalkeeper.
- The players would be playing at a similar level relative to their 2020 and 2021 statistics.
- Squad of 23 in total as per FIFA 2022 world cup regulations. This includes a required 3 goalkeepers.
- Players would peak at 30 and start to decline afterwards. Retirement was not considered. When the players reached 35, the average age of retirement (Waihenya, 2021), a larger decline in the players' abilities were accounted for.

This is how we coded player skill adjustments over years. For example the change from 2021 to 2022 is shown below. 
```{r}
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

```

## Other Assumptions
- Negative values in the data set did not make statistical sense (negative wins, goals, etc.). Took absolute values of all negative data values.
- Percentage value columns recalculated as they had missing values and they would also give a more accurate value after recalculation. The formulas were found by referencing the data dictionary provided in the raw player data.
- After calculation of the percentage variables, outliers were considered to be those over 105% and removed from the dataset (it is set at 105% due to possiblities of rounding error).
-  No differentiation in the traits that should be implemented for the calculation of a players' ranking for players with different positions but the same role (ie. full-back and centre-back defenders used the same statistics for calculation).
-  Timeline for the implementation plan is built under the assumption that tournaments are held in November.

# Data Limitations
- There were several missing values in the percentage variables (Standard SoT%, Total Cmp%, etc.), when possible, these were recalculated.
- Expected-goal-saves for goalkeeping data was not provided, which would have helped remove bias from the save-to-shot ratio metric used to evaluate goalkeeping ability.
- Limited years avaiable for the players (2020 and 2021). Some players had drastic differences between the years.
- All columns relating to expected goals in the tournament shooting data were missing for 2020 data, thus these statistics weren't considered in our models.
- No 2020 data was provided for the tournament passing and defending data sets, therefore the training sets involving passing and defending data only incorporated league values.

![twin-shoot-goal](https://user-images.githubusercontent.com/100133925/161907615-19883cab-446c-4765-aeb1-42aa6b51f87e.gif)


---

# Risk and Risk Mitigation Considerations ![everythings_fine_parrot](https://user-images.githubusercontent.com/100197722/161956038-61909666-1c35-4c38-b912-617cd3c6005e.gif)
Since we are looking at building a national football team with the goal of achieving a positive economic impact, there are several risks we need to consider reducing the potential exposure to losses. These risks may have an economic or a reputational impact which will affect Rarita???s global influence and in turn, their ability to grow and develop as a nation. The table below describes the potential risks and risk mitigation techniques.

| Risk | Description | Mitigation Techniques |
| :---:  | :---:  | :---:  |
| Performance Risk | The actual performance of the selected team may deviate from their expected performance. This may lead to increased financing and/or a negative impact on Rarita???s reputation. | For the players that are underperforming in the season or match, we can substitute another player in. <br /> <br /> Continued underperformance will result in a layoff. |
| People Risk | Human element which includes everyone involved. Individual behaviour is unpredictable. <br /> <br /> E.g., if a player is caught doping. | Provide all personnel involved with detailed materials that promote fair play and honest behaviour. <br /> <br /> Develop and implement codes of conduct and discipline policies for the players and staff. |
| Physical Risk | Injury to a player or players which may lead to the team dropping out of the tournament. <br /> <br /> Linked to financial risk in which the medical compensation comes out of our pocket. | Select substitutes for the Raritan national team. <br /> <br /> Plan and organise safety procedures or action plans in the event of injury. <br /> <br /> Transfer part of the financial risk through medical insurance. |
| Currency Risk | Conversion rate from Euros to Doubloons. | If possible, conduct all transactions only in Doubloons (???). |
| Financial Risk | Costs are unexpectedly high and exceed our financial capacity. | Transfer risk to other entities through contracts, insurance, or waivers. |

This is followed by the risk assessment matrix for the ranking, likelihood of occurrence, financial impact, and potential impact for each risk.

| Risk | Rank | Occurrence/Likelihood | Financial Impact | Reputational Impact |
| :---:  | :---:  | :---:  | :---:  | :---:  |
| Performance Risk | 1 | mid | high | high |
| People Risk | 2 | mid | mid | mid / high |
| Financial Risk | 3 | mid | mid | low / mid |
| Physical Risk | 4 | high | low | low |
| Currency Risk | 5 | low | low | low |

---

# Works Cited (????????????)??? ???(????????????)

Central midfielder - the perfect all-rounder. planet.training. (n.d.). Retrieved March 25, 2022, from https://planet.training/central-midfielder-the-perfect-all-rounder

Cook, J. (2018, June 13). World Cup Special: Does a volatile currency make for an exciting football team? WorldFirst UK Blog. Retrieved March 25, 2022, from https://www.worldfirst.com/uk/blog/events/world-cup-special-does-a-volatile-currency-make-for-an-exciting-football-team/

Corbett, R. (2002, August 23). Risk management for sport organizations and sport facilities. Sport Law. Retrieved March 25, 2022, from https://sportlaw.ca/risk-management-for-sport-organizations-and-sport-facilities/

Dendir, S. (2016, October 20). (PDF) when Do soccer players peak? A note - researchgate. ResearchGate. Retrieved March 26, 2022, from https://www.researchgate.net/publication/309367548_When_do_soccer_players_peak_A_note

Doidge, M., Claus, R., Gabler, J., Irving, R., Millward, P., Silv??rio, J. (2019, June 4). The impact of international football events on local, national and transnational fan cultures: a critical overview. Taylor & Francis Online. Retrieved March 27, 2022, from https://www.tandfonline.com/doi/full/10.1080/14660970.2019.1616264 

Gavi??o, L. O., Gavi??o, E. V., Sant???Anna, A. P., Lima, G. B. A., & Garcia, P. A. de A. (2021, July 19). Performance analysis of professional soccer goalkeepers by composition of probabilistic preferences. Revista Brasileira de Ci??ncias do Esporte. Retrieved March 25, 2022, from https://www.scielo.br/j/rbce/a/7Fsdbn5SNkJQgWfSjvSYWfg/?lang=en

Gelade, G. (2014, May 23). Evaluating the ability of goalkeepers in English premier ... ResearchGate. Retrieved March 25, 2022, from https://www.researchgate.net/publication/270258236_Evaluating_the_ability_of_goalkeepers_in_English_Premier_League_football

James, G, Witten, D, Hastie, T, Tibshirani, R (2013), ???An Introduction to Statistical Learning with Applications in R???, Springer Texts In Statistics, accessed via Perusall on 27 March 2022

Kal??n, A., Rey, E., de Rell??n-Guerra, A. S., & Lago-Pe??as, C. (2019, January 28). Are soccer players older now than before? aging trends and market value in the last three decades of the UEFA champions league. Frontiers. Retrieved March 25, 2022, from https://www.frontiersin.org/articles/10.3389/fpsyg.2019.00076/full

Singh, B. D. (2020, August 14). Buying a soccer team: A machine learning approach. Medium. Retrieved March 25, 2022, from https://towardsdatascience.com/buying-a-soccer-team-a-machine-learning-approach-283f51d52511

UEFA. (2020, October 1). Regulations FIFA World Cup 2022??? - UEFA. FIFA. Retrieved March 25, 2022, from https://editorial.uefa.com/resources/0263-10f46ad3158d-d735f61afd27-1000/fifa_world_cup_2022_preliminary_competition_regulations-covid19_1_.pdf

Waihenya, S. (2021, September 4). What age do soccer players retire at? (quick read). Soccer Whizz. Retrieved March 26, 2022, from https://soccerwhizz.com/age-soccer-players-retire/

---











