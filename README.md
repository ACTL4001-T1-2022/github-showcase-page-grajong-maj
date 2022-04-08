<p align="center">
  <img src="https://user-images.githubusercontent.com/100133925/161915139-a3d910ab-104f-409f-9f7d-6b76d3745ef7.gif" />
</p>


James Wang, Ashkon Mostofi, Yichen Li, Faybian Chow :trollface:

---

# Objective Analysis :computer:

This study aims to find the best Raritan soccer team from the combination of players available. We will be selecting and establishing a competitive team that will rank within the top ten nations of the FSA within the next five years, as well as a high probability of achieving an FSA championship within the next ten years. Utilising regression and other modelling techniques, the players were ranked position-wise and chosen for the team. Whilst creating the team, there were data limitations and assumptions which are highlighted throughout the report. The economic impacts of the chosen team is examined, including revenue and expenses on Rarita. A risk analysis on Rarita regarding its economic, political, and reputational status is also provided. Finally, there is an implementation plan with key metrics to support and monitor the progress of the proposed team. 


<img src="https://user-images.githubusercontent.com/100133925/161908801-c0bca66d-0d7b-4eb0-a368-cbfa7d98c699.gif" width = "100">
---

## Required packages

The entire R code used for this case challenge can be obtained here(hyperlink)

---

## Data Cleaning


---

## Modelling 

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

### Regression Trees

### Random Forests

### Bagging

### Boosting

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

By examining FIFA teams, we found that the most effective team entails a formation that includes 4 defenders, 3 midfielders, 3 forwards and 1 goalkeeper. Since there is a loaning fee for international players, the team only includes the best players from the Raritan nation due to financial reasons. As per FIFA 2022 World Cup regulations, a squad of **23** must be chosen with **3 goalkeepers**. An extra substitute for all other 10 positions is also included to round out the 23-player squad. To choose the best players in each role, a wide variety of modelling techniques were implemented. These include linear regression, lasso regression, regression trees, bagging, random forest and boosting. 

After comparing all the models by examining their mean-squared-error, the best model was chosen for each statistic (defending, passing, shooting, goalkeeping) and implemented to rank the players. Players are given different points based on how well they performed in each statistic, and they are ranked accordingly, and the highest-rated players are selected for the Raritan Team. 


 use <br /> to make a new line  in a table





| Position | Number of players (Main/Substitute) | Player Statistic(s) included | Weighting Split | Statistic(s) to Measure |
| :---: | :---: | :---: | :---: | :---: |
| Goalkeeper | 1/2	| Goalkeeping	| 100% Goalkeeping |	Save % |
| Forward	| 3/3	| Shooting	| 100% Shooting	 | Goals per shot-on-target |
| Midfielder | 3/3 | Shooting <br /> Defending <br /> Passing | 30% Shooting <br /> 30% Defending <br /> 40% Passing | Goals per shot-on-target <br /> Dribblers tackled % <br /> Pressures % <br /> Total pass completion % |
| Defender | 4/4 | Defending <br /> Passing | 75% Defending, <br /> 25% Passing | Pressures % <br /> Total pass completion % |



## National Team 🏴:

After selecting the best predictive model, the following table displays the squad of **23** chosen for the Raritan National Team. This is the squad that will suit up for the international tournament. 

| Player Name | Age | Annual Salary (∂) | Position | Starter/Substitute |
| :---:  | :---:  | :---:  | :---:  | :---:  |
| B. Ampofo | 32 | ∂ 3,840,000 | Goalkeeper | Starter |
| A. Omar | 33 | ∂ 7,320,000 | Goalkeeper | Substitute |
| F. Ithungu | 28 | ∂ 1,530,000 | Goalkeeper | Substitute |
| L. Mandala | 30 | ∂ 7,220,000 | Forward | Starter |
| L. Tarigan | 31 | ∂ 6,400,000 | Forward | Starter |
| H. Vos | 26 | ∂ 4,940,000 | Forward | Starter |
| D. Lehner | 32 | ∂ 6,730,000 | Forward | Substitute |
| N. Yamashita | 19 | ∂ 8,270,000 | Forward | Substitute |
| Z. Zziwa | 23 | ∂ 9,120,000 | Forward | Substitute |
| O. Tshuma | 25 | ∂ 28,050,000 | Midfielder | Starter |
| P. Villa | 20 | ∂ 7,820,000 | Midfielder | Starter |
| O. Wanjala | 23 | ∂ 1,750,000 | Midfielder | Starter |
| F. Chin | 22 | ∂ 1,340,000 | Midfielder | Substitute |
| G. Namuganza | 31 | ∂ 29,660,000 | Midfielder | Substitute |
| E. Nakanjako | 20 | ∂ 10,750,000 | Midfielder | Substitute |
| H. Zare | 29 | ∂ 15,430,000 | Defender | Starter |
| N. Terzi | 22 | ∂ 5,000,000 | Defender | Starter |
| C. Tukamushaba | 25 | ∂ 970,000 | Defender | Starter |
| W. Yeoh | 29 | ∂ 560,000 | Defender | Starter |
| T. Okoro | 26 | ∂ 730,000 | Defender | Substitute |
| M. Ludwig | 32 | ∂ 11,910,000 | Defender | Substitute |
| T. Larsson | 20 | ∂ 1,140,000 | Defender | Substitute |
| H. Azizi | 22 | ∂ 5,870,000 | Defender | Substitute |

---

# Team Competitiveness 

![image](https://user-images.githubusercontent.com/100133925/162393272-6a2828cc-2206-4eb9-a5ba-550985007fef.png)

![image](https://user-images.githubusercontent.com/100133925/162393161-d256d08d-34a3-49de-8789-83a7f0831883.png)

---
# Economic Impact 
![giphy](https://user-images.githubusercontent.com/100133925/161907758-35255d7b-4bf2-4e00-8092-d57d08d45568.gif)

---
# Implementation Plan 

---
# Assumptions
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
Since we are looking at building a national football team with the goal of achieving a positive economic impact, there are several risks we need to consider reducing the potential exposure to losses. These risks may have an economic or a reputational impact which will affect Rarita’s global influence and in turn, their ability to grow and develop as a nation. The table below describes the potential risks and risk mitigation techniques.

| Risk | Description | Mitigation Techniques |
| :---:  | :---:  | :---:  |
| Performance Risk | The actual performance of the selected team may deviate from their expected performance. This may lead to increased financing and/or a negative impact on Rarita’s reputation. | For the players that are underperforming in the season or match, we can substitute another player in. <br /> <br /> Continued underperformance will result in a layoff. |
| People Risk | Human element which includes everyone involved. Individual behaviour is unpredictable. <br /> <br /> E.g., if a player is caught doping. | Provide all personnel involved with detailed materials that promote fair play and honest behaviour. <br /> <br /> Develop and implement codes of conduct and discipline policies for the players and staff. |
| Physical Risk | Injury to a player or players which may lead to the team dropping out of the tournament. <br /> <br /> Linked to financial risk in which the medical compensation comes out of our pocket. | Select substitutes for the Raritan national team. <br /> <br /> Plan and organise safety procedures or action plans in the event of injury. <br /> <br /> Transfer part of the financial risk through medical insurance. |
| Currency Risk | Conversion rate from Euros to Doubloons. | If possible, conduct all transactions only in Doubloons (∂). |
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

# Works Cited (☞ﾟヮﾟ)☞ ☜(ﾟヮﾟ☜)

Central midfielder - the perfect all-rounder. planet.training. (n.d.). Retrieved March 25, 2022, from https://planet.training/central-midfielder-the-perfect-all-rounder

Cook, J. (2018, June 13). World Cup Special: Does a volatile currency make for an exciting football team? WorldFirst UK Blog. Retrieved March 25, 2022, from https://www.worldfirst.com/uk/blog/events/world-cup-special-does-a-volatile-currency-make-for-an-exciting-football-team/

Corbett, R. (2002, August 23). Risk management for sport organizations and sport facilities. Sport Law. Retrieved March 25, 2022, from https://sportlaw.ca/risk-management-for-sport-organizations-and-sport-facilities/

Dendir, S. (2016, October 20). (PDF) when Do soccer players peak? A note - researchgate. ResearchGate. Retrieved March 26, 2022, from https://www.researchgate.net/publication/309367548_When_do_soccer_players_peak_A_note

Doidge, M., Claus, R., Gabler, J., Irving, R., Millward, P., Silvério, J. (2019, June 4). The impact of international football events on local, national and transnational fan cultures: a critical overview. Taylor & Francis Online. Retrieved March 27, 2022, from https://www.tandfonline.com/doi/full/10.1080/14660970.2019.1616264 

Gavião, L. O., Gavião, E. V., Sant’Anna, A. P., Lima, G. B. A., & Garcia, P. A. de A. (2021, July 19). Performance analysis of professional soccer goalkeepers by composition of probabilistic preferences. Revista Brasileira de Ciências do Esporte. Retrieved March 25, 2022, from https://www.scielo.br/j/rbce/a/7Fsdbn5SNkJQgWfSjvSYWfg/?lang=en

Gelade, G. (2014, May 23). Evaluating the ability of goalkeepers in English premier ... ResearchGate. Retrieved March 25, 2022, from https://www.researchgate.net/publication/270258236_Evaluating_the_ability_of_goalkeepers_in_English_Premier_League_football

James, G, Witten, D, Hastie, T, Tibshirani, R (2013), “An Introduction to Statistical Learning with Applications in R”, Springer Texts In Statistics, accessed via Perusall on 27 March 2022

Kalén, A., Rey, E., de Rellán-Guerra, A. S., & Lago-Peñas, C. (2019, January 28). Are soccer players older now than before? aging trends and market value in the last three decades of the UEFA champions league. Frontiers. Retrieved March 25, 2022, from https://www.frontiersin.org/articles/10.3389/fpsyg.2019.00076/full

Singh, B. D. (2020, August 14). Buying a soccer team: A machine learning approach. Medium. Retrieved March 25, 2022, from https://towardsdatascience.com/buying-a-soccer-team-a-machine-learning-approach-283f51d52511

UEFA. (2020, October 1). Regulations FIFA World Cup 2022™ - UEFA. FIFA. Retrieved March 25, 2022, from https://editorial.uefa.com/resources/0263-10f46ad3158d-d735f61afd27-1000/fifa_world_cup_2022_preliminary_competition_regulations-covid19_1_.pdf

Waihenya, S. (2021, September 4). What age do soccer players retire at? (quick read). Soccer Whizz. Retrieved March 26, 2022, from https://soccerwhizz.com/age-soccer-players-retire/

---

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

