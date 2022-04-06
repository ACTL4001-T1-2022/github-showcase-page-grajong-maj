
![grajong 2](https://user-images.githubusercontent.com/100133925/161904323-616bdd9b-5c21-4daa-bcbc-acc6bd101caf.gif)


> James Wang, Ashkon Mostofi, Yichen Li, Faybian Chow :trollface:
# Objective Analysis :soccer:

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

# Team Selection :adult:

By examining FIFA teams, we found that the most effective team entails a formation that includes 4 defenders, 3 midfielders, 3 forwards and 1 goalkeeper. Since there is a loaning fee for international players, the team only includes the best players from the Raritan nation due to financial reasons. As per FIFA 2022 World Cup regulations, a squad of **23** must be chosen with **3 goalkeepers**. An extra substitute for all other 10 positions is also included to round out the 23-player squad. To choose the best players in each role, a wide variety of modelling techniques were implemented. These include linear regression, lasso regression, regression trees, bagging, random forest and boosting. 

After comparing all the models by examining their mean-squared-error, the best model was chosen for each statistic (defending, passing, shooting, goalkeeping) and implemented to rank the players. Players are given different points based on how well they performed in each statistic, and they are ranked accordingly, and the highest-rated players are selected for the Raritan Team. 


 use <br /> to make a new line  in a table





| Position | Number of players (Main/Substitute) | Player Statistic(s) included | Weighting Split | Statistic(s) to Measure |
| :---: | :---: | :---: | :---: | :---: |
| Goalkeeper | 1/2	| Goalkeeping	| 100% Goalkeeping |	Save % |
| Forward	| 3/3	| Shooting	| 100% Shooting	 | Goals per shot-on-target |
| Midfielder | 3/3 | Shooting <br /> Defending <br /> Passing | 30% Shooting <br /> 30% Defending <br /> 40% Passing | Goals per shot-on-target <br /> Dribblers tackled % <br /> Pressures % <br /> Total pass completion % |
| Defender | 4/4 | Defending <br /> Passing | 75% Defending, <br /> 25% Passing | Pressures % <br /> Total pass completion % |



## National Team :pirate_flag:

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

# Assumptions
## Key Team Assumptions
- Team formation would be 3 forwards, 3 midfielders, 4 defenders and 1 goalkeeper.
- The players would be playing at a similar level relative to their 2020 and 2021 statistics.
- Squad of 23 in total as per FIFA 2022 world cup regulations. This includes a required 3 goalkeepers.
- Players would peak at 30 and start to decline afterwards. Retirement was not considered. When the players reached 35, the average age of retirement (Waihenya, 2021), a larger decline in the players' abilities were accounted for.

## Other Assumptions
- Negative values in the data set did not make statistical sense (negative wins, goals, etc.). Took absolute values of all negative data values.
- Percentage value columns recalculated as they had missing values and they would also give a more accurate value after recalculation. The formulas were found by referencing the data dictionary provided in the raw player data.
- After calculation of the percentage variables, outliers were considered to be those over 105% and removed from the dataset (it is set at 105% due to possiblities of rounding error).
-  No differentiation in the traits that should be implemented for the calculation of a players' ranking for players with different positions but the same role (ie. full-back and centre-back defenders used the same statistics for calculation).
-  Timeline for the implementation plan is built under the assumption that tournaments are held in November.


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

