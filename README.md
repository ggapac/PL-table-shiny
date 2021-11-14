# Premier League standings 2021
#### Entry for [The RStudio Table Contest: 2021 Edition](https://www.rstudio.com/blog/rstudio-table-contest-2021/)

## Description
Sport can be a true gold mine for anyone who likes to play around with data. For this table contest I decided to present the data from the current season of the English [Premier League](https://www.premierleague.com/). I created an interactive Shiny app, which displays the current standings table and allows the user to check each team's previous matches, as well as check the standings in the previous match days of the season. The app is deployed on shinyapps.io and can be accessed [here](https://ggapac.shinyapps.io/PL-table-shiny/).

#### Premier League - background knowledge
If you are not a football* fan, the whole thing might be a bit confusing, but please bear with me! Let's briefly review the logic behind the whole thing. The Premier League (PL) was founded in 1992 and each year consists of 20 top football teams in England. Seasons run from August to May and consist of 38 match days as each team is playing 38 matches in order to play other 19 teams both home and away. During the season the teams are collecting points. A win gets them 3 points, a draw is worth 1 point and a loss means 0 points. But it is not only about the end result! The goals also matter. According to the PL rules the final standings table is sorted by number of points received, goal difference and number of goals scored.

*\* USA people, feel free to replace all the "football" words with "soccer".*

### Functionalities
When generated the table shows standings of the current match day. For each team we can see: 
- position, together with an indicator telling us whether the team's current position is better, equal, or worse than the previous one,
- logo and team name,
- number of matches played,
- goal difference (goals scored subtracted by goals received), where a positive number is green, zero is grey and negative is red,
- number of wins, draws, losses, where the background color is selected based on all values in the column and allows us to quickly see if a team is standing out in any way,
- number of points.  

All rows are expandable. By clicking on the little arrow at the beginning of each row we open a new table that displays all the previous matches of the chosen team. We can see their opponent, whether they played home or away, the final score and the date of the match. At the beginning of each row we also have a little colored circle, which allows us to see whether the team won, lost or drew based on the color (green, red, grey, respectively).

![Expandable rows](/examples/expandable.gif)

The table also allows you to "travel in time", which sounds pretty cool, right? The slider above allows us to pick any of the previous match days and see the league standings back then.

![Slider](/examples/slider.gif)

## Run it yourself
Below you can find the requirements along with the description for running the code in this repository. Please note that our scripts also expect a `_config.R` file in the same folder. In this file we store a variable called `auth_token`, which is our personal API token. For obvious reasons I don't include this file in the repository.

#### Data
In order to get the latest data we use the awesome [Football-Data.org API](https://www.football-data.org/). You need to create an account to get the API token, and *(good news!)* for the purposes of this app a free account is enough.  
The API also provides URLs for team logos, but to avoid constant requests I decided to store those images locally.

#### Requirements
- R version 4.0.4, installed from: [https://www.r-project.org/](https://www.r-project.org/).  
- Libraries:
    - [shiny](https://cran.r-project.org/web/packages/shiny/index.html) version 1.7.1
    - [reactable](https://cran.r-project.org/web/packages/reactable/index.html) version 0.2.3.9000
    - [httr](https://cran.r-project.org/web/packages/httr/index.html) version 1.4.2
    - [htmltools](https://cran.r-project.org/web/packages/htmltools/index.html) version 0.5.2
    - [readr](https://cran.r-project.org/web/packages/readr/index.html) version 2.0.2
    - [shinycssloaders](https://cran.r-project.org/web/packages/shinycssloaders/index.html) version 1.0.0
    - [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html) version 1.0.5


#### Folder structure

    .
    ├── www                 # Folder with PL teams logos                    
    ├── .gitignore                  
    ├── README.md                    	
    ├── _server.R           # Script with functions for the server side of the app
    ├── _ui.R               # Script with functions for the UI and style of the app
    └── app.R               # Shiny app file
