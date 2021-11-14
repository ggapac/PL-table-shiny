source("_config.R", local = TRUE)

## Functions -------------------------------------------------------------------

get_teams_matches <- function(api_url) {
    #' Get teams' matches
    #'
    #' @description Make an API call to get all the teams' matches in season
    #' 2021. Take only unfinished matches and store the basic information about
    #' the match for each team.
    #' 
    #' @param api_url General api url.
    #' 
    #' @return Named list of data frames, where names are PL team names. Each
    #' data frame stores past matches for a particular team: date, home/away
    #' team, goals scored/received, final result.
    
    matches_api <- GET(paste0(api_url, "matches?season=2021"),
                       add_headers("X-Auth-Token" = auth_token)
    )
    
    matches <- content(matches_api)$matches
    team_matches <- list()
    
    for (i in 1:length(matches)) {
        tmp_match <- matches[[i]]
        
        if (tmp_match$status != "FINISHED") next
        
        winner <- tmp_match$score$winner
        result_home <- result_away <- "draw"
        if (winner == "HOME_TEAM") {
            result_home <- "win"
            result_away <- "lose"
        } else if (winner == "AWAY_TEAM") {
            result_home <- "lose"
            result_away <- "win"
        }
        
        home_team_goals <- tmp_match$score$fullTime$homeTeam
        away_team_goals <- tmp_match$score$fullTime$awayTeam
        
        match_df <- data.frame("result" = "",
                               "home" = tmp_match$homeTeam$name,
                               "score" = paste0(home_team_goals,
                                                " - ",
                                                away_team_goals),
                               "away" = tmp_match$awayTeam$name,
                               "date" = format(parse_datetime(tmp_match$utcDate),
                                               "%d.%m.%Y"))
        
        match_df$result <- result_home
        match_df$goals_scored <- home_team_goals
        match_df$goals_received <- away_team_goals
        team_matches[[tmp_match$homeTeam$name]] <-
            rbind(match_df, team_matches[[tmp_match$homeTeam$name]])
        
        match_df$result <- result_away
        match_df$goals_scored <- away_team_goals
        match_df$goals_received <- home_team_goals
        team_matches[[tmp_match$awayTeam$name]] <-
            rbind(match_df, team_matches[[tmp_match$awayTeam$name]])
    }
    
    team_matches
}

get_all_standings_tables <- function(team_matches, cur_match_day) {
    #' Get standings tables
    #'
    #' @description Calculate standings tables for the past and the
    #' current match days.
    #' 
    #' @param team_matches Named list storing data frames of each team's
    #' matches.
    #' @param cur_match_day Integer, current match day. Value from 1 to 38.
    #' 
    #' @return List of data frames (standing tables), where the list index
    #' corresponds to the match day of the standings table it contains.
    
    all_standings <- list()
    for(i in 1:cur_match_day) {
        all_standings[[i]] <- build_standings_table(team_matches, i,
                                                    all_standings)
    }
    all_standings
}

build_standings_table <- function(team_matches, match_day, all_standings) {
    #' Build standings table
    #'
    #' @description Build a standings table for a particular match day.
    #' Calculate number of points for each team, their goal difference, number
    #' of wins/loses/draws, number of matches played. Also compare their current
    #' position with the previous one (if it exists). Sort the table first by
    #' the number of points, then by goal difference (PL sorting rules:
    #' https://www.premierleague.com/premier-league-explained).
    #' 
    #' @param team_matches Named list storing data frames of each team's
    #' matches.
    #' @param match_day Integer, match day. Value from 1 to 38.
    #' @param all_standings List of data frames (standing tables), where the
    #' list index corresponds to the match day of the standings table it
    #' contains.
    #' 
    #' @return List of data frames (standing tables), where the list index
    #' corresponds to the match day of the standings table it contains.
    
    PL_table <- NULL
    
    for(team in names(team_matches)) {
        tmp <- tail(team_matches[[team]], match_day)
        
        # Get number of wins, loses, draws
        tmp$result <- factor(tmp$result, levels = c("win", "lose", "draw"))
        results <- table(tmp$result)
        
        PL_table <- rbind(PL_table,
                          data.frame(position = NA,
                                     team = team,
                                     played = sum(results),
                                     goal_diff = sum(tmp$goals_scored) -
                                         sum(tmp$goals_received),
                                     goals_scored = sum(tmp$goals_scored),
                                     won = unname(results[1]),
                                     draw = unname(results[3]),
                                     lost = unname(results[2]),
                                     points = calculate_pts(results)))
        
    }
    
    # Sort according to PL rules, get position
    PL_table <- PL_table[with(PL_table,
                              order(-points, -goal_diff, -goals_scored)), ]
    PL_table$position <- 1:nrow(PL_table)
    
    
    if (match_day == 1) {
        # On the first match day there is no previous positions
        prev_table <- NULL
    } else if (max(PL_table$played) != match_day & match_day != 2) {
        # If there were no matches played in the current match day just yet,
        # we compare to two match days before
        prev_table <- all_standings[[match_day - 2]]
    } else {
        # Previous positions are stored in the standings table of the previous
        # match day
        prev_table <- all_standings[[match_day - 1]]
    }
    PL_table$prev <- compare_with_prev_position(PL_table$team, prev_table)
    
    PL_table
}

calculate_pts <- function(results) {
    #' Calculate team points
    #'
    #' @description Calculate team points based on the results.
    #' 
    #' @param results Named vector with number of draws, losses and wins - in
    #' that particular order.
    #' 
    #' @return Number of points.
    
    unname(results[3] * 1 + results[1] * 3)
}

scale_vals <- function(val, min_val, max_val) {
    #' Scale value
    #'
    #' @description Scale value: (val - min_val) / (max_val - min_val).
    #' 
    #' @param val Value of interest.
    #' @param min_val Minimum value.
    #' @param max_val Maximum value.
    #' 
    #' @return Scaled value.
    
    return ((val - min_val) / (max_val - min_val))
}

stats_col <- function(min_val, max_val, cell_class,
                      maxWidth = 50, align = "center", ...) {
    #' Result stats column
    #'
    #' @description Create column definition for the result stats columns -
    #' number of wins, draws, losses.
    #' 
    #' @param min_val Minimum value.
    #' @param max_val Maximum value.
    #' @param cell_cass Value of interest.
    #' @param maxWidth Maximum width of column.
    #' @param align Column alignment, e.g. "center".
    #' @param ... Other arguments for colDef.
    #' 
    #' @return Generated column definition.
    
    colDef(maxWidth = maxWidth,
           align = align, 
           cell = function(value) {
               scaled <- scale_vals(value,
                                    min_val,
                                    max_val)
               
               div(class = cell_class,
                   style = stats_col_style(scaled),
                   value)
           }, ...)
}

sub_table <- function(ix, match_day, team_matches, PL_table_df) {
    #' Create sub table
    #'
    #' @description Create a table of matches for a team given a standings
    #' table of the chosen match day.
    #' 
    #' @param ix Index/team position of the team in interest.
    #' @param match_day Current match day, integer from 1 to 38.
    #' @param team_matches Named list storing data frames of each team's
    #' matches.
    #' @param PL_table_df Data frame, the standings table.
    #' 
    #' @return Generated column definition.
    
    team_name <- PL_table_df$team[ix]
    
    # Get team history given a match day of interest
    team_hist <- team_matches[[team_name]]
    dat <- tail(team_hist, match_day)
    dat$goals_scored <- NULL
    dat$goals_received <- NULL
    
    div(style = "padding: 16px",
        reactable(dat,
                  defaultColDef = colDef(align = "center",
                                         style = list(fontSize = 12),
                                         headerStyle = list(fontSize=12)),
                  columns = list(
                      home =
                          colDef(name = "HOME",
                                 style = function (value) {
                                     teamname_style(value, team_name)}),
                      away =
                          colDef(name = "AWAY",
                                 style = function (value) {
                                     teamname_style(value, team_name)}),
                      date =
                          colDef(name = " ",
                                 style = list(fontSize = 10, color = "#656565"),
                                 maxWidth = 70),
                      score = 
                          colDef(name = "SCORE",
                                 style = list(fontWeight = "bold",
                                              fontSize = 12)),
                      result =
                          colDef(name = " ",
                                 maxWidth = 30,
                                 cell = function(value, index) {
                                     badge <- res_indicator(dat$result[index])
                                     tagList(badge)
                                     }),
                      position =
                          colDef(name = "POS", maxWidth = 50)),
                  theme = reactableTheme(
                      backgroundColor = sub_table_background),
                  sortable = FALSE,
                  outlined = TRUE,
                  showPageInfo = FALSE,
                  showPageSizeOptions = TRUE,
                  defaultPageSize = 10))
    
}

compare_with_prev_position <- function(ordered_teams, prev_table) {
    #' Compare current team position with previous
    #'
    #' @description Given the current order of teams and the previous
    #' standings table calculate a vector telling us whether the team's
    #' current position is better, equal, or worse than the previous one.
    #' 
    #' @param ordered_teams Vector of team names, ordered by position.
    #' @param prev_table Data frame, the previous standings table.
    #' 
    #' @return Position comparison vector with 3 possible values: "up", "down",
    #' "same".
    
    compared_pos <- rep("same", length(ordered_teams))
    
    if (is.null(prev_table)) return(compared_pos)
    
    get_pos <- function(team_name) {
        return(which(prev_table$team == team_name))
    }
    
    prev_positions <- sapply(ordered_teams, get_pos)
    cur_pos <- 1:length(ordered_teams)
    
    compared_pos[prev_positions > cur_pos] <- "down"
    compared_pos[prev_positions < cur_pos] <- "up"
    
    compared_pos
} 
