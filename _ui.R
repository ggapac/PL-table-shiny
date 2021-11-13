## Colors ----------------------------------------------------------------------

purple <- "#38003c"
black <- "#000000"

# Spinner
spinner_color <- "#0dc5c1"
spinner_ticks <- "#99a4ac"

# Sub table
win_green <- "#008000"
draw_grey <- "#BEBEBE"
lose_red <- "#e00000"
sub_table_background <- "#FAFAFA"

# Position indicators
rectangle_same <- "#A0A0A0"
triangle_up <- "#BEBEBE"
triangle_down <- "#808080"

# Win, lose, draw stats
win_stats_border <- "rgba(0, 128, 0, 0.6)"
draw_stats_border <- "rgba(190, 190, 190, 1)"
lose_stats_border <- "rgba(224, 0, 0, 0.5)"
light_grey <- "#F9F9F9"
dark_grey <- "#D3D3D3"


## Functions -------------------------------------------------------------------

res_indicator <- function(result) {
    #' Result indicator
    #'
    #' @description Indicator in the sub table showing us whether the team won,
    #' lost or drew in a match. 
    #' 
    #' @param result Result: "win", "draw" or "lose".
    #' 
    #' @return HTML with a green/grey/red circle based on the result.
    
    if (result == "win") {
        color <- win_green
    } else if (result == "draw") {
        color <- draw_grey
    } else {
        color <- lose_red
    }
    
    span(style = list(
        display = "inline-block",
        marginRight = "8px",
        width = "9px",
        height = "9px",
        backgroundColor = color,
        borderRadius = "50%"
    ))
}

pos_indicator <- function(type) {
    #' Position indicator
    #'
    #' @description Indicator that shows next to the team's position in the
    #' standings table and tells us if the current position is better, worse or
    #' equal than the previous one. If it is better, we return a triangle facing
    #' up, if it is worse, we return a triangle facing down. If it is equal, we
    #' return a rectangle.
    #' 
    #' @param type String telling us the type of indicator we want: "up",
    #' "same", "down".  
    #'  
    #' @return HTML span with the right indicator.
    
    if (type == "up") {
        return(span(class = "triangle-bottom"))
    } else if (type == "same") {
        return(span(style = list(
            display = "inline-block",
            marginLeft = "5px",
            marginBottom = "2px",
            width = "6px",
            height = "2px",
            backgroundColor = rectangle_same
        )))
    }
    return(span(class = "triangle-top"))
}

build_css <- function(n_match_days, cur_match_day) {
    #' Build a CSS string
    #'
    #' @description Build a CSS string for the table.
    #' 
    #' @param n_match_days Total number of match days.
    #' @param cur_match_day Integer, current match day. Value from 1 to
    #' n_match_days.
    #' 
    #' @return CSS string.
    
    css <- paste0("
        .spi-rating {
            border: 1px solid rgba(0, 0, 0, 0.03);
            border-radius: 0%;
            font-size: 13px;
        }
        
        .rating-win {
            border-color: ", win_stats_border, "
        }
        
        .rating-draw {
            border-color: ", draw_stats_border, "
        }
        
        .rating-lose {
            border-color: ", lose_stats_border, "
        }
        
        .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
            background: ", purple, ";
            border-top: 1px solid , ", purple, ";
            border-bottom: 1px solid ", purple, ";
        }
        
        .irs-grid-pol.small {
            height: 0px;
        }
        
        .irs {
            width: 100%;
        }
        
        .irs-grid-text {
            color: black;
        }
        
        .border-right {
            border-right: 2px solid rgba(0, 0, 0, 0.1);
        }
        
        .triangle-top {
            width: 0; 
            height: 0; 
            border-left: 4px solid transparent;
            border-right: 4px solid transparent;
            border-bottom: 4px solid ", triangle_up, ";
            position: relative;
            top: -8px;
            margin-left: 4px;
        }
        
        .triangle-bottom {
            width: 0; 
            height: 0; 
            border-left: 4px solid transparent;
            border-right: 4px solid transparent;
            border-top: 4px solid ", triangle_down, ";
            position: relative;
            top: 10px;
            margin-left: 4px;
        }
    ")
    
    # Different color for disabled ticks in the slider.
    ticks_step <- round(n_match_days / 10)
    ticks <- seq(1, n_match_days, by = ticks_step)
    if (!n_match_days %in% ticks) ticks <- c(ticks, n_match_days)
    
    for (i in 1:length(ticks)) {
        if (cur_match_day < ticks[i]) {
            css <- paste0(css, "
                  .js-grid-text-", i-1, "{color: ", spinner_ticks, ";}")
        }
    }
    css
}

stats_col_style <- function(scaled_val) {
    #' Stats column style
    #'
    #' @description Given a scaled value of one of the results stats columns
    #' get the background color and prepare a style list for this value.
    #' 
    #' @param scaled_val Scaled value.
    #' 
    #' @return List with chosen and calculated style parameters: background
    #' color, color, font weight.
    
    if (is.nan(scaled_val)) scaled_val <- 1
    color <- stats_col_background_palette(scaled_val)
    list(background = color, fontWeight = "bold", color = purple)
}

stats_col_background_palette <- function(x) {
    #' Stats column background palette
    #'
    #' @description Using colorRamp and a range of colors return rgb color
    #' definition for a particular value.
    #' 
    #' @param x Scaled value.
    #' 
    #' @return RGB color.
    
    rgb(colorRamp(c(light_grey, dark_grey))(x), maxColorValue = 255)
}

teamname_style <- function(value, team_name) {
    #' Sub table team name style
    #'
    #' @description Prepare sub table team name style. If team name corresponds
    #' to the team of interest, set color to purple and font weight to bold.
    #' Otherwise make it black and set font weight to normal.
    #' 
    #' @param value Team name belonging to the current sub table.
    #' @param team_name Team name displayed in the sub table's row.
    #' 
    #' @return List with chosen style parameters: color, font weight,
    #' font size of 12.
    
    if (value == team_name) {
        color <- purple
        fontweight <- "bold"
    } else {
        color <- black
        fontweight <- "normal"
    } 
    list(color = color, fontSize = 12, fontWeight = fontweight)
}

get_team_img <- function(team) {
    #' Get team logo
    #'
    #' @description Read team's logo image from a local folder and prepare a tag
    #' list for display.
    #' 
    #' @param team Team name.
    #' 
    #' @return A tag list with team's logo.
    
    image <- img(src = paste0(gsub(" ", "", team, fixed = TRUE), ".svg"),
                 height = "24px", alt = team)
    tagList(div(style = list(display = "inline-block", width = "45px"), image),
            team)
}

goal_diff_color <- function(value) {
    #' Goal difference color
    #'
    #' @description Pick color based on the goal difference value. Green if the
    #' goal difference is positive, grey if it is zero, red if negative.
    #' 
    #' @param value Integer, goal difference.
    #' 
    #' @return Color HEX code.
    
    if (value > 0) {
        color <- "#008000"
    } else if (value < 0) {
        color <- "#e00000"
    } else {
        color <- "#777"
    }
    color
}

matchday_slider <- function(inputId, label, min, max, value, step = NULL,
                            from_min, from_max) {
    #' Match day slider
    #'
    #' @description 
    #' 
    #' @param inputId Slider inputId.
    #' @param label Slider label.
    #' @param min Integer, min slider value.
    #' @param max Integer, max slider value.
    #' @param value Integer, default slider value.
    #' @param step Integer, slider step size.
    #' @param from_min Integer, min selectable slider value.
    #' @param from_max Integer, max selectable slider value.
    #' 
    #' @return Shiny slider input modified based on our parameters.
    
    x <- sliderInput(inputId, label, min, max, value, step,
                     width = '100%', ticks = TRUE)
    x$children[[2]]$attribs <- c(x$children[[2]]$attribs, 
                                 "data-from-min" = from_min, 
                                 "data-from-max" = from_max)
    x
}
