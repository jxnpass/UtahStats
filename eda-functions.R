library(tidyverse)
library(tidyr)
library(stringr)
library(extrafont)

loadfonts(device = "all")

### THEMES SET ---------
theme_set(theme_bw())

add_theme <- 
  theme(
    text = element_text(family = "Arial", size = 10), 
    plot.title = element_text(size = 12, face = "bold"), 
    plot.subtitle = element_text(size = 11), 
    plot.caption = element_text(size = 10), 
    legend.position = 'bottom',
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

col1 = "#ffffff"
col2 = "#0c366e"
col3 = "#febd59"
col4 = "#548aae"
col5 = "#d9d9d9"
col6 = "#0f5387"
col7 = "#9b9999"
col8 = "#000000"
coluvu <- "#275D38"
  
color_2_breaks = c(col5, col6)
color_3_breaks = c(col5, col4, col2)
color_4_breaks = c(col1, col5, col4, col6)
color_5_breaks = c(col1, col5, col4, col6, col2)
color_6_breaks = c(col1, col5, col4, col6, col2, col3)
color_7_breaks = c(col1, col5, col4, col6, col2, col3, col8)

add_tilt <- theme(axis.text.x = element_text(size = 7, angle = 20, 
                                             hjust = 1, vjust = 1)) 
  
add_y_percent <- scale_y_continuous(labels = scales::percent)
add_x_percent <- scale_x_continuous(labels = scales::percent)

remove_gridlines <- theme(panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank())
remove_vertical_gridlines <- theme(panel.grid.major.x = element_blank(),
                                   panel.grid.minor.x = element_blank())
remove_horizontal_gridlines <- theme(panel.grid.major.y = element_blank(),
                                     panel.grid.minor.y = element_blank())
  
color_politics <- c(
  "Very liberal" = col2,
  "Liberal" = col4,
  "Centrist" = col5,
  "Conservative" = "red",
  "Very conservative" = "red4"
)

color_politics_simple <- c("Democrat" = col2, "Moderate" = col7,
                           "Republican" = "red3")

color_dating_apps <- c("Mutual" = "#e633ab",
                       "Hinge" = col8, "Tinder" = "#fb435b",
                       "Bumble" = "#f9bb34", "Facebook Dating" = "#0077f5",
                       "Other" = col7)

### BASIC FUNCTIONS --------------------

# to get rid of NAs for visuals
filter_na_out <- function(df, col_name = c()) {
  new_df <- df %>%
    filter(if_all(all_of(col_name), ~ !is.na(.)))
  
  return(new_df)
}

### SECTION 1 -- SAMPLE DEMOGRAPHICS FUNCTIONS -----------------

# reset gender to be more vague
reset_sex <- function(df) {
  new_df <- df %>% 
    mutate(Q3_Sex = ifelse(
      Q3_Sex != "Male" & Q3_Sex != "Female", "Other", Q3_Sex
    ))
  
  return(new_df)
  
}

# reset country to be more generalized
reset_origin <- function(df, do = "Q7_Origin") {
  
  state_regions <- list(
    "Western States" = c("Alaska", "Arizona", "California", "Colorado", "Hawaii", 
                         "Idaho", "Montana", "Nevada", "New Mexico", "Oregon", 
                         "Washington", "Wyoming"),
    "Eastern States" = c("Connecticut", "Delaware", "Florida", "Georgia", "Maine", 
                         "Maryland", "Massachusetts", "New Hampshire", "New Jersey", 
                         "New York", "North Carolina", "Pennsylvania", "Rhode Island", 
                         "South Carolina", "Vermont", "Virginia", "West Virginia"),
    "Midwestern States" = c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", 
                            "Minnesota", "Missouri", "Nebraska", "North Dakota", 
                            "Ohio", "South Dakota", "Wisconsin"),
    "Southern States" = c("Alabama", "Arkansas", "Kentucky", "Louisiana", "Mississippi", 
                          "Oklahoma", "Tennessee", "Texas"),
    "Utah" = "Utah"
  )
  
  # Function to classify states into regions
  classify_state <- function(state) {
    case_when(
      state %in% state_regions[["Western States"]] ~ "Western States",
      state %in% state_regions[["Eastern States"]] ~ "Eastern States",
      state %in% state_regions[["Midwestern States"]] ~ "Midwest",
      state %in% state_regions[["Southern States"]] ~ "South",
      state %in% state_regions[["Utah"]] ~ "Utah",
      TRUE ~ "International"
    )
  }
  
  if (do == "Q7_Origin") {
    new_df <- df %>% 
      mutate(Q7_Origin = classify_state(Q7_Origin))
  }
  else if (do == "Q33_MissionLocation"){
    new_df <- df %>% 
      mutate(Q33_MissionLocation = classify_state(Q33_MissionLocation))
  }
  
  return(new_df)
}

reset_college <- function(df) {
 
  new_df <- df %>% 
    mutate(Q13_CollegeName = ifelse(
      Q13_CollegeName != "BYU" &
      Q13_CollegeName != "UVU" &
      Q13_CollegeName != "None",
      "Other", Q13_CollegeName
    )) %>% 
    mutate(Q13_CollegeName = factor(Q13_CollegeName, 
                                    levels = c("BYU",
                                               "UVU",
                                               "None",
                                               "Other")))
  
  return(new_df)
  
  }

# stateside mission or foreign 

stateside_mission <- function(df) {
  
  states <- map_data("state") %>% 
    select(region) %>% 
    unique() %>% 
    mutate(region = str_to_title(region)) %>% 
    add_row(data.frame(region = c("Alaska","Hawaii"))) %>% 
    arrange(region)
  states <- states$region
  
  new_df <- df %>% 
    mutate(SERVED_US_MISSION = 
             ifelse(Q33_MissionLocation %in% states & 
                      !is.na(Q33_MissionLocation), "Yes", "No"),
           ) %>% 
    mutate(SERVED_US_MISSION = 
             ifelse(is.na(Q33_MissionLocation), 
                    "Did Not Serve", SERVED_US_MISSION)) %>% 
    mutate(SERVED_US_MISSION = factor(SERVED_US_MISSION,
                                      levels = c("Did Not Serve", "Yes", "No")))
  
  return(new_df)
  
}

# table1 visual render

render_t1 <- function(x) {
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  n <- length(x)
  error <- qt(0.975, df = n - 1) * sd_val / sqrt(n)
  lower_ci <- mean_val - error
  upper_ci <- mean_val + error
  min_val <- min(x, na.rm = T)
  max_val <- max(x, na.rm = T)
  
  # Format the output
  paste0(
    "Mean = ", round(mean_val, 2), "<br/>",
    "SD = ", round(sd_val, 2), "<br/>",
    "95% CI = [", round(lower_ci, 2), ", ", round(upper_ci, 2), "]", "<br/>",
    "[Min, Max] = [", round(min_val, 2), ", ", round(max_val, 2), "]"
  )
}

# map data and visuals help

map_us <- map_data("state") %>% rename(State = region)
map_us$State <- str_to_title(map_us$State)

state_centroids <- data.frame(
  State = c("Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
            "Delaware", "Florida", "Georgia", "Idaho", "Illinois", "Indiana", "Iowa", 
            "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
            "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", 
            "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", 
            "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
            "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
            "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
            "West Virginia", "Wisconsin", "Wyoming"),
  x = c(-87.9023, -111.0937, -92.3731, -119.4179, -105.7821, -72.6851, 
        -75.5277, -81.5158, -83.6431, -114.7420, -89.3985, -86.2816, -93.0977, 
        -98.4842, -84.2700, -91.9623, -69.4455, -76.6413, -71.3824, -84.6130, 
        -94.6859, -89.3985, -92.6038, -110.3626, -99.9018, -117.0554, -71.5724, 
        -74.4057, -105.9930, -75.4999, -79.0193, -101.0020, -82.9071, -97.4926, 
        -120.5542, -77.1945, -71.4774, -80.9450, -99.4388, -86.7816, -99.9018, 
        -111.0937, -72.5778, -78.6569, -120.7401, -80.4549, -89.6165, -107.2903),
  y = c(32.3182, 34.0489, 34.7465, 36.7783, 39.5501, 41.6032, 
        38.9108, 27.6648, 32.1656, 44.0682, 40.6331, 40.2672, 41.8780, 
        39.0119, 37.8393, 30.9843, 45.2538, 39.0458, 42.4072, 44.3148, 
        46.7296, 32.3547, 37.9643, 46.8797, 41.4925, 38.8026, 43.1939, 
        40.0583, 34.5199, 43.2994, 35.7596, 47.5515, 40.4173, 35.0078, 
        43.8041, 41.2033, 41.5801, 33.8361, 43.9695, 35.5175, 31.9686, 
        39.3210, 44.5588, 37.4316, 47.7511, 38.5976, 43.7844, 43.0750)
)

scale_by_size <- function(Count) {
  New_Count = c()
  for (i in seq_along(Count)) {
    New_Count[i] = min(3, Count[i])
  }
  return(New_Count)
}

filter_sex <- function(df) {
  
  new_df <- df %>% 
    filter(Q3_Sex == "Male" | Q3_Sex == "Female")
  
  return(new_df)
  
}

reset_ethnicity <- function(df) {
  
  new_df <- df %>% 
    mutate(Q4_Ethnicity = case_when(
      Q4_Ethnicity == "American Indian or Alaska Native" ~ "Other",
      Q4_Ethnicity == "Native Hawaiian or Pacific Islander" ~ "Other",
      Q4_Ethnicity == "Black or African American" ~ "Other",
      T ~ Q4_Ethnicity
    ))
  
  return(new_df)
}

### SECTION 2 -- COLLEGE ------------

# cleaning majors

clean_byu_major_names <- function(df) {

  new_df <- df %>%
   mutate(Q15_Major = case_when(
    str_detect(Q15_Major, "\\(BS\\)") ~ str_replace(Q15_Major, " \\(BS\\)", ""),
    str_detect(Q15_Major, "\\(BA\\)") ~ str_replace(Q15_Major, " \\(BA\\)", ""),
    str_detect(Q15_Major, "\\(BM\\)") ~ str_replace(Q15_Major, " \\(BM\\)", ""),
    str_detect(Q15_Major, "\\(BFA\\)") ~ str_replace(Q15_Major, " \\(BFA\\)", ""),
    TRUE ~ Q15_Major
      ),
    Q15_Major = str_replace(Q15_Major, " \\*", ""),
    Q15_Major = str_replace(Q15_Major, "\\*", "")
    )

return(new_df)
}

clean_misc_major <- function(df) {
  
  new_df <- df %>% 
    mutate(Q15_Major = ifelse(Q15_Major == "Misc.", "Undecided", Q15_Major),
           department = ifelse(department == "Misc.", "Undecided", department),
           college = ifelse(college == "Misc.", "Undecided", college),)
  
  return(new_df)
}

# group_GPA
group_GPA <- function(df) {
  
  letter_order <- c("A+", "A", "A-", "B+","B", "B-", "< C+")
  
  new_df <- df %>% 
    mutate(GPA_letter = case_when(
      Q14_CollegeGPA == 4.0 ~ "A+",   
      Q14_CollegeGPA >= 3.7 ~ "A",   
      Q14_CollegeGPA >= 3.3 ~ "A-",   
      Q14_CollegeGPA >= 3.0 ~ "B+",   
      Q14_CollegeGPA >= 2.7 ~ "B",    
      Q14_CollegeGPA >= 2.3 ~ "B-",   
      Q14_CollegeGPA < 2.3 ~ "< C+"  
    )) %>% 
    mutate(GPA_letter = factor(GPA_letter, levels = letter_order))
  
  return(new_df)
  
}

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {

  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  
  
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

acronym_college <- function(df) {
  
  new_df <- df %>% 
    mutate(college = case_when(
      college == "Business (Marriott School of Business)" ~ "MSB",
      college == "Computational, Mathematical and Physical Sciences" ~ "CPMS",
      college == "Family, Home, and Social Sciences" ~ "FHSS",
      college == "Fine Arts and Communications" ~ "FA&C",
      college == "Ira A. Fulton College of Engineering" ~ "Engineering",
      college == "David O. McKay School of Education" ~ "Education",
      T ~ college
    ))
  
  return(new_df)
  
  
  
}

### SECTION 3 - MISSION AND CHURCH ----------


filter_relationships <- function(df) {
  
  groups <- c("Single", "Engaged", "Married", "Serious Relationship")
  
  new_df <- df %>% 
    filter(Q6_RelationshipStatus %in% groups) %>% 
    mutate(Q6_RelationshipStatus = case_when(
      Q6_RelationshipStatus %in% c("Engaged", "Married") ~ "Engaged/Married",
      Q6_RelationshipStatus == "Serious Relationship" ~ "Dating",
      T ~ Q6_RelationshipStatus
    ))
  
  return(new_df)
  
}

# SECTION 4 - LIFESTYLE -----------

reset_currentliving <- function(df) {
  
  new_df <- df %>% 
    mutate(Q22_CurrentLiving = case_when(
      str_detect(Q22_CurrentLiving, "Heritage") ~ "Student Housing",
      str_detect(Q22_CurrentLiving, "Helaman") ~ "Student Housing",
      Q22_CurrentLiving == "House" ~ "House",
      T ~ "Off Campus Apartment"
    ))
  
  return(new_df)
  
}

### RECREATE DATAFRAME WITH COLUMNS HOLDING MULTIPLE VALUES

parse_string_values <- function(df, col_name, append_col_name) {
  words_list <- str_split(df[[col_name]], ",")
  unique_words <- unique(unlist(words_list))
  
  new_df <- sapply(unique_words, function(word) {
    sapply(words_list, function(words) {
      ifelse(word %in% words, TRUE, FALSE)
    })
  })
  
  new_df <- as.data.frame(new_df)
  new_df[] <- lapply(new_df, function(x) ifelse(x, T, F))
  new_df <- cbind(df %>% select(all_of(append_col_name)), new_df)
  
  length_append <- length(append_col_name)
  colnames(new_df)[1:length_append] <- append_col_name
  colnames(new_df)[(length_append+1):ncol(new_df)] <- (unique_words)
  
  return(new_df)
}

### FACTOR LEVELS

# mission role assignments Q35_MissionTopAssignment
factor_missionroles <- function(df) {
  
  levels <- c("None", "Office", "Trainer", "DL", "STL", "ZL", "AP")
  
  new_df <- df %>% 
    mutate(Q35_MissionTopAssignment = factor(Q35_MissionTopAssignment, levels = levels))
  
  return(new_df)
  
  
}

# applying factor to Parent_Income
factor_income <- function(df) {
  
  income_levels <- c("Less than $10,000",  "$10,000 - $19,999",  "$20,000 - $29,999",  
                     "$30,000 - $39,999",  "$40,000 - $49,999",  "$50,000 - $59,999",  
                     "$60,000 - $69,999",  "$70,000 - $79,999", "$80,000 - $89,999",  
                     "$90,000 - $99,999", "$100,000 - $149,999", "More than $150,000" )
  
  new_df <- df %>% 
    mutate(Q18_ParentIncome = factor(Q18_ParentIncome, levels = income_levels))

  return(new_df)
  }

factor_politics <- function(df) {
  
  politics_levels <- c("Very liberal", "Liberal", "Centrist", "Conservative", "Very conservative")

  new_df <- df %>% 
    mutate(Q26_PoliticalPhilosophy = factor(Q26_PoliticalPhilosophy, levels = politics_levels))
  
  return(new_df)
}

factor_simplify_politics <- function(df) {
  
  politics_levels <- c("Democrat", "Moderate", "Republican")
  
  new_df <- df %>% 
    mutate(Q26_PoliticalPhilosophy = case_when(
      str_detect(Q26_PoliticalPhilosophy, "[Cc]onservative", ) ~ "Republican",
      str_detect(Q26_PoliticalPhilosophy, "[Ll]iberal") ~ "Democrat",
      Q26_PoliticalPhilosophy == "Centrist" ~ "Moderate"
    )) %>% 
    mutate(Q26_PoliticalPhilosophy = factor(Q26_PoliticalPhilosophy, levels = politics_levels))
  
  return(new_df)
  
}

factor_churchactivity <- function(df) {
  
  levels <- c("Active", "Less Active", "Inactive")
  
  new_df <- df %>% 
    mutate(Q30_ChurchActivity = factor(Q30_ChurchActivity, levels = levels))
  
  return(new_df)
  
  
}

factor_churchstance <- function(df) {
  
  levels <- c("Do not support", "Somewhat Support", "Mostly Support", "Strongly Support")
  
  new_df <- df %>% 
    mutate(Q31_ChurchSupport = factor(Q31_ChurchSupport, levels = levels))
  
  return(new_df)
  
  
}


# INSIGHTS FUNCTIONS

# when using glm (log-reg), calculate probability increase

calc_prob <- function(int, coef) {
  
  log_odds_0 <- 1 / (1 + exp(-int))
  log_odds_1 <- 1 / (1 + exp(-(int + coef)))

  prob_change <- log_odds_1 - log_odds_0
  return(prob_change)
    
}



