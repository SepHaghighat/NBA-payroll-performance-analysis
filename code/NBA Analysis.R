library(tidyverse)

#Importing the dataset
TeamPayroll_raw = read_csv("NBA Dataset/NBA Payroll(1990-2023).csv")
TeamSummaries_raw  = read_csv("NBA Dataset/Team Summaries.csv")
TeamHistories_raw = read_csv("NBA Dataset/TeamHistories.csv")

#Take a brief look at them (especially for checking the data types)
glimpse(TeamPayroll_raw)
glimpse(TeamSummaries_raw)
glimpse(TeamHistories_raw)


#Selecting the column that I want 
TeamPayroll = TeamPayroll_raw %>%
  select(team, seasonStartYear, payroll, inflationAdjPayroll) %>%
  #filtering to the time horizon (1990-2020)
  filter(seasonStartYear <= 2020) %>%
  #Changing the data type of two columns and Adding them as new variables
  mutate(payroll_clean = as.numeric(gsub("[$,]", "", payroll)),
        inflationAdjPayroll_clean = as.numeric(gsub("[$,]", "", inflationAdjPayroll))) %>%
  #Drop the columns we already have the better version of them (the columns from last lines added)
  select(!payroll & !inflationAdjPayroll)

#Check whether changes are successful 
glimpse(TeamPayroll)

#Adding teamCityHist for franchise relocations
TeamPayroll = TeamPayroll %>%
  mutate(teamCityHist = case_when(
    team %in% c("LA Lakers", "LA Clippers") ~ "Los Angeles",
    team == "Oklahoma City" & seasonStartYear < 2008 ~ "Seattle",
    team == "Oklahoma City" & seasonStartYear >= 2008 ~ "Oklahoma City",
    team == "New Orleans" & seasonStartYear > 2004 & seasonStartYear < 2007 ~ "Oklahoma City",
    team == "Brooklyn" & seasonStartYear < 2012 ~ "New Jersey",
    team == "Brooklyn" & seasonStartYear >= 2012 ~ "Brooklyn",
    team == "Memphis" & seasonStartYear < 2001 ~ "Vancouver",
    team == "Memphis" & seasonStartYear >= 2001 ~ "Memphis",
    TRUE ~ team))

#Adding teamName for the correction of Los Angeles teams
TeamPayroll = TeamPayroll %>%
  mutate(
    teamName = case_when(
      str_detect(team, "Lakers") ~ "Lakers",
      str_detect(team, "Clippers") ~ "Clippers",
      TRUE ~ NA_character_
    ))

#We only want NBA data
TeamHistories = TeamHistories_raw %>%
  filter(league == "NBA") %>%
  #Dumping some unnecessary teams (all-star teams)
  filter(teamId > 1000000000) %>%
  #We only want the team that were active during 1990 to 2020
  filter(seasonFounded <= 2020, seasonActiveTill >= 1990) %>%
  #Removing the league column as it is only NBA right now
  select(!league)

#Joining for all the teams except Los Angeles teams
TeamPayroll_joined_nonLA = TeamPayroll %>%
  filter(teamCityHist != "Los Angeles") %>%
  left_join(TeamHistories, by = join_by(teamCityHist == teamCity))

#Joining for the Los Angeles teams
TeamPayroll_joined_LA = TeamPayroll %>%
  filter(teamCityHist == "Los Angeles") %>%
  left_join(TeamHistories, by = join_by(teamCityHist == teamCity, teamName == teamName))

#Adding the rows of two joins together
TeamPayroll_joined = bind_rows(TeamPayroll_joined_nonLA,TeamPayroll_joined_LA)

#Filtering to only the seasons that we are looking for and arrange in the specific order
TeamPayroll_joined = TeamPayroll_joined %>%
  filter(seasonStartYear >= seasonFounded, seasonStartYear <= seasonActiveTill)%>%
  arrange(seasonStartYear, desc(payroll_clean))


#Verifying correct number of teams per season
TeamPayroll_check = TeamPayroll_joined %>%
  distinct(seasonStartYear, teamId) %>%   # avoid double-counting teams
  group_by(seasonStartYear) %>%
  summarise(
    actual_teams = n(),
    expected_teams = case_when(
      seasonStartYear <= 1994 ~ 27,
      seasonStartYear <= 2003 ~ 29,
      TRUE ~ 30))

TeamPayroll_check %>%
  filter(actual_teams != expected_teams)

#This graph would show the number of teams based on the seasons
TeamPayroll_check %>%
  ggplot(aes(x = seasonStartYear)) +
  geom_line(aes(y = actual_teams), linewidth = 1, color = "blue") +
  geom_line(aes(y = expected_teams), linetype = "dashed", color = "blue") +
  labs(
    title = "Number of NBA Teams per Season",
    y = "Number of Teams",
    x = "Season Start Year")

#Adding seasonStartYear because the season column is actually seasonEndYear
TeamSummaries = TeamSummaries_raw %>%
  mutate(seasonStartYear = season - 1) %>%
  #Filter to only NBA and our time horizon
  filter(lg == "NBA", seasonStartYear >= 1990, seasonStartYear <= 2020)

#Creating a dataset for seasons' average stats rows so may we use it later
TeamSummaries_league_avg = TeamSummaries %>%
  filter(team == "League Average")

#Creating two columns for having the team name and the city separately
TeamSummaries = TeamSummaries %>%
  mutate(teamName = word(team, -1),
         teamCity = str_trim(str_remove(team, paste0("\\s+", teamName, "$"))))

#Removing the rows of seasons' average stats
TeamSummaries = TeamSummaries %>%
  filter(team != "League Average") %>%
  #Correcting the name of Oklahoma City Hornets team for two specific seasons
  mutate(team = if_else(team == "New Orleans/Oklahoma City Hornets" & seasonStartYear %in% c(2005, 2006),
        "Oklahoma City Hornets",team)) %>%
  #Correcting the issue of Portland Trail Blazers 
  mutate( teamCity = if_else(team == "Portland Trail Blazers","Portland", teamCity),
          teamName = if_else(team == "Portland Trail Blazers","Trail Blazers",teamName))
  
#Joining with TeamHistories because we need the column teamId later
TeamSummaries = TeamSummaries %>%
  left_join(TeamHistories, by = c("teamCity", "teamName")) %>%
  filter( seasonStartYear >= seasonFounded, seasonStartYear <= seasonActiveTill)

#Creating the final dataset
NBA_analysis = TeamPayroll_joined %>%
  left_join(TeamSummaries, by = c("teamId", "seasonStartYear")) %>%
  #Selecting only the variables that we want
  select(
    seasonStartYear,
    teamName = team.y,
    teamAbbrev = abbreviation,
    playoffs,
    age,
    payroll = payroll_clean,
    inflationAdjPayroll = inflationAdjPayroll_clean,
    wins = w,
    losses = l,
    marginOfVictory = mov,
    simpleRatingSystem = srs,
    netRating = n_rtg,
    attend,
    avgAttend = attend_g) %>%
  #Adding some varibales that we are going to use for analysis and locating them
  mutate(win_pct = round( wins / (wins + losses), 2)) %>%
  relocate(win_pct, .after = losses) %>%
  group_by(seasonStartYear) %>%
  mutate(payrollRatio = inflationAdjPayroll / mean(inflationAdjPayroll)) %>%
  ungroup() %>%
  relocate(payrollRatio, .after = inflationAdjPayroll) %>%
  mutate(payrollin10m = round(inflationAdjPayroll / 10000000, 2)) %>%
  relocate(payrollin10m, .after = inflationAdjPayroll)

#Take a quick look at the mean, median, ... 
summary(NBA_analysis$inflationAdjPayroll)
summary(NBA_analysis$wins)
summary(NBA_analysis$win_pct)

#The graph is representing the average payroll of a NBA team through the decades
NBA_analysis %>%
  group_by(seasonStartYear) %>%
  summarise(avg_payroll = mean(payrollin10m)) %>%
  ggplot(aes(x = seasonStartYear, y = avg_payroll)) +
  #Line graph attributes
  geom_line(linewidth = 1, color = "blue") +
  #Attributes of points on the line
  geom_point(
    data = ~ filter(.x, seasonStartYear %in% c(1990, 2000, 2014, 2020)),
    color = "red", size = 2) +
  #Setting the exact number of average payroll (round to 2 decimal) to be shown on specific years
  geom_text(
    data = ~ filter(.x, seasonStartYear %in% c(1990, 2000, 2014, 2020)),
    aes(label = round(avg_payroll, 2)), vjust = -0.8, size = 3) +
  #Title and naming of the axes 
  labs(
    title = "Average NBA Teams Payroll Over Time",
    x = "Season",
    y = "Average Payroll (in $10 million)")+
  theme_minimal()

#The graph shows the relationship between team win percentage and relative payroll across NBA eras
NBA_analysis %>%
  mutate(era = case_when(
    seasonStartYear < 2000 ~ "1990s",
    seasonStartYear < 2010 ~ "2000s",
    TRUE ~ "2010s"
  )) %>%
  ggplot(aes(x = payrollRatio, y = win_pct)) +
  #Plotting the observations
  geom_point(alpha = 0.5) +
  #Adding a trend line for each era
  geom_smooth(method = "lm", se = FALSE) +
  #Separating the graph into plots by era
  facet_wrap(~ era) +
  #Title and naming of the axes 
  labs(
    title = "Team Win Percentage and Relative Payroll by Era",
    x = "Payroll Relative to League Average",
    y = "Win Percentage")+
  theme_minimal()

#Measuring the mean payroll for qualified teams for playoffs and non-qualified ones
NBA_analysis %>%
  group_by(playoffs) %>%
  summarise(avg_payroll = mean(inflationAdjPayroll))

#Using t-test to see whether average payroll differs between qualified teams for playoffs and non-qualified ones
t.test(inflationAdjPayroll ~ playoffs, data = NBA_analysis)

#Creating a table to have NBA champions and runner-ups in selected seasons (1992,2001,2017)
Champions = tibble::tribble(
  ~seasonStartYear, ~teamName,                 ~result,
  1992,             "Chicago Bulls",           "Champion",
  1992,             "Portland Trail Blazers",  "Runner-up",
  2001,             "Los Angeles Lakers",      "Champion",
  2001,             "Philadelphia 76ers",      "Runner-up",
  2017,             "Golden State Warriors",   "Champion",
  2017,             "Cleveland Cavaliers",     "Runner-up")

#This graph shows the comparison of payroll rank and win percentage rank for selected NBA seasons
NBA_analysis %>%
  filter(seasonStartYear %in% c(1992, 2001, 2017)) %>%
  #Computing within-season ranks for payroll and win percentage
  group_by(seasonStartYear) %>%
  mutate(
    payrollRank = rank(-payrollRatio, ties.method = "first"),
    winRank = rank(-win_pct, ties.method = "first")
  ) %>%
  ungroup() %>%
  #Merging with champions
  left_join(Champions,
            by = c("seasonStartYear", "teamName")) %>%
  ggplot(aes(x = payrollRank, y = winRank)) +
  geom_point(alpha = 0.5, size = 2, color = "red") +
  #For showing champions and runner-ups clearer
  geom_point(
    data = ~ dplyr::filter(.x, !is.na(result)),
    aes(shape = result),
    size = 3,
    color = "blue") +
  #Showing champion and runner-up teams names
  geom_text(
    data = ~ dplyr::filter(.x, !is.na(result)),
    aes(label = teamName),
    vjust = -0.8,
    size = 3) +
  #Reversing both axes
  scale_x_reverse() +
  scale_y_reverse() +
  #Separating graphs by season (obviously means our selected seasons)
  facet_wrap(~ seasonStartYear) +
  #Title and naming of the axes 
  labs(
    title = "Payroll and Win Percentage Rank in Selected NBA Seasons",
    x = "Payroll Rank (1 = Highest Payroll)",
    y = "Win Percentage Rank (1 = Best Record)",
    shape = "Season Finalists") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"))

#The graph represents the relationship between payroll and net rating for selected seasons 
NBA_analysis %>%  
  filter(seasonStartYear %in% c(1992, 2001, 2017)) %>%
  #Computing within-season ranks for payroll and net rating
  group_by(seasonStartYear) %>%
  mutate(
    payrollRank = rank(-payrollRatio, ties.method = "first"),
    netRatingRank = rank(-netRating, ties.method = "first")) %>%
  ungroup() %>%
  #Merging with champions
  left_join(Champions,
            by = c("seasonStartYear", "teamName")) %>%
  ggplot(aes(x = payrollRank, y = netRatingRank)) +
  geom_point(alpha = 0.5, size = 2, color = "red") +
  #For showing champions and runner-ups clearer
  geom_point(
    data = ~ dplyr::filter(.x, !is.na(result)),
    aes(shape = result),
    size = 3,
    color = "blue") +
  #Showing champion and runner-up teams names
  geom_text(
    data = ~ dplyr::filter(.x, !is.na(result)),
    aes(label = teamName),
    vjust = -0.8,
    size = 3) +
  #Reversing both axes
  scale_x_reverse() +
  scale_y_reverse() +
  #Separating graphs by season (obviously means our selected seasons)
  facet_wrap(~ seasonStartYear) +
  #Title and naming of the axes 
  labs(
    title = "Payroll Rank and Net Rating Rank in Selected NBA Seasons",
    x = "Payroll Rank (1 = Highest Payroll)",
    y = "Net Rating Rank (1 = Best Net Rating)",
    shape = "Season Finalists") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"))












