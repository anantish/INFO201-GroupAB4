# read in the data file containing seattle shooting information
officer_shooting_data_seattle <- read.csv("SPD_Officer_Involved_Shooting__OIS__Data.csv", stringsAsFactors = FALSE)

# read in data file containing shooting information for many cities in the country
vice_news_data <- read.csv("ViceNews_FullOISData - Sheet1.csv", stringsAsFactors = FALSE)

# extract the new york city data from the country dataset
officer_shooting_data_new_york <- filter(vice_news_data, Department == "New York Police Department")

# create a dataframe with appropriate columns for seattle
new_officer_shooting_data_seattle <- select(officer_shooting_data_seattle, Date, Officer.Race, Subject.Race, Subject.Weapon, Fatal, GO)
# rename certain columns within the above dataframe to be consistent with what is in the new york data set
new_officer_shooting_data_seattle_renamed_2 <- rename(new_officer_shooting_data_seattle, OfficerRace = Officer.Race,
                                                    SubjectRace = Subject.Race, SubjectArmed = Subject.Weapon)
#resolve what we believe are duplicates within the Seattle data (using the GO column)
new_officer_shooting_data_seattle_renamed <- group_by(new_officer_shooting_data_seattle_renamed_2, GO) %>%
                                                  summarise(Date = paste(unique(Date), collapse = " , "),
                                                            OfficerRace = paste(OfficerRace, collapse = " , "),
                                                            SubjectRace = paste(unique(SubjectRace), collapse =" , "),
                                                            SubjectArmed = paste(unique(SubjectArmed), collapse =" , "),
                                                            Fatal = paste(unique(Fatal), collapse =" , "))

# create a dataframe with appropriate columns for new york
new_officer_shooting_data_new_york <- select(officer_shooting_data_new_york, Date, OfficerRace, SubjectRace, SubjectArmed, Fatal)

# correct values within the new york dataframe so as to be consistent in terms of how represented in the Seattle dataset
new_officer_shooting_data_new_york[new_officer_shooting_data_new_york == "B"] <- "Black"
new_officer_shooting_data_new_york[new_officer_shooting_data_new_york == "W"] <- "White"
new_officer_shooting_data_new_york[new_officer_shooting_data_new_york == "L"] <- "Latino"
new_officer_shooting_data_new_york[new_officer_shooting_data_new_york == "U"] <- "Unknown"
new_officer_shooting_data_new_york[new_officer_shooting_data_new_york == "A"] <- "Asian"

new_officer_shooting_data_new_york[new_officer_shooting_data_new_york == "N"] <- "No"
new_officer_shooting_data_new_york[new_officer_shooting_data_new_york == "Y"] <- "Yes"
new_officer_shooting_data_new_york[new_officer_shooting_data_new_york == "F"] <- "Yes"

# we will only retain the shooting year for both Seattle and New York (losing some precision for the date in the process)
new_officer_shooting_data_new_york <- transform(new_officer_shooting_data_new_york, Date = ifelse(nchar(Date) >= 8,
                                                    format(as.Date(new_officer_shooting_data_new_york$Date, format="%m/%d/%Y"),"%Y"), Date))

# do the same for Seattle
new_officer_shooting_data_seattle_renamed <- transform(new_officer_shooting_data_seattle_renamed, Date = ifelse(nchar(Date) >= 8,
                                                    format(as.Date(new_officer_shooting_data_seattle_renamed$Date, format="%m/%d/%Y"),"%Y"), Date))

# for seattle, find the number of fatal shootings per year
seattle_fatal_by_year <- group_by(new_officer_shooting_data_seattle_renamed, Date) %>% filter(grepl("Yes", Fatal)) %>%
                                  summarize(NumberFatalShootings = n())

# for seattle, find non-fatal shooting counts by year
seattle_non_fatal_by_year <- group_by(new_officer_shooting_data_seattle_renamed, Date) %>% filter(grepl("No", Fatal)) %>%
  summarize(NumberShootings = n())

# for seattle, compute fatal shootings per year where the target was non-white
seattle_fatal_by_year_non_White <- group_by(new_officer_shooting_data_seattle_renamed, Date) %>% filter(grepl("Yes", Fatal) & SubjectRace != "White") %>%
  summarize(NumberShootings = n())

# for seattle, compute fatal shootings per year where the target was white
seattle_fatal_by_year_White <- group_by(new_officer_shooting_data_seattle_renamed, Date) %>% filter(grepl("Yes", Fatal) & SubjectRace == "White") %>%
  summarize(NumberShootings = n())

# for seattle, compute non-fatal shootings per year where the target was non-white
seattle_non_fatal_by_year_non_White <- group_by(new_officer_shooting_data_seattle_renamed, Date) %>% filter(grepl("No", Fatal) & SubjectRace != "White") %>%
  summarize(NumberShootings = n())

# for seattle, compute non-fatal shootings per year where the target was white
seattle_non_fatal_by_year_White <- group_by(new_officer_shooting_data_seattle_renamed, Date) %>% filter(grepl("No", Fatal) & SubjectRace == "White") %>%
  summarize(NumberShootings = n())

# for new york, find the number of fatal shootings per year
new_york_fatal_by_year <- group_by(new_officer_shooting_data_new_york, Date) %>% filter(grepl("Yes", Fatal)) %>%
  summarize(NumberFatalShootings = n())

# for new york, find the number of non-fatal shootings per year
new_york_non_fatal_by_year <- group_by(new_officer_shooting_data_new_york, Date) %>% filter(grepl("No", Fatal)) %>%
  summarize(NumberNonFatalShootings = n())

# for new york, compute fatal shootings per year where the target was non-white
new_york_fatal_by_year_non_White <- group_by(new_officer_shooting_data_new_york, Date) %>% filter(grepl("Yes", Fatal) & SubjectRace != "White") %>%
  summarize(NumberShootings = n())

# for new york, compute fatal shootings per year where the target was white
new_york_fatal_by_year_White <- group_by(new_officer_shooting_data_new_york, Date) %>% filter(grepl("Yes", Fatal) & SubjectRace == "White") %>%
  summarize(NumberShootings = n())

# for new york, compute non-fatal shootings per year where the target was non-white
new_york_non_fatal_by_year_non_White <- group_by(new_officer_shooting_data_new_york, Date) %>% filter(grepl("No", Fatal) & SubjectRace != "White") %>%
  summarize(NumberShootings = n())

# for new york, compute non-fatal shootings per year where the target was white
new_york_non_fatal_by_year_White <- group_by(new_officer_shooting_data_new_york, Date) %>% filter(grepl("No", Fatal) & SubjectRace == "White") %>%
  summarize(NumberShootings = n())
