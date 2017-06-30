# load stuff
library(tidyverse)
library(ggthemes)
nps <- read_csv("All National Parks Visitation 1904-2016.csv")

# clean data
nps <- nps %>%
    select(Parkname:YearRaw) %>%
    arrange(`Unit Name`, YearRaw) %>%
    filter(YearRaw!="Total")

colnames(nps) <- c("Park", "Region", "State", "Code", "Name", "Type", "Visitors", "Year")
nps$Park <- factor(nps$Park)
nps$Region <- factor(nps$Region)
nps$State <- factor(nps$State)
nps$Code <- factor(nps$Code)
nps$Name <- factor(nps$Name)
nps$Type <- factor(nps$Type)
nps$Year <- as.numeric(nps$Year)

# get national park totals
parkTotals <- nps %>%
    filter(Type=="National Park", 
           Code!="WOTR",
           Name!="Denali National Preserve") %>% # for some reason, Wolf Trap Center for Performing Arts is there, and Denali is doubled
    group_by(Name) %>%
    summarise(Total = sum(Visitors)) %>%
    arrange(-Total)

# get area for each national park from https://en.wikipedia.org/wiki/List_of_national_parks_of_the_United_States
area <- read_csv("parks_area.csv")

# merge with nps data frame
parks <- merge(nps, area) %>%
    select(Name, Region, State, Code, Visitors, Year, Area) %>%
    arrange(Name, Year) %>% 
    as_tibble() %>%
    mutate(PersonAcre = Visitors/Area)

# regions
parks$Region <- ifelse(parks$Region == "AK", "Alaska",
                       ifelse(parks$Region == "IM", "Intermountain",
                              ifelse(parks$Region =="MW", "Midwest",
                                     ifelse(parks$Region == "NE", "Northeast",
                                            ifelse(parks$Region =="PW", "Pacific West",
                                                   ifelse(parks$Region == "SE", "Southeast", NA))))))

# set columns
parks$Name <- factor(parks$Name)
parks$ShortName <- factor(parks$ShortName)
parks$Region <- factor(parks$Region)
parks$State <- factor(parks$State)
parks$code <- factor(parks$Code)

# get change in visitors
for (i in unique(parks$ShortName)) {
    tmp <- parks %>% 
        filter(ShortName==i) %>%
        mutate(Change = c(NA, diff(Visitors)),
               ChangeProp = Change/lag(Visitors))
    if (i=="Acadia") { df <- tmp }
    else { df <- rbind(df, tmp) }
}
parks <- df

# save data frame
save(parks, file="parkVisits.Rda")
