library(tidyverse)
library(readxl)
cities <- read_csv("data/cities_constituency_data.csv")
elections <- readRDS("data/ledb_candidatelevel.rds")
place <- read_csv("data/places_geocoded.csv")


set.seed(435464564)
sample <- sample_n(cities, 150, replace=FALSE)
cities <- cities %>% mutate(sampled=if_else(geo_name %in% sample$geo_name, 1, 0))

elections <- elections %>% mutate(sampled=if_else(fips %in% sample$fips,1,0))
mean(cities$sampled)
mean(elections$sampled)
sum(elections$sampled)



test.variables <- c("population_2020", "mass_ideology_2020","pres_pctD_20", "pres_pctD_16", "pres_pctD_08", 
                    "percent_women", "percent_white", "percent_black", "percent_hispanic", "percent_asian_american") 


for (i in seq_along(test.variables)) {
  png(filename = paste(test.variables[i], "_qqplot.png", sep =""))
      qqplot(sample[[test.variables[i]]], y = cities[[test.variables[i]]],
              xlab = paste("Sample", test.variables[i]), 
              ylab=paste("Population", test.variables[i]))
  abline(0,1)
  dev.off()
}
## show this for every collected data or important ones to show sample is representative

full_count <- cities %>% count(state)
sample_count <- sample %>% count(state) 
full_count <- full_count %>% mutate(prop=n/872) %>% print()
sample_count <- sample_count %>% mutate(prop=n/100) %>% print()

write_csv(sample, "data/sample.csv")
sample <- read_excel("data/sample with manager.xlsx")
sample$council.manager <- as.numeric(sample$council.manager)
sum(sample$council.manager, na.rm=TRUE)
mean(sample$council.manager, na.rm=TRUE)
sum(is.na(sample$council.manager))
150-101-12
# 37 non manager cities, 101 manager cities

census <- read_csv("data/census.csv")

unique(elections$office_consolidated)
myelections <- elections %>% mutate(sampled = if_else(fips %in% sample$fips, 1, 0))
mean(myelections$sampled)
sum(myelections$sampled)



myelections$fips <- as.numeric(myelections$fips)
x <- left_join(sample, myelections, by = "fips")
x$City <- x$geo_name.x
mysample <- select(x, fips, City, state, full_name, firstname, lastname, state_abb, office_consolidated, year, district, contest, votes, vote_share, winner, n_winners, council.manager)
mysample <- mysample %>% filter(year > 2009) %>% arrange(year)
 

unique(mysample$contest)
contests <- mysample %>% group_by(contest) %>% summarize(total.votes = sum(votes), year=year, city=City, office = office_consolidated, state = state, council.manager=council.manager) %>% print()
contests <- contests %>% filter(!duplicated(contest)) %>% print
mayor <- contests %>% filter(office=="Mayor")
council <- contests %>% filter(office=="City Council")

at.large <- council %>% ungroup() %>% filter(grepl("synthetic at-large", contest))

mycontests <- full_join(mayor, at.large, by = c("city", "year", "contest", "office", "total.votes", "state", "council.manager"))
arrange(mycontests, by = year)

for (i in 2010:2022) {
  assign(paste0("mycontests_", i), filter(mycontests, year==i))
}
pop.2010 <- read_csv("data/2010.csv")
pop.2011 <-read_csv("data/2011.csv")
pop.2012 <-read_csv("data/2012.csv")
pop.2013 <-read_csv("data/2013.csv")
pop.2014 <-read_csv("data/2014.csv")
pop.2015 <-read_csv("data/2015.csv")
pop.2016 <-read_csv("data/2016.csv")
pop.2017 <-read_csv("data/2017.csv")
pop.2018 <-read_csv("data/2018.csv")
pop.2019 <-read_csv("data/2019.csv")
pop.2020 <-read_csv("data/2020.csv")
pop.2021 <-read_csv("data/2021.csv")
pop.2022 <-read_csv("data/2022.csv")



p <- function(x) {
  x <- x %>% rename(pop.estimate=B01003_001E)%>% 
    mutate(pop.estimate=as.numeric(pop.estimate)) %>%
    separate(NAME, into  = c("city", "state"), sep=",") %>% 
    select(city, state, pop.estimate) %>%
    drop_na()
}
## function works
pop.2010 <- p(pop.2010)
pop.2011 <- p(pop.2011)
pop.2012 <- p(pop.2012)
pop.2013 <- p(pop.2013)
pop.2014 <- p(pop.2014)
pop.2015 <- p(pop.2015)
pop.2016 <- p(pop.2016)
pop.2017 <- p(pop.2017)
pop.2018 <- p(pop.2018)
pop.2019 <- p(pop.2019)
pop.2020 <- p(pop.2020)
pop.2021 <- p(pop.2021)
pop.2022 <- p(pop.2022)


t <- function(x,y) {
  x$city <- tolower(x$city)
  y$city <- tolower(y$city)
  y$state <- trimws(y$state)
  left_join(x,y, by= c("city", "state"))
}
mycontests_2010 <- t(mycontests_2010, pop.2010)
mycontests_2011 <- t(mycontests_2011, pop.2011)
mycontests_2012 <- t(mycontests_2012, pop.2012)
mycontests_2013 <- t(mycontests_2013, pop.2013)
mycontests_2014 <- t(mycontests_2014, pop.2014)
mycontests_2015 <- t(mycontests_2015, pop.2015)
mycontests_2016 <- t(mycontests_2016, pop.2016)
mycontests_2017 <- t(mycontests_2017, pop.2017)
mycontests_2018 <- t(mycontests_2018, pop.2018)
mycontests_2019 <- t(mycontests_2019, pop.2019)
mycontests_2020 <- t(mycontests_2020, pop.2020)
mycontests_2021 <- t(mycontests_2021, pop.2021)
mycontests_2022 <- t(mycontests_2022, pop.2022)

mycontests.list <- list(mycontests_2010,mycontests_2011,mycontests_2012,
                        mycontests_2013,mycontests_2014,mycontests_2015,
                        mycontests_2016,mycontests_2017,mycontests_2018,
                        mycontests_2019,mycontests_2020,
                       mycontests_2021,mycontests_2022)

mycontests <- rbind(mycontests_2010,mycontests_2011, mycontests_2012,
                    mycontests_2013,mycontests_2014,mycontests_2015,
                    mycontests_2016,mycontests_2017,mycontests_2018,
                    mycontests_2019,mycontests_2020,mycontests_2021,
                    mycontests_2022)

mycontests <- ungroup(mycontests)
sample_n(mycontests, 10)
mycontests <- mycontests %>% mutate(turnout= (total.votes/pop.estimate)*100)
nrow(mycontests)
finalcontests <- mycontests %>% filter(turnout<=100)
nrow(finalcontests)
mean(finalcontests$turnout)
mean(finalcontests$council.manager)
sum(finalcontests$council.manager)
418-309
## 309 council manager elections 109 non manager elections
sum(is.na(finalcontests$council.manager))
## all cities have values

write_csv(finalcontests, "data/turnoutrate.csv")


