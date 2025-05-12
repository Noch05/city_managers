## ----------------------------------------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(stargazer)
library(MatchIt)
library(cobalt)
library(gridExtra)
library(grid)
library(maps)


## ----------------------------------------------------------------------------------------------------
#| output: false
cities <- read_csv("data/cities_constituency_data.csv")
elections <- readRDS("data/ledb_candidatelevel.rds")
set.seed(435464564)
sample <- sample_n(cities, 150, replace=FALSE)
cities <- cities %>% 
  mutate(sampled = if_else(geo_name %in% sample$geo_name, 1, 0))
elections <- elections %>%
  mutate(sampled = if_else(fips %in% sample$fips , 1 , 0))

sample <- read_csv("data/sample with manager.csv")
sample <- sample %>%
  mutate(across(c(council.manager, percent_women, percent_white,
                  percent_black, percent_hispanic),as.numeric))

elections$fips <- as.numeric(elections$fips)
sampled_elections <- left_join(sample, elections, by = "fips") %>%
  rename(city = geo_name.x) %>%
  select(fips, state, city, pres_pctD_08, pres_pctD_16, pres_pctD_20, council.manager, state_abb, office_consolidated, district, contest, votes, year, month, incumbent, vote_share) %>%
  filter(year > 2009)
nrow(sampled_elections)

sampled_contests <- sampled_elections %>%
  group_by(contest) %>%
  reframe(
    total.votes = sum(votes),
    year = year,
    city = city,
    office = office_consolidated,
    state = state,
    pres_pctD_08 = pres_pctD_08,
    pres_pctD_16 = pres_pctD_16,
    pres_pctD_20 = pres_pctD_20,
    council.manager = council.manager,
    month = month,
    incumbent = if_else(all(is.na(incumbent)), NA_real_, max(incumbent, na.rm = TRUE)),
    margin = {
      sorted_votes <- sort(votes, decreasing = TRUE)
      if(length(sorted_votes) < 2) {
        100
      }
      else {
        100*(sorted_votes[1] - sorted_votes[2])/(sorted_votes[1]+sorted_votes[2])
      }
    }
  ) %>% 
  filter(!duplicated(contest)) %>%
  filter(grepl("at-large", contest) | grepl("mayor", contest))

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

p <- function(x) {
  x %>% 
    rename(pop.estimate = DP05_0018E, 
           percent_men = DP05_0002PE,
           percent_women = DP05_0003PE,
           percent_white = DP05_0059PE,
           percent_black = DP05_0060PE,
           percent_hispanic = DP05_0066PE,
           percent_asian_american = DP05_0062PE) %>% 
    mutate(across(c(pop.estimate, percent_men, percent_women, percent_white, 
                    percent_black, percent_hispanic, percent_asian_american),
                  as.numeric)) %>%
    separate(NAME, into = c("city", "state"), sep = ",") %>%
    select(city, state, pop.estimate, percent_men, percent_women, percent_white, 
           percent_black, percent_hispanic, percent_asian_american) %>%
    drop_na()
}

s <- function(x) {
  x %>% 
    rename(pop.estimate = DP05_0021E, 
           percent_men = DP05_0002PE,
           percent_women = DP05_0003PE,
           percent_white = DP05_0064PE,
           percent_black = DP05_0065PE,
           percent_hispanic = DP05_0071PE,
           percent_asian_american = DP05_0067PE) %>% 
    mutate(across(c(pop.estimate, percent_men, percent_women, percent_white, 
                    percent_black, percent_hispanic, percent_asian_american), 
                  as.numeric)) %>%
    separate(NAME, into = c("city", "state"), sep = ",") %>%
    select(city, state, pop.estimate, percent_men, percent_women, percent_white, 
           percent_black, percent_hispanic, percent_asian_american) %>%
    drop_na()
}

t <- function(x, y) {
  x$city <- tolower(x$city)
  y$city <- tolower(y$city)
  y$state <- trimws(y$state)
  left_join(x,y, by= c("city", "state"))
}

poplist <- list(pop.2010, pop.2011, pop.2012, pop.2013, pop.2014, pop.2015,
                pop.2016, pop.2017, pop.2018, pop.2019, pop.2020, pop.2021)


sampled_contests_list <- vector("list", length = 12)

contest_by_year <- split(sampled_contests, sampled_contests$year)


for(i in 1:12) {
  if(i <= 7) {
    poplist[[i]] <- p(poplist[[i]]) 
  } else {
    poplist[[i]] <- s(poplist[[i]])  
  }
  sampled_contests_list[[i]] <- t(contest_by_year[[i]], poplist[[i]])
}

sampled_contests <- bind_rows(sampled_contests_list)

final_contests <- sampled_contests %>%
  mutate(turnout = (total.votes/pop.estimate)*100) %>%
  filter(turnout <= 100) %>% drop_na()

final_contests <- final_contests %>%
  mutate(
    log.pop = log(pop.estimate),
    concurrent = factor(
      case_when(
        year %% 4 == 0 & month == 11 ~ "president",
        year %% 4 == 2 & month == 11 ~ "midterm",
        year %% 4 == 3 & month == 11 & state %in% c("Louisiana") ~ "midterm",
        year %% 4 ==1 & month == 11 & state %in% c("New Jersey", "Virginia") 
        ~ "midterm",
        TRUE ~ "none"), levels = c("none", "midterm", "president")),
    gubernatorial = if_else(
      year %% 4 == 0 & month == 11 & state %in% c("Missouri", "Indiana",
                                                  "North Carolina") |
        year %% 4 == 3 & month == 11 & state %in% c("Louisiana") |
        year %% 4 == 1 & month == 11 & state %in% c("New Jersey", "Virginia") |
        year %% 4 == 2 & month == 11 & state %in% c(
          "Georgia", "Maine", "Michigan", "Oregon", 
          "Arizona", "Rhode Island", "Texas", 
          "Arkansas", "California", "Connecticut", 
          "Florida", "Illinois", "Iowa", "Kansas", 
          "Massachusetts", 
          "North Carolina", "South Carolina", "Ohio", "Pennsylvania", 
          "Colorado", "Minnesota", "Tennessee"),1,0),
    concurrent_pres = if_else(concurrent=="president", 1, 0),
    concurrent_mid = if_else(concurrent=="midterm",1,0),
    office = factor(office, levels = c("City Council", "Mayor")),
    mayor_elec = if_else(office=="Mayor",1,0),
    meanD = mean(c(pres_pctD_08,pres_pctD_16,pres_pctD_20)))

addition <- final_contests %>% group_by(city) %>% summarize(
  has_council = if_else(any(office=="City Council"),1,0),
  has_mayor = if_else(any(office=="Mayor"),1,0),
  same_months = if_else(all(month[office=="City Council"] %in% 
                              month[office=="Mayor"]),1,0 ),
  .groups = "drop") %>%
  filter(has_mayor & has_council) %>% select(same_months, city)

has_mayor <- final_contests %>% group_by(city) %>% summarize(
  has_mayor = if_else(any(office=="Mayor"),1,0), .groups = "drop")

final_contests <- left_join(final_contests, addition, by = "city")

final_contests <- left_join(final_contests, has_mayor, by = "city")

rm(pop.2010,pop.2011,pop.2012, pop.2013,pop.2014,pop.2015,pop.2016,
   pop.2017,pop.2018,pop.2019,pop.2020,pop.2021,poplist,sampled_contests_list,
   contest_by_year)
gc()

write_rds(final_contests, "data/final_contests.rds")




## ----------------------------------------------------------------------------------------------------
#| output: false

formula <- council.manager ~ concurrent+ has_mayor+ office+ log.pop+ incumbent + 
  margin + percent_women+ percent_black+percent_hispanic+ percent_asian_american
matching_methods <- c("nearest", "optimal", "full", "quick", "genetic")
distance_methods <- c("glm", "gam", "gbm", "lasso", "ridge", "elasticnet",
                      "rpart", "randomforest", "nnet", "cbps", 
                      "bart", "mahalanobis", "scaled_euclidean", "robust_mahalanobis")
balances <- list()
matching <- function(method, formula, data, cutpoints = NULL, grouping = NULL, 
                     subclass = NULL, distance, distance.options = NULL) {
  match <- matchit(formula, method = method, data = data, cutpoints = cutpoints, 
                   distance = distance, distance.options = distance.options) 
  bal <- bal.tab(match, stats = c("mean.diffs", "variance.ratios"), 
                 binary = "std", thresholds = c(m=0.1, v=2), un = TRUE)
  return(bal$Balance)
}


set.seed(125)
nnet <- list(size = 2)
for(methods in matching_methods) {
  for(distance in distance_methods) { 
    if(distance == "nnet") {
      set.seed(125)
      balances[[paste(methods, distance, sep = "_")]] <- matching(methods, 
                                                                  formula, final_contests, distance = distance, distance.options = nnet)
    }
    else {
      set.seed(125)
      balances[[paste(methods, distance, sep = "_")]]<- matching(methods, 
                                                                 formula, final_contests, distance = distance)
    }
  }
}
bal_result <- list()
for(i in seq_along(balances)) {
  m <- sum(if_else(balances[[i]]["M.Threshold"]=="Balanced, <0.1",1 , 0), 
           na.rm=TRUE)
  v <- sum(if_else(balances[[i]]["V.Threshold"]=="Balanced, <2", 1 , 0), 
           na.rm=TRUE)
  bal_result[[i]] <-tibble(method = paste(names(balances[i])), 
                           bal_mean = m,
                           bal_var = v)
  
}
bal_result <- bind_rows(bal_result)
my_pick <- bal_result %>% filter(bal_mean==max(bal_mean)) %>%
  filter(bal_var == max(bal_var)) %>% print()

balances[["optimal_glm"]]
balances[["optimal_gam"]]
tie_breaker <- tibble(
  model = c("optimal_glm", "optimal_gam"),
  un_balanced_mean_diff = 
    c(mean(c(abs(balances[["optimal_glm"]][7,4])-0.1, 
             abs(balances[["optimal_glm"]][11,4])-0.1)),
      mean(c(abs(balances[["optimal_gam"]][7,4])-0.1,
             abs(balances[["optimal_gam"]][11,4])-0.1))))


## ----------------------------------------------------------------------------------------------------
set.seed(125)
optimal_glm <- matchit(formula, method = "optimal", data = final_contests, distance = "glm")
matched_data <- match.data(optimal_glm)
write_rds(matched_data, "data/matched_data.rds")
write_rds(optimal_glm, "data/matched_model.rds")



## ----------------------------------------------------------------------------------------------------
test.vars <- c("population_2020", "mass_ideology_2020","pres_pctD_20",
               "pres_pctD_16", "pres_pctD_08",
               "percent_women", "percent_white", "percent_black", "percent_hispanic", 
               "percent_asian_american") 
qq_labels <- c("Population (2020)", "Ideology Measure (2020)", 
               "Dem. Vote 2020 (%)", 
               "Dem. Vote 2016  (%)",
               "Dem. Vote 2008  (%)",
               "Women (%)", "White (%)", "Black (%)", "Hispanic (%)", 
               "Asian (%)")
quantiles <- list()
qqplots <- list()

for (i in seq_along(test.vars)) {
  sample_q <- quantile(sample[[test.vars[i]]], probs = seq(0.01, 1, 0.01), 
                       na.rm = TRUE)
  pop_q <- quantile(cities[[test.vars[i]]], probs = seq(0.01, 1 , 0.01), 
                    na.rm = TRUE)
  quantiles[[i]] <- tibble(sample_q, pop_q, variable = qq_labels[i])
}

quantiles <- bind_rows(quantiles)


## ----------------------------------------------------------------------------------------------------
#| fig.cap: "QQ Plots for Sampled vs. Population Cities"
#| fig.width: 9
#| fig.height: 6
#| label: fig-qq
ggplot(quantiles, aes(sample_q, pop_q)) +
  geom_point()+
  geom_abline(slope = 1, intercept = 0, linetype = 2)+
  labs(x = "Sample Quantiles", y = "Population Quantiles")+
  facet_wrap(~variable, scale = "free", ncol =3)+
  theme_minimal()+
  theme(strip.text = element_text(size = 12),
        axis.text= element_text(size=10),
        axis.title = element_text(size=15))
ggsave("qq.png", last_plot(), create.dir = TRUE, path = "plots")




## ----------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Summary Statistics of Selected Covariates in Unmatched Sample"
#| label: tbl-sum
final_contests %>% select(turnout, council.manager, incumbent,margin, 
                          percent_women, percent_black , percent_hispanic , 
                          percent_asian_american, log.pop, concurrent_pres,
                          concurrent_mid,has_mayor,mayor_elec) %>%
  rename("Turnout(%)" = turnout,
         "Has a Manager" = council.manager,
         "Has a Mayor" = has_mayor, 
         "Mayoral Election" = mayor_elec, 
         "Incumbency" = incumbent,
         "Population(ln)" = log.pop,
         "Margin of Victory(%)" = margin,
         "Women(%)" = percent_women,
         "Black(%)" = percent_black,
         "Asian(%)" = percent_asian_american,
         "Hispanic(%)" = percent_hispanic,
         "Concurrent Presidential" = concurrent_pres,
         "Concurrent Midterm" =  concurrent_mid) %>%
  as.data.frame() %>%
  stargazer(summary = TRUE, header = FALSE, digits = 2,
            single.row = TRUE, column.sep.width = "0pt", font.size = "footnotesize",
            float = FALSE,  omit.summary.stat = "N", out = "plots/summary.txt")


## ----------------------------------------------------------------------------------------------------
#| fig.cap: "Model Building: Comparing Linear Terms to Loess Regression"
#| fig.height: 5
#| fig.width: 8
#| label: fig-lm
final_covs <- final_contests[, c("log.pop", 
                                 "margin", "percent_women","percent_black", 
                                 "percent_hispanic", "percent_asian_american")]
final_cov_lab <- c("Population (ln)", "Margin of Victory", "Women(%)", "Black (%)",
                   "Hispanic(%)", "Asian(%)")
coef_plots <- list()
for (i in seq_along(final_covs)) {
  coef_plots[[i]]<- ggplot(final_contests, 
                           aes_string(names(final_covs[i]), "turnout"))+
    geom_point()+
    theme_minimal()+ 
    labs(x = paste(final_cov_lab[i]),  y = "Turnout(%)")+
    geom_smooth(method = "lm", se = FALSE)+
    geom_smooth(se = FALSE)
}
grid.arrange(grobs = coef_plots, ncol = 3)
ggsave("Coef_plot.png",
       plot = arrangeGrob(grobs = coef_plots, ncol=3), path = "plots")


## ----------------------------------------------------------------------------------------------------
#| output: 'asis'
#| label: tbl-bal
#| tbl-cap: "Balance Table Results for Matching Procedure: Optimal GLM Method"
final_balances <- bal.tab(optimal_glm, stats = c("mean.diffs", "variance.ratios"), 
                          binary = "std", thresholds = c(m=0.1, v=2), un = TRUE)
balance_table <- as_tibble(final_balances[[1]]) %>% 
  mutate(Covariates = c("Propensity Score", "Not Concurrent", 
                        "Concurrent With Midterm","Concurrent with President", 
                        "Has a Mayor",
                        "Mayoral Election", "Population (ln)",
                        "Incumbent Prescence", "Margin of Victory", "Women (%)",
                        "Black (%)", "Hispanic (%)", "Asian (%)")) %>%
  select(Covariates, Type, Diff.Un, V.Ratio.Un, Diff.Adj,
         M.Threshold, V.Ratio.Adj, V.Threshold) %>% mutate(
           Diff.Un = round(Diff.Un, 4),
           V.Ratio.Un = round(V.Ratio.Un, 4),
           Diff.Adj = round(Diff.Adj, 4),
           V.Ratio.Adj = round(V.Ratio.Adj, 4))
stargazer(balance_table, summary = FALSE, header = FALSE,
          digits = 4, column.sep.width = "0pt", single.row = TRUE,
          rownames = FALSE, font.size = "footnotesize", float = FALSE, out = "plot/bal_tab.txt")


## ----------------------------------------------------------------------------------------------------
#| fig.cap: "Love Plot for Matched Covariates"
#| fig.width: 8
#| fig.height: 5
#| label: fig-love
bal_names <- list("Propensity Score", "Not Concurrent", "Concurrent With Midterm",
                  "Concurrent with President", "Has a Mayor",
                  "Mayoral Election", "Population (ln)",
                  "Incumbent Prescence", "Margin of Victory", "Women (%)",
                  "Black (%)", "Hispanic (%)", "Asian (%)")
names(bal_names) <- var.names(final_balances)

love <- love.plot(optimal_glm, stats = c("mean.diffs", "variance.ratios"), 
                  binary = "std", thresholds = c(m=0.1, v=2), un = TRUE,
                  var.names = bal_names, title = NULL)
ggsave("love_plot.png", plot = love, path = "plots")



## ----------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Summary Statistics of Selected Covariates in Matched Sample"
#| label: tbl-summ
matched_data %>% select(turnout, council.manager, incumbent,margin, 
                        percent_women, percent_black , percent_hispanic , 
                        percent_asian_american, log.pop, concurrent_pres,
                        concurrent_mid,has_mayor,mayor_elec) %>%
  rename("Turnout(%)" = turnout,
         "Has a Manager" = council.manager,
         "Has a Mayor" = has_mayor, 
         "Mayoral Election" = mayor_elec, 
         Incumbency = incumbent,
         "Population(ln)" = log.pop,
         "Margin of Victory(%)" = margin,
         "Women(%)" = percent_women,
         "Black(%)" = percent_black,
         "Asian(%)" = percent_asian_american,
         "Hispanic(%)" = percent_hispanic,
         "Concurrent Presidential" = concurrent_pres,
         "Concurrent Midterm" =  concurrent_mid) %>%
  as.data.frame() %>%
  stargazer( summary = TRUE, header = FALSE, digits = 2,
             single.row = TRUE, column.sep.width = "0pt", font.size = "footnotesize",
             float = FALSE, omit.summary.stat = "N", out = "plots/sum_match.txt")


## ----------------------------------------------------------------------------------------------------

lm_ref <- lm(turnout ~ council.manager, final_contests)
lm_full <- lm(turnout ~ council.manager + concurrent + has_mayor + office +
                incumbent + margin + percent_women + percent_black + log.pop + 
                percent_hispanic + percent_asian_american, data = final_contests)

lm_ref_matched <- lm(turnout ~ council.manager, data = matched_data, weights = weights)
lm_full_matched <- lm(turnout ~ council.manager + concurrent+ office + 
                        has_mayor + incumbent + margin + log.pop + 
                        percent_women + percent_black + percent_hispanic +
                        percent_asian_american, data = matched_data, weights = weights)




## ----------------------------------------------------------------------------------------------------
models <- list(lm_ref,lm_full, lm_ref_matched, lm_full_matched)
p.values <- vector()

for (i in seq_along(models)) {
  tidied <- tidy(models[[i]])  %>% filter(term == "council.manager") %>% select(p.value)
  p.values[i] <- as.vector(tidied)
}
adjusted <- p.adjust(p.values, method = "BH")


## ----------------------------------------------------------------------------------------------------
#| output: 'asis'
#| label: tbl-regression
#| tbl-cap: "Regression Model Results: With 95% Confidence Intervals"
stargazer(lm_ref, lm_full, lm_ref_matched, lm_full_matched,
          type= "latex", header = FALSE, ci = TRUE,
          ci.level = 0.95,
          df = TRUE, font.size = "footnotesize", 
          model.names = TRUE,
          column.sep.width = "0pt", digits = 2,
          star.cutoffs = c(0.05, NA, NA), notes = " *p<0.05", 
          notes.append = FALSE,
          covariate.labels = c("Manager", "Midterm Concurrency", "President Concurrency",
                               "Has a Mayor", "Mayoral Election", "Incumbent Presence", "Margin of Victory",
                               "Percent Women", "Percent Black", "Population(ln)", "Percent Hispanic", "Percent Asian"),
          dep.var.labels = "Voter Turnout",
          no.space = TRUE,
          single.row = TRUE,
          column.labels = c("Unmatched", "Matched"),
          column.separate = c(2,2), float = FALSE, out = "plots/reg_table.txt")

#| output: 'asis'
#| label: tbl-regression
#| tbl-cap: "Regression Model Results: With 95% Confidence Intervals"
stargazer(lm_full, lm_full_matched,
          type= "html", header = FALSE, ci = TRUE,
          ci.level = 0.95,
          df = TRUE, font.size = "footnotesize", 
          model.names = TRUE,
          column.sep.width = "0pt", digits = 2,
          star.cutoffs = c(0.05, NA, NA), notes = "*p<0.05, 95% CI in parentheses",
          notes.append = FALSE,
          covariate.labels = c("Manager", "Midterm Concurrency", "President Concurrency",
                               "Has a Mayor", "Mayoral Election", "Incumbent Presence", "Margin of Victory",
                               "Percent Women", "Percent Black", "Population(ln)", "Percent Hispanic", "Percent Asian"),
          dep.var.labels = "Voter Turnout",
          no.space = TRUE,
          single.row = TRUE,
          column.labels = c("Unmatched", "Matched"),
          column.separate = c(1,1), float = TRUE,
          keep.stat= c("n", "rsq", "f"),
          out = "plots/half_reg_table.txt")



## ----------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "List of Sampled Cities"
#| label: tbl-city

city <- final_contests %>% group_by(city) %>% reframe(city = city, state = state) %>%
  ungroup() %>% filter(!duplicated(city))

stargazer(city, summary = FALSE, header = FALSE, type = "latex", 
          font.size = "tiny", column.sep.width = "0pt",
          float = FALSE, out = "plots/city.txt")


## ----------------------------------------------------------------------------------------------------
#| fig.cap: "Map of Sampled Cities"
#| fig.width: 7
#| label: fig-map
places <- read_csv("data/places_geocoded.csv")
places$fips <- as.numeric(places$fips)
sample$geo_name <- tolower(sample$geo_name)
sampled_places <- final_contests %>%
  group_by(city) %>% reframe(geo_name = city,
                             state = state) %>% 
  ungroup() %>%
  filter(!duplicated(geo_name)) %>%
  left_join(sample, by = c("geo_name")) %>%
  left_join(places, by = "fips")
usa <- map_data("usa")
ggplot()+
  geom_map(map = usa)+
  borders(database = "state")+
  geom_point(aes(lng, lat),data = sampled_places, shape = 2)+
  theme_minimal()+
  labs(x = NULL, y = NULL)+
  theme(legend.position = "none")+
  scale_x_continuous(breaks = NULL)+
  scale_y_continuous(breaks = NULL)
ggsave("map.png", last_plot(), path = "plots")



## ----------------------------------------------------------------------------------------------------

all_covs <- final_contests[, c("council.manager","turnout", "has_mayor", "mayor_elec", "incumbent", "log.pop", 
                               "margin", "percent_men", "percent_women", "percent_white",
                               "percent_black", "percent_hispanic", "percent_asian_american",
                               "concurrent_pres", "concurrent_mid")]
names(all_covs) <-c("Has a Manager", "Turnout", "Has a Mayor", "Mayor Election", 
                    "Incumbency", "Population(ln)", "Margin", "Men(%)", "Women(%)",
                    "White(%)", "Black(%)", "Hispanic(%)", "Asian(%)", "Concurrent Pres.",
                    "Concurrent Mid.")
inst_cov <- all_covs[, c("Has a Manager", "Turnout", "Has a Mayor", "Mayor Election",
                         "Incumbency", "Margin")]
dem_covs <- all_covs[, c("Has a Manager", "Turnout", "Population(ln)", "Men(%)", "Women(%)",
                         "White(%)", "Black(%)", "Hispanic(%)", "Asian(%)")]
tim_covs <- all_covs[,c("Has a Manager", "Turnout", "Concurrent Pres.","Concurrent Mid.")]

inst_cors<- as.data.frame((cor(inst_cov)))
dem_cors <- as.data.frame((cor(dem_covs)))
tim_cors <- as.data.frame((cor(tim_covs)))


## ----------------------------------------------------------------------------------------------------
#| output: 'asis'
#| label: tbl-icor
#| tbl-cap: "Correlation Matrix of Institutional Covariates"
stargazer(inst_cors, summary = FALSE,  
          header = FALSE,
          digits = 2, single.row = TRUE,
          column.sep.width = "0pt", font.size = "footnotesize",
          float = FALSE, out = "plots/inst.txt")


## ----------------------------------------------------------------------------------------------------
#| output: 'asis'
#| label: tbl-dcor
#| tbl-cap: "Correlation Matrix of Demographic Covariates"
stargazer(dem_cors, summary = FALSE, 
          header = FALSE,
          digits = 2, single.row = TRUE,
          column.sep.width = "0pt", font.size = "scriptsize",
          float = FALSE, out = "plots/dem.txt")


## ----------------------------------------------------------------------------------------------------
#| output: 'asis'
#| tbl-cap: "Correlation Matrix of Timing Covariates"
#| label: tbl-tcor
stargazer(tim_cors, summary = FALSE, 
          header = FALSE,
          digits = 2, single.row = TRUE,
          column.sep.width = "0pt", font.size = "footnotesize",
          label = "tbl-tcor", float = FALSE, out = "plots/time.txt")


## ----------------------------------------------------------------------------------------------------
#| fig.cap: "Individual Distributional Balance for Matched Covariates"
#| fig.width: 10
#| fig.height: 7
#| label: fig-bal
bals <- c("incumbent", "percent_women", "percent_black", "percent_hispanic",
          "percent_asian_american", "log.pop", "concurrent", 
          "has_mayor", "distance")
bal_labs <- c("Incumbent", "Women (%)", "Black (%)", "Hispanic (%)", "Asian (%)", 
              "Population (ln)", "Election Concurrency", "Has a Mayor",
              "Propensity Score")

bal_plots <- list()


for(i in seq_along(bals)) {
  bal_plots[[i]] <- bal.plot(optimal_glm, var.name = bals[i], which = "both",
                             sample.names = c("Unmatched", "Matched"))+
    ggtitle(NULL) +
    labs(
      x = paste(bal_labs[i]), 
      y = ifelse(
        bal_labs[i] %in% c("Incumbent", "Election Concurrency", "Has a Mayor"), 
        "Proportion", 
        "Density")
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )
}
grid.arrange(grobs = bal_plots, nrow = 3)
ggsave("balance_plot.png", plot =
         arrangeGrob(grobs = bal_plots, nrow = 3), path = "plots")


## ----------------------------------------------------------------------------------------------------
#| fig.cap: "Residual Plots for the Multiple Regression Models"
#| label: fig-resid
#| fig.height: 3
#| fig.width: 5
resid_plot_un <- ggplot(augment(lm_full), aes(.fitted, .resid))+
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 2, color = "blue")+
  labs(title = "Unmatched", x = "Fitted Values", y = "Residuals")+
  theme_minimal()+
  ylim(c(-50,50))
resid_plot_ma <-ggplot(augment(lm_full_matched),
                       aes(.fitted, .resid))+
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 2, color = "blue")+
  labs(title = "Matched", x = "Fitted Values", y = "Residuals")+
  theme_minimal()+
  ylim(c(-50,50))
grid.arrange(resid_plot_un, resid_plot_ma, ncol = 2)


ggsave("residuals.png", plot = arrangeGrob(resid_plot_un,
                                           resid_plot_ma,ncol = 2), 
       path = "plots")

