library(tidyverse)
library(zoo)
library(xtable)
library(dampack)

# Data

#   Import world bank income groups

World_bank_categories <- read.csv('Data Used/world_bank_income_groups.csv')

World_bank_categories$iso_code <- World_bank_categories$CountryCode

Region <- World_bank_categories[(which(World_bank_categories$GroupCode %in% c("EAP","ECA","LAC","MNA","SSA","SAS"))),]

Region <- rename(Region,  "Region"="GroupName",
                 "RegionCode"="GroupCode")

#   "Our World in Data"- contains vaccine data and demographics by country

Vaccine <- read.csv('Data Used/owid-covid-data-2.csv')

Vaccine$date <- as.Date(Vaccine$date, "%Y-%m-%d")

Vaccine <- Vaccine[,c("iso_code","continent","location","date","total_vaccinations","people_vaccinated","people_fully_vaccinated",
                                                      "aged_65_older")]

# Cumulative deaths data by country


Deaths <- read.csv('Data Used/export_country_cumulative.csv')

Deaths$date <- as.Date(Deaths$date, "%Y-%m-%d")

Deaths$iso_code <- Deaths$iso3c

Deaths <- Deaths[,c("iso_code","date","population", "cumulative_estimated_daily_excess_deaths","cumulative_estimated_daily_excess_deaths_ci_95_top",
                                                   "cumulative_estimated_daily_excess_deaths_ci_95_bot","cumulative_daily_covid_deaths")]

# Merge datasets together

#   Merge vaccine data and deaths data by date and country code

#   Add region and income level data

Vaccine_Equity <- merge(x=Deaths, y=Region, by=("iso_code"), all.x=TRUE)

#Keep only latest date, and only those in a region of interest

Vaccine_Equity_latest <- Vaccine_Equity %>% 
  group_by(iso_code) %>%
  filter(date==max(date) & !is.na(Region))

# Merge in latest vaccination data

 Vaccine2 <- Vaccine %>%
   group_by(iso_code) %>%
   filter(!is.na(people_vaccinated) & !is.na(people_fully_vaccinated)) %>%
   filter(date==max(date))


Vaccine_Equity_latest <- merge(x=Vaccine_Equity_latest, y=Vaccine2, by="iso_code", all.x=TRUE)

# Create variable for # people over 65

Vaccine_Equity_latest$pop_over_65 <- (Vaccine_Equity_latest$aged_65_older/100) * Vaccine_Equity_latest$population

# Check missing values for people vaccinated and people over 65 - doesn't matter for this analysis, though.

#     [1] "Eritrea"                   "Micronesia, Fed. Sts."     "Marshall Islands"          "Korea, Dem. People's Rep."

# Vaccine_Equity_latest %>% 
#   group_by(CountryName) %>% 
#   filter(all(is.na(aged_65_older))) %>% 
#   pull(CountryName)

#     [1] "Eritrea"                   "Micronesia, Fed. Sts."     "Marshall Islands"         
#     [5] "Korea, Dem. People's Rep."  

Vaccine_Equity_latest %>% 
  group_by(CountryName) %>% 
  filter(all(is.na(people_vaccinated))) %>% 
  pull(CountryName)

#     [1] "Eritrea"                   "Micronesia, Fed. Sts."     "Marshall Islands"         
#     [5] "Korea, Dem. People's Rep." 

Vaccine_Equity_latest %>% 
  group_by(CountryName) %>% 
  filter(all(is.na(people_fully_vaccinated))) %>% 
  pull(CountryName)

# Remove countries with missing vaalues

Vaccine_Equity_latest <- Vaccine_Equity_latest %>%
  drop_na(people_vaccinated) %>%
  drop_na(people_fully_vaccinated)
  

# Create new dataset by region

Vaccine_equity_region <- Vaccine_Equity_latest %>%
  group_by(Region) %>%
  mutate(region_pop = sum(population), region_pop_65 = sum(pop_over_65), region_vaccinated = sum(people_vaccinated), 
         region_fully_vaccinated = sum(people_fully_vaccinated),
         region_covid_deaths = sum(cumulative_daily_covid_deaths), region_excess_deaths = sum(cumulative_estimated_daily_excess_deaths),
         region_excess_deaths_5 = sum(cumulative_estimated_daily_excess_deaths_ci_95_bot), region_excess_deaths_95 = sum(cumulative_estimated_daily_excess_deaths_ci_95_top)) %>%
  select(c("Region","RegionCode","region_pop","region_pop_65","region_vaccinated","region_fully_vaccinated","region_covid_deaths",
           "region_excess_deaths","region_excess_deaths_5","region_excess_deaths_95")) 

#   Keep only unique regions

Vaccine_equity_region <- distinct(Vaccine_equity_region)

#write.csv(Vaccine_equity_region, file="~/Documents/Rotations/Gonsalves/Results/Vaccine_equity_for_SA_12.13.21.csv", row.names = FALSE)

#   Create a new variable that estimates the number infected based off deaths numbers...

#   The IFR can (and should) be changed in sensitivity analyses etc: this is the infection fatality rate and will
#   allow us to back-calculate # infections

IFR = 5/1000
vaccine_mortality_efficacy= 0.9
cost_of_dose = 7
percent_excess_covid = 1
natural_immunity = 0.5


Vaccine_equity_region <- Vaccine_equity_region %>%
  mutate(infection_percent_covid_deaths = (region_covid_deaths/IFR)/region_pop,
         infection_percent_excess_deaths = ((region_excess_deaths*percent_excess_covid)/IFR)/region_pop,
         infection_percent_excess_deaths_lower5 = ((region_excess_deaths_5*percent_excess_covid)/IFR)/region_pop,
         infection_percent_excess_deaths_upper5 = ((region_excess_deaths_95*percent_excess_covid)/IFR)/region_pop)

#   Percent vaccinated

Vaccine_equity_region<- Vaccine_equity_region %>%
  mutate(vaccinated_percent = region_vaccinated / region_pop,
         fully_vaccinated_percent = region_fully_vaccinated / region_pop)

# Calculate number vaccine doses needed to fully vaccinate

Vaccine_equity_region <- Vaccine_equity_region %>%
  mutate(vaccine_needed = (vaccinated_percent - fully_vaccinated_percent)*region_pop + 2* (1-vaccinated_percent)*region_pop + region_pop)

#write.csv(Vaccine_equity_region, file="~/Documents/Rotations/Gonsalves/Results/Vaccine_equity_for_SA.csv", row.names = FALSE)



# Calculate number of deaths that could be averted by full vaccination:
#   1. Number at risk is anyone who has been infected

# Vaccine_equity_region1 <- Vaccine_equity_region %>%
#   mutate(number_at_risk_covid_deaths1 = (1-infection_percent_covid_deaths)*region_pop,
#          number_at_risk_excess_deaths1 = (1-infection_percent_excess_deaths)*region_pop,
#          number_at_risk_excess_deaths_lower51 = (1-infection_percent_excess_deaths_lower5)*region_pop,
#          number_at_risk_excess_deaths_upper51 = (1-infection_percent_excess_deaths_upper5)*region_pop) %>%
#   mutate(deaths_averted_covid_percent= number_at_risk_covid_deaths1*IFR*vaccine_mortality_efficacy,
#          deaths_averted_excess_percent= number_at_risk_excess_deaths1*IFR*vaccine_mortality_efficacy,
#          deaths_averted_excess_lower5_percent= number_at_risk_excess_deaths_lower51*IFR*vaccine_mortality_efficacy,
#          deaths_averted_excess_upper5_percent= number_at_risk_excess_deaths_upper51*IFR*vaccine_mortality_efficacy)

#   2. Number at risk is anyone who has been infected or vaccinated

Vaccine_equity_region1 <- Vaccine_equity_region %>%
  mutate(number_at_risk_covid_deaths2 = (1-((infection_percent_covid_deaths*fully_vaccinated_percent)+
                                              (infection_percent_covid_deaths-infection_percent_covid_deaths*fully_vaccinated_percent)*natural_immunity +
                                        (1-infection_percent_covid_deaths)*fully_vaccinated_percent))*region_pop,

         number_at_risk_excess_deaths2 = (1-((infection_percent_excess_deaths*fully_vaccinated_percent)+
                                               (infection_percent_excess_deaths-infection_percent_excess_deaths*fully_vaccinated_percent)*natural_immunity+
                                               (1-infection_percent_excess_deaths)*fully_vaccinated_percent))*region_pop,

         number_at_risk_excess_deaths_lower52 = (1-((infection_percent_excess_deaths_lower5*fully_vaccinated_percent)+
                                                      (infection_percent_excess_deaths_lower5-infection_percent_excess_deaths_lower5*fully_vaccinated_percent)*natural_immunity+
                                                      (1-infection_percent_excess_deaths_lower5)*fully_vaccinated_percent))*region_pop,

         number_at_risk_excess_deaths_upper52 = (1-((infection_percent_excess_deaths_upper5*fully_vaccinated_percent)+
                                                      (infection_percent_excess_deaths_upper5-infection_percent_excess_deaths_upper5*fully_vaccinated_percent)*natural_immunity+
                                                      (1-infection_percent_excess_deaths_upper5)*fully_vaccinated_percent))*region_pop) %>%

  mutate(number_at_risk_covid_deaths2=ifelse(number_at_risk_covid_deaths2<0, 0, number_at_risk_covid_deaths2),
         number_at_risk_excess_deaths2=ifelse(number_at_risk_excess_deaths2<0, 0, number_at_risk_excess_deaths2),
         number_at_risk_excess_deaths_lower52=ifelse(number_at_risk_excess_deaths_lower52<0, 0, number_at_risk_excess_deaths_lower52),
         number_at_risk_excess_deaths_upper52=ifelse(number_at_risk_excess_deaths_upper52<0, 0, number_at_risk_excess_deaths_upper52)) %>%

  mutate(deaths_averted_covid_percent= number_at_risk_covid_deaths2*IFR*vaccine_mortality_efficacy,
         deaths_averted_excess_percent= number_at_risk_excess_deaths2*IFR*vaccine_mortality_efficacy,
         deaths_averted_excess_lower5_percent= number_at_risk_excess_deaths_lower52*IFR*vaccine_mortality_efficacy,
         deaths_averted_excess_upper5_percent= number_at_risk_excess_deaths_upper52*IFR*vaccine_mortality_efficacy) %>%

  mutate(cost_per_death_averted_covid_death = (cost_of_dose*vaccine_needed)/deaths_averted_covid_percent,
         cost_per_death_averted_excess_death = (cost_of_dose*vaccine_needed)/deaths_averted_excess_percent,
         cost_per_death_averted_excess_death_lower5 = (cost_of_dose*vaccine_needed)/deaths_averted_excess_lower5_percent,
         cost_per_death_averted_excess_death_upper5 = (cost_of_dose*vaccine_needed)/deaths_averted_excess_upper5_percent)

#   3. Number at risk is anyone who has been vaccinated

Vaccine_equity_region1 <- Vaccine_equity_region1 %>%
  mutate(number_at_risk_vaccination = (1-fully_vaccinated_percent)*region_pop) %>%
  mutate(number_at_risk_vaccination=ifelse(number_at_risk_vaccination<0,0,number_at_risk_vaccination)) %>%
  mutate(deaths_averted_vaccination= number_at_risk_vaccination*IFR*vaccine_mortality_efficacy) %>%
  mutate(cost_per_death_averted_vaccination = (cost_of_dose*vaccine_needed)/deaths_averted_vaccination)

# Final table dataset

#write.csv(Vaccine_equity_region1, file="~/Documents/Rotations/Gonsalves/Results/Vaccine_equity_fullresults_12.2.21.csv", row.names = FALSE)

Vaccine_equity_region_final <- Vaccine_equity_region1[,c("Region","region_pop","region_pop_65","infection_percent_covid_deaths",
                                                        "infection_percent_excess_deaths","infection_percent_excess_deaths_lower5",
                                                        "infection_percent_excess_deaths_upper5","vaccinated_percent","fully_vaccinated_percent",
                                                        "vaccine_needed","deaths_averted_covid_percent","deaths_averted_excess_percent",
                                                        "deaths_averted_excess_lower5_percent","deaths_averted_excess_upper5_percent",
                                                        "cost_per_death_averted_covid_death","cost_per_death_averted_excess_death",
                                                        "cost_per_death_averted_excess_death_lower5","cost_per_death_averted_excess_death_upper5",
                                                        "deaths_averted_vaccination","cost_per_death_averted_vaccination")]

sum(Vaccine_equity_region_final$deaths_averted_vaccination)

Vaccine_equity_region_final1 <- Vaccine_equity_region_final


colnames(Vaccine_equity_region_final1) <- c("Region","Population","Population over 65","Percent previously infected, based on reported COVID deaths and IFR",
                                              "Percent previously infected, based on excess deaths","Percent previously infected, based on excess deaths lower 95% CI",
                                              "Percent previously infected, based on excess deaths upper 95% CI","Percent vaccinated","Percent fully vaccinated",
                                              "Number vaccine doses needed to vaccinate 100%","Deaths averted through 100% vaccination, based on infection numbers from reported COVID deaths",
                                              "Deaths averted through 100% vaccination, based on infection numbers from excess deaths",
                                              "Deaths averted through 100% vaccination, based on infection numbers from excess deaths lower 95% CI",
                                              "Deaths averted through 100% vaccination, based on infection numbers from excess deaths upper 95% CI",
                                              "Cost per death averted, based on infection numbers from reported COVID deaths",
                                              "Cost per death averted, based on infection numbers from excess deaths",
                                              "Cost per death averted, based on infection numbers from excess deaths lower 95% CI",
                                              "Cost per death averted, based on infection numbers from excess deaths upper 95% CI",
                                              "Deaths averted through 100% vaccination, based on vaccination numbers",
                                              "Cost per death averted, based on only vaccination numbers")

write.csv(Vaccine_equity_region_final1, file="Results/Vaccine_equity_allregions_12.20.21_um_secondbase_omicron.csv", row.names = FALSE)





