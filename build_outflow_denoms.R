rm(list = ls())
library(tidyverse)
library(ipumsr)
library(srvyr)
library(fredr)

infl_df <- fredr("CPIAUCSL") %>%
    mutate(YEAR = year(date)) %>%
    group_by(YEAR) %>%
    summarize(adj = mean(value, na.rm = TRUE)) %>%
    filter(YEAR >= 2016, YEAR <= 2025) %>%
    mutate(adj = last(adj)/adj)

# information about migration pumas can be found here
# https://usa.ipums.org/usa/volii/20migpuma.shtml
migpuma_name_vec <- c(
    "800100" = "NW Colorado", 
    "800200" = "Moutain N Colorado", 
    "800300" = "Larimer", 
    "800490" = "Denver MSA", 
    "801000" = "Weld", 
    "801800" = "NE Colorado", 
    "801900" = "SE Colorado", 
    "802007" = "El Paso", 
    "802190" = "Pueblo", 
    "802200" = "SW Colorado", 
    "802490" = "Mesa",
    "999999" = "Out of State"
)



ipums_df <- "G:/Shared drives/State Demography Office/#Staff/Neal/" %>%
    str_c("IPUMS/usa_00082.xml") %>%
    read_ipums_ddi() %>%
    read_ipums_micro() %>% 
    mutate(year_group = if_else(
        YEAR < 2020, "2016-2019", "2021-2024")) %>%
    filter(STATEFIP == 8 | MIGPLAC1 == 8) %>%
    mutate(AGEGROUP = age_to_agegroup(AGE, c(0, 18, 30, 45, 65, Inf))) %>%
    mutate(RACE2 = case_when(
        HISPAN > 1 ~ "Hispanic",
        RACE == 1 ~ "White",
        RACE == 2 ~ "Black",
        RACE == 3 ~ "AIAN",
        RACE <= 6 ~ "API",
        RACE == 7 ~ "Other",
        TRUE ~ "2 or more"
    )) %>%
    left_join(infl_df) %>%
    mutate(HHI = as.numeric(HHINCOME)*adj) %>%
    mutate(INCOMEGROUP = cut(
        HHI, breaks = c(-Inf, .1, 25000, 50000, 100000, 150000, 250000, Inf),
        labels = c(
            "No/Neg Income", "Under 25k", "25-50k", "50-100k", "100-150K",
            "150-250K", ">250K"
        ))) %>%
    # only care about migrants in this analysis
    filter(MIGPUMA1 != MIGPUMANOW & MIGPUMA1 != 0) %>%
    mutate(ALTMIGPUMA1 = if_else(
        # if they were in a state other than Colorado last year
        # give them a state assignment rather than a puma assignment
        MIGPLAC1 != 8, MIGPLAC1, MIGPUMA1 + 800000
    )) %>%
    mutate(ALTMIGPUMANOW = if_else(
        # if they are in a state other than Colorado this year
        # give them a state assignment rather than a puma assignment
        STATEFIP != 8, STATEFIP, MIGPUMANOW + 800000
    )) %>%
    mutate(CALTMIGPUMA1 = if_else(
        # if they were in a state other than Colorado last year
        # give them a generic assignment rather than a puma assignment
        MIGPLAC1 != 8, 999999, MIGPUMA1 + 800000
    )) %>%
    mutate(CALTMIGPUMANOW = if_else(
        # if they are in a state other than Colorado this year
        # give them a generic assignment rather than a puma assignment
        STATEFIP != 8, 999999, MIGPUMANOW + 800000
    )) %>%
    mutate(CALTMIGPUMA1 = migpuma_name_vec[as.character(CALTMIGPUMA1)]) %>%
    mutate(CALTMIGPUMANOW = migpuma_name_vec[as.character(CALTMIGPUMANOW)])


ipums_design_df <- as_survey(ipums_df, weights = "PERWT", strata = "YEAR")

state_flow_age_df <- ipums_design_df %>%
    mutate(dir = case_when(
        CALTMIGPUMANOW == "Out of State" ~ "Out",
        CALTMIGPUMA1 == "Out of State" ~ "In",
        TRUE ~ NA_character_
        )) %>%
    filter(MIGPUMA1 > 2) %>%
    filter(!is.na(dir)) %>%
    group_by(YEAR, dir, AGEGROUP) %>%
    survey_tally() %>%
    mutate(moe = n_se * 1.645) %>%
    ungroup() %>%
    mutate(lwr = n-moe, upr = n+moe)

state_flow_age_df %>%
    ggplot(aes(x = YEAR, y = n, ymin = lwr, ymax = upr, fill = dir)) +
    geom_col(position = "dodge") +
    geom_errorbar(
        aes(ymin = lwr, ymax = upr), position = position_dodge(width = 0.9)) +
    facet_wrap(~AGEGROUP, scales = "free_y")


flow_df <- ipums_design_df %>%
    group_by(YEAR, CALTMIGPUMA1, CALTMIGPUMANOW) %>%
    survey_tally() %>%
    mutate(moe = n_se * 1.645) %>%
    ungroup() %>%
    filter(YEAR >= 2022) %>%
    mutate(lwr = n-moe, upr = n+moe)

agg_flow_df <- ipums_design_df %>%
    filter(YEAR >= 2022) %>% 
    group_by(CALTMIGPUMA1, CALTMIGPUMANOW) %>%
    survey_tally() %>%
    mutate(moe = n_se * 1.645) %>%
    ungroup() %>%
    mutate(lwr = n-moe, upr = n+moe)

ipums_design_df %>%
    # lets only look at denver in or out
    filter(CALTMIGPUMA1 == "Denver MSA" | CALTMIGPUMANOW == "Denver MSA") %>%
    # only years beyond 2022 for newer MPUMA definitions
    filter(YEAR >= 2022) %>% 
    # we can then make this relative to denver
    mutate(Direction = if_else(CALTMIGPUMA1 == "Denver MSA", "Out", "In")) %>%
    group_by(YEAR, Direction, AGEGROUP) %>%
    survey_tally() %>%
    mutate(moe = n_se * 1.645) %>%
    ungroup() %>%
    mutate(lwr = n-moe, upr = n+moe) %>%
    ggplot(aes(x = AGEGROUP, y = n, fill = Direction)) +
    geom_col(position = "dodge") +
    geom_errorbar(aes(ymin = lwr, ymax = upr), position = position_dodge(width = 0.9)) +
    facet_wrap(~YEAR) +
    ggtitle("Migration to Denver MSA")

flow_df %>%
    filter(CALTMIGPUMA1 == "Denver MSA") %>%
    mutate(Direction = "Out") %>%
    mutate(NAME = CALTMIGPUMANOW) %>%
    bind_rows(
        flow_df %>%
            filter(CALTMIGPUMANOW == "Denver MSA") %>%
            mutate(Direction = "In") %>%
            mutate(NAME = CALTMIGPUMA1)
    ) %>%
    ggplot(aes(x = YEAR, y = n, fill = Direction)) +
    geom_col(position = "dodge") +
    geom_errorbar(aes(ymin = lwr, ymax = upr), position = position_dodge(width = 0.9)) +
    facet_wrap(~NAME, scales = "free_y") +
    ggtitle("Migration to Denver MSA")



