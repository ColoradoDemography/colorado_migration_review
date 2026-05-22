infl_df <- fredr("CPIAUCSL") %>%
    mutate(YEAR = year(date)) %>%
    group_by(YEAR) %>%
    summarize(adj = mean(value)) %>%
    filter(YEAR >= 2016, YEAR <= 2024) %>%
    mutate(adj = last(adj)/adj)

ipums_df <- read_ipums_micro(read_ipums_ddi("./data/ipums/usa_00079.xml")) %>% 
    mutate(year_group = if_else(
        YEAR < 2020, "2016-2019", "2021-2024")) %>%
    filter(STATEFIP == 8) %>%
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
        )))

ipums_design_df <- as_survey(ipums_df, weights = "PERWT", strata = "YEAR")

ipums_design_df %>%
    group_by(year_group, INCOMEGROUP) %>%
    survey_tally() %>%
    mutate(moe = n_se * 1.645)


ipums_design_df %>%
    group_by(year_group, RACE2) %>%
    survey_tally() %>%
    mutate(moe = n_se * 1.645)

ipums_design_df %>%
    group_by(year_group, AGEGROUP) %>%
    survey_tally() %>%
    mutate(moe = n_se * 1.645)


