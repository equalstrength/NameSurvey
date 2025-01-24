###############################################################################
# EqualStrength Name Survey - Data Preparation
###############################################################################

# Loading required packages
library(tidyverse) # for data manipulation
library(haven) # to import/export from/to SPSS/STATA formats

###############################################################################
# 1) Importing dictionary of the raw national datasets ########################

# This file contains the expected information (e.g. country of origin, sex..)
# for each of the names tested in the survey
tb_dict <- read_csv("./data/raw/tb_dictionary_full.csv")

# Removing empty spaces before or after the name
tb_dict$name  <- str_trim(tb_dict$name)

# Adjusting name of country variable column
tb_dict$country_code  <- str_remove(tb_dict$country_code, "Name1_")

# Correcting country code for Bangladesh
tb_dict$country_code <- if_else(tb_dict$country_name == "Bangladesh", "V001c_48", tb_dict$country_code)

###############################################################################
# 2) Importing datafiles ######################################################

# Generating list of files in 'long format (ending with 'long.sav' or 'Datalong') 
ls_round_1 <- list.files("./data/raw/round1", recursive = TRUE, pattern = "\\wide.sav$|Datawide*.|SWI", full.names=TRUE)

# Importing  all files from the list and binding all rows into one dataset
df_round_1 <- map_dfr(ls_round_1, read_sav) |> mutate(round = 1)

# Generating list of files sent in the second Round
ls_round_2 <- list.files("./data/raw/round2", recursive = TRUE, full.names=TRUE)

# Removing path to Spanish file as it is already in the long format
spain_file <- "./data/raw/round2/ES_DATA_Long.sav"
ls_round_2 <- setdiff(ls_round_2, spain_file)

# Importing  all files from the second Round
df_round_2 <- map_dfr(ls_round_2, read_sav) |> mutate(round = 2)

# Removing test entries in Germany and the UK
df_round_2 <- df_round_2 |> filter(!Name1 %in% c('keine', ""))

# Final raw  dataset binding rows
df_raw <- bind_rows(df_round_1, df_round_2)

# 3) Transforming from wide to long ###########################################
# Each row refers to a respondent (wide format). This code will "break" the dataset
# and then "pile" them so each row refers to a name (each respondent assessed 10 names)
###############################################################################

# Creating a unique ID (sequential within countries)--------------------------
df_raw <- df_raw |> group_by(Country) |> mutate(id = paste0(Country, cur_group_rows())) |> ungroup()


# Function to extract specific columns of each name-------------------------
get_onename <- function(name_number){
    df_raw |>
        rename(Name = paste0("Name", name_number)) |>
        select(id, Name, starts_with(paste0("Name", name_number, "_"))) |>
        rename_with(~str_remove(., paste0("Name", name_number, "_")))
}

# Applying function to all 10 names and joining with metadata ----------------
df_long <- 
    df_raw |> 
        select(!starts_with("Name")) |>
        right_join(map_dfr(1:10, get_onename), by = "id")


# Correcting the spelling of names in SPSS files -----------------
df_long <-
    df_long |>
        mutate(Name = case_match(
            Name,
            "Ayla Sahin" ~ "Ayla Şahin",
            "Ilyas Sahin" ~ "Ilyas Şahin",
            "Ayse Kaya" ~ "Ayşe Kaya",
            "Ay?e Kaya" ~ "Ayşe Kaya",
            "Hatice Sahin" ~ "Hatice Şahin",
            "Hatice ?ahin" ~ "Hatice Şahin",
            "Mehmet Sahin" ~ "Mehmet Şahin",
            "Mehmet ?ahin" ~ "Mehmet Şahin",
            "Adriana Lakatos?ová" ~ "Adriana Lakatošová",
            "Adriana Lakatošová" ~ "Adriana Lakatošová",
            "Ernest Lakatos?" ~ "Ernest Lakatoš",
            "Jan Dvorák" ~ "Jan Dvořák",
            "Jana Cerná" ~ "Jana Černá",
            "Jirí Kucera" ~ "Jiří Kučera",
            "Josef Cerný" ~ "Josef Černý",
            "Katerina Procházková" ~ "Kateřina Procházková",
            "Kate?ina Procházková" ~ "Kateřina Procházková",
            "Lucie Kucerová" ~ "Lucie Kučerová",
            "Marie Dvoráková" ~ "Marie Dvořáková",
            "Marie Dvo?áková" ~ "Marie Dvořáková",
            "René Gaz?i" ~ "René Gaži",
            "Samanta Ginová" ~ "Samanta Giňová",
            "Samanta Gi?ová" ~ "Samanta Giňová",
            "Santiago Gi?a" ~ "Santiago Giňa",
            "Santiago Gina" ~ "Santiago Giňa",
            "Vanesa Gaz?iová" ~ "Vanesa Gažiová",
            "Kiss Gergo" ~ "Kiss Gergő",
            "Kiss Gerg?" ~ "Kiss Gergő",
            "Kolompár Dezso" ~ "Kolompár Dezső",
            "Kolompár Dezs?" ~ "Kolompár Dezső",
            "Rézmuves Géza" ~ "Rézműves Géza",
            "Rézm?ves Géza" ~ "Rézműves Géza",
            "Rézmuves Marianna" ~ "Rézműves Marianna",
            "Rézm?ves Marianna" ~ "Rézműves Marianna",
            .default = Name
        ))

# Removing input error in Fillin time -----------------

df_long$Filltime_Total = if_else(df_long$Filltime_Total > 150000, NA, df_long$Filltime_Total)


# Adding observations from Spain that are already in long format =)
df_csic <- read_sav(spain_file)

# Adjusting names/variable formats
names(df_csic) <- str_remove(names(df_csic), "Name1_")
df_csic <- df_csic |> rename(Name = Name1)
df_csic <- df_csic |> select(input_1:V001c_52, Weging)
df_csic$VS5b_1 <- as.character(df_csic$VS5b_1)
df_csic$VS5b_2 <- as.character(df_csic$VS5b_2)

# binding rows of the main international and Spanish datasets
df_long <- bind_rows(df_long, df_csic)

# 4) Merging dictionary and dataset ###########################################
###############################################################################
df_es <- 
    df_long |>
        mutate(country_survey = haven::as_factor(Country))  |>
        left_join(tb_dict, by = c(c("Name" = "name"), "country_survey"))

# 5) Creating columns with congruence #######################################
# This compares the response given in the survey with the response expected
# according to the dictionary

# Sex -------------------------------------------------------------------
# If gender-neutral or DK then not-congruent (cong_sex)
df_es$cong_sex <- if_else(df_es$V001a == df_es$sex, 1, 0)

# Region -------------------------------------------------------------------
# Including Pakistan as congruent only if South Asia
df_es$cong_region <- if_else(df_es$V001b == df_es$region, 1, 0)

# Religion -------------------------------------------------------------------
df_es$cong_religion <- if_else(df_es$V001e == df_es$religion, 1, 0)


# Country -------------------------------------------------------------------
# Check if the column marked Yes is the column for the expected country
df_es <- 
    df_es |>
    group_by(country_survey, country_name) |>
    mutate(cong_country = if_else(eval(as.name(country_code)) == 1, 1, 0, missing = 0)) |>
    ungroup()

# With Bangladesh and Pakistan equivalent for congruence
df_es <-
    df_es |>
    group_by(country_survey, country_name) |>
    mutate(cong_country_PKBD = case_when(
        eval(as.name(country_code)) == 1 ~ 1,
        country_name == "Pakistan" & V001c_48 == 1 ~ 1,
        country_name == "Bangladesh" & V001c_46 == 1 ~ 1,
        .default = 0)) |>
    ungroup()

# Ethnicity -------------------------------------------------------------------
df_es <- 
    df_es |>
    mutate(cong_ethn = case_when(
        country_survey %in% c("Czech Republic", "Hungary", "Ireland") &
            V001caHUNCZ == ethnicity ~ 1,
        country_survey %in% c("Czech Republic", "Hungary", "Ireland") &
            V001caHUNCZ != ethnicity ~ 0,
        country_survey == "Spain" & VRoma == ethnicity ~ 1,
        country_survey == "Spain" & VRoma != ethnicity ~ 0,
        .default = NA
    )) 


# # Number of countries 
df_es <-
    df_es |>
        mutate(cong_n_countries = rowSums(across(starts_with("V001c_")), na.rm = T))


# 6) Transforming variables #######################################

# Inverting scale for ladder and assigning missing value to DK
# DK is either 8 (most countries) or 99 (Spain)
# Spain has the scale already inverted
# New scale goes from 0 (low) to 1 (high)
df_es <- df_es |> mutate(ladder_adj = case_when(
    country_survey == "Spain" & V001h < 98 ~ V001h/7,
    country_survey != "Spain" & V001h < 8 ~ (8 - V001h)/7,
    V001h %in% c(8, 99) ~ NA,
    .default = NA
))

# Adjusting variable skin colour 
# number 10 was mistakenly assigned to DK in round 1
# New scale goes from 0.1 to 1
df_es  <- df_es |> mutate(skin_adj = case_when(
    round == 2 & V001db == 11 ~ NA,
    round == 1 ~ NA,
    .default = V001db / 10
))

# Adjusting variable fluency 
# New scale goes from 0 (low) to 1 (high)
df_es  <- df_es |> mutate(fluency = if_else(V001gaa_1 == 8, NA, V001gaa_1 / 7))


# Adjusting variable religiosity 
# New scale goes from 0 (low) to 1 (high)
df_es  <- df_es |> mutate(religiosity = if_else(V001fa_1 == 8, NA, V001fa_1 / 7))


# Creating variable for EqualStrength region

df_es  <-  df_es |>
    mutate(region_es = case_when(
    country_name %in% c("Congo", "Nigeria", "Senegal") ~ "SSA",
    country_name %in% c("Morocco", "Pakistan", "Turkey") ~ "MENAP",
    country_survey == "Czech Republic" & country_name == "Czech" ~ "Majority",
    country_survey == "The Netherlands" & country_name == "Netherlands" ~ "Majority",
    country_survey == "Switzerland" & country_name %in% c("CH-DE", "CH-FR") ~ "Majority",
    country_survey == country_name ~ "Majority",
    .default = "Other"
))


# Creating score and rank for each observation --------------


# Get weighted mean using the survey weight variable
get_mean <- function(var_es, wgt_es){
    weighted.mean({{var_es}}, w = {{wgt_es}}, na.rm = TRUE)
}

# Function to get composite score
get_score <- function(var_gnd, var_oth, wgt_es){
  wgnd <- 0.8 # Weight for the gender variable
  woth <- 2 - wgnd # Weight for the other variable (n-wgnd)
  comp1 <- wgnd * get_mean(var_gnd, wgt_es)
  comp2 <- woth * get_mean(var_oth, wgt_es)
  score <- (comp1 + comp2) / 2

  return(score)
}

# Adding score and rank to each observation
tb_score <-
  df_es |>
  filter(sex != 3) |> # 270 observations in NL excluded
  group_by(country_survey, region_es, country_name, sex, Name) |>
      summarise(
        score_skn = get_score(cong_sex, skin_adj, Weging),
        score_rlg = get_score(cong_sex, cong_religion, Weging),
        score_cnt = get_score(cong_sex, cong_country, Weging),
        score_ethn = get_score(cong_sex, cong_ethn, Weging),
        .groups = "keep") |> 
      mutate(score = case_when(
                country_survey %in% c("Czech Republic", "Hungary") ~ score_ethn,
                country_name %in% c("Dutch Antilles", "Surinam") ~ score_skn,
                region_es == "SSA" ~ score_skn,
                region_es == "MENAP" ~ score_rlg,
                region_es == "Majority" ~ score_cnt,
                region_es == "Other" ~ score_ethn,
                .default = NA)) |>
      group_by(country_survey, country_name, sex) |>
      mutate(rank = dense_rank(desc(score))) |>
      ungroup() |>
      select(country_survey, Name, score, rank)


df_es <- df_es |> left_join(tb_score, by = c("Name", "country_survey"))

# Combining respondents' region into one variable

df_es <- 
    df_es |> mutate(VS4_Region = case_when(
        !is.na(VS4BE) ~ paste0("BE_", as_factor(VS4BE)),
        !is.na(VS4DE) ~ paste0("DE_", as_factor(VS4DE)),
        !is.na(VS4ES) ~ paste0("ES_", as_factor(VS4ES)),
        !is.na(VS4HUN) ~ paste0("HU_", as_factor(VS4HUN)),
        !is.na(VS4IER) ~ paste0("IE_", as_factor(VS4IER)),
        !is.na(VS4NL) ~ paste0("NL_", as_factor(VS4NL)),
        !is.na(VS4UK) ~ paste0("UK_", as_factor(VS4UK)),
        !is.na(VS4ZWI) ~ paste0("CH_", as_factor(VS4ZWI)),
        !is.na(VS4CZ) ~ paste0("CZ_", as_factor(VS4CZ))
    ))

###############################################################################
# 7) Removing country-specific columns and observations ######################

# Variables ----------------------------------------------

#Switzerland
swi_cols <- c(
    "V002Swi1",
    "V002Swi2",
    "V002Swi3",
    "V002Swi4",
    "V002Swi5",
    "V002Swi6",
    "V002Swic",
    "V002Swid",
    "V002Swie",
    "V002Swif",
    "V002Swif_o",
    "V002Swigaa_1",
    "V002Swih"
)
df_es <- select(df_es, setdiff(names(df_es), swi_cols))

# Regions (keeping single variable VS4_region)
region_cols <- c("VS4BE", "VS4DE","VS4ES", "VS4HUN", "VS4IER", "VS4NL", "VS4UK", "VS4ZWI", "VS4CZ" )
df_es <- select(df_es, setdiff(names(df_es), region_cols))


# Unadjusted variables
unadjusted_cols <- c("V001db", "V001h", "V001gaa_1", "Leeftijd3N","religion", "sex", "Filltime",
                "VS5b_1", "VS5b_2", "VS5b_o", "region", "Country", "country_code", "score", "rank")
df_es <- select(df_es, setdiff(names(df_es), unadjusted_cols))

# Congruence and country-specific variables
cong_country_cols <- c("cong_country_PKBD", "cong_n_countries", "V001caHUNCZ", 
                            "V001caHUNCZ_o", "V001caUK_1", "V001caUK_2")
df_es <- select(df_es, setdiff(names(df_es), cong_country_cols))

# Observations ----------------------------------------------

df_es <- 
    df_es |> 
        filter(!country_name %in% c(
            "Bangladesh", "Black Caribbean",
            "Dutch Antilles", "Surinam",
            "Irish traveller"
        ))


###############################################################################
# 8) Renaming columns  ########################################################

country_cols <- paste0("V001c_", 1:51)

country_labels <- map_vec(country_cols, ~paste0("V001c_", gsub(" ", "", attributes(df_es[[.x]])$label)))

df_es <- df_es |> rename_with(~all_of(country_labels), all_of(country_cols))

names(df_es) <- gsub("\\W+", "", names(df_es))


###############################################################################
# 9) Exporting files  ########################################################
###############################################################################
run_date <- Sys.Date()

write_sav(df_es, paste0("./data/ES2_NameSurvey_", run_date, ".sav"))
write_dta(df_es, paste0("./data/ES2_NameSurvey_", run_date, ".dta"))
saveRDS(df_es, paste0("./data/ES2_NameSurvey_", run_date, ".RDS"))

