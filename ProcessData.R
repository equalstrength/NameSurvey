###############################################################################
# EqualStrength Name Survey - Data Preparation
###############################################################################

# Required packages
library(tidyverse) # for data manipulation
library(haven) # to import/export from/to SPSS/STATA formats
library(openxlsx) # to import/export from/to MS Excel file

# 1) Importing dictionary #####################################################
###############################################################################
# This file contains the expected information (e.g. country of origin, sex..)
# for each of the names tested in the survey
tb_dict <- openxlsx::read.xlsx("./data/tb_dictionary_full.xlsx")

# Removing empty spaces before or after the name
tb_dict$name  <- str_trim(tb_dict$name)

# Adjusting name of country variable column
tb_dict$country_code  <- str_remove(tb_dict$country_code, "Name1_")

# Correcting country code for Bangladesh
tb_dict$country_code <- if_else(tb_dict$country_name == "Bangladesh", "V001c_48", tb_dict$country_code)

# Correcting spelling in names that contain the number 5 instead of "ssa"
tb_dict <- tb_dict |> mutate(name = str_replace(name, "5", "ssa"))


# 2) Importing datafiles ######################################################
###############################################################################
# Generating a list of files in 'long format (ending with 'long.sav' or 'Datalong') 
ls_round_1 <- list.files("./data/raw", recursive = TRUE, pattern = "\\wide.sav$|Datawide*.|SWI", full.names=TRUE)

# Importing  all files from the list and binding all rows into one dataset
df_round_1 <- map_dfr(ls_round_1, read_sav) |> mutate(round = 1)

# Generating a list of files sent in the second Round
ls_round_2 <- list.files("./data/round2", recursive = TRUE, full.names=TRUE)

# Removing path to Spanish file as it is already in the long format
spain_file <- "./data/round2/raw_round2/ES_DATA_Long.sav"
ls_round_2 <- setdiff(ls_round_2, spain_file)

# Generating a list of files sent in the second Round
df_round_2 <- map_dfr(ls_round_2, read_sav) |> mutate(round = 2)

# Final raw binded dataset
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
            "Hatice Sahin" ~ "Hatice Şahin",
            "Mehmet Sahin" ~ "Mehmet Şahin",
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
            "Francois Kapinga" ~ "François Kapinga",
            .default = Name
        ))


# Adding observations from Spain that are already in long format =)
df_csic <- read_sav(spain_file)

# Adjusting names/variable formats
names(df_csic) <- str_remove(names(df_csic), "Name1_")
df_csic <- df_csic |> rename(Name = Name1)
df_csic <- df_csic |> select(input_1:V001c_52)
df_csic$VS5b_1 <- as.character(df_csic$VS5b_1)
df_csic$VS5b_2 <- as.character(df_csic$VS5b_2)

#Temporarily adding weight variable for Spain as 1
df_csic$Weging <- 1
print("Spain has weight variable as 1")

# binding datasets
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
    filter(country_code != ".") |>
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
# New scale goes from 0 (low) to 1 (high)
df_es$ladder_adj <- if_else(df_es$V001h %in% c(8,99), NA, (8 - df_es$V001h)/7)

# Adjusting variable skin colour 
# number 10 was mistakenly assigned to DK in round 1
# New scale goes from 0.1 to 1
df_es  <- df_es |> mutate(skin_adj = case_when(
    round == 2 & V001db == 11 ~ NA,
    round == 1 ~ NA,
    .default = V001db / 10
))

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

###############################################################################
# 7) Exporting files  ########################################################
###############################################################################
run_date <- Sys.Date()

write_sav(df_es, paste0("./data/ES2_NameSurvey_", run_date, ".sav"))
write_dta(df_es, paste0("./data/ES2_NameSurvey_", run_date, ".dta"))
saveRDS(df_es, paste0("./data/ES2_NameSurvey_", run_date, ".RDS"))
