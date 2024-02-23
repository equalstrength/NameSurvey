# Required packages
library(tidyverse)
library(haven)
library(plotly)
library(gt)

# Latest Working DataSet
df_es <- readRDS("./data/ES2_NameSurvey_2024-02-23.RDS")

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

