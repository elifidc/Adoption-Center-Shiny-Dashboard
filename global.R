library(shiny)
library(shinydashboard)
library(tidyverse)     
library(plotly)
library(leaflet)
library(DT)
library(lubridate)
library(scales)
library(RColorBrewer)


df_clean <- read_csv("animal-shelter-intakes-and-outcomes.csv") %>%
  mutate(
    `Intake Condition` = str_squish(str_to_title(`Intake Condition`)),
    `Animal Type` = str_to_title(`Animal Type`),
    `Outcome Type` = str_to_title(`Outcome Type`),
    Intake_Type = str_to_title(`Intake Type`),
    intake_month = floor_date(`Intake Date`, "month"),
    outcome_month = floor_date(`Outcome Date`, "month"),
    is_alive = was_outcome_alive,
    `Outcome Grouped` = case_when(
      `Outcome Type` %in% c("Rescue", "Adoption", "Return To Rescue", "Foster To Adopt") ~ "Adoption/Rescue",
      TRUE ~ `Outcome Type`
    ),
    `Animal Type Grouped` = case_when(
      `Animal Type` %in% c("Cat", "Dog", "Rabbit", "Bird", "Wild", "Reptile") ~ `Animal Type`,
      TRUE ~ "Other"
    ),
    `Reason Cleaned` = str_to_title(case_when(
      str_squish(str_to_upper(`Reason for Intake`)) == "MOVE" ~ "Owner Moving",
      str_squish(str_to_upper(`Reason for Intake`)) == "OWNER PROB" ~ "Owner Problem",
      str_squish(str_to_upper(`Reason for Intake`)) %in% c("ILL", "POOR HELTH") ~ "Medical Issues",
      str_squish(str_to_upper(`Reason for Intake`)) == "TOO MANY" ~ "Too Many Animals",
      str_squish(str_to_upper(`Reason for Intake`)) == "AGG PEOPLE" ~ "Aggressive Toward People",
      str_squish(str_to_upper(`Reason for Intake`)) == "NO TIME" ~ "No Time",
      TRUE ~ str_to_title(str_squish(`Reason for Intake`))
    )),
    grouped_condition = case_when(
      `Intake Condition` %in% c("Ill Mild", "Ill Moderate", "Ill Moderatete") ~ "Illness (Mild/Moderate)",
      `Intake Condition` == "Ill Severe" ~ "Illness (Severe)",
      `Intake Condition` %in% c("Injured Mild", "Injured Moderate", "I/I Report") ~ "Injured (Mild/Moderate)",
      `Intake Condition` == "Injured Severe" ~ "Injured (Severe)",
      `Intake Condition` %in% c("Behavior Mild", "Behavior Moderate", "Behavior Severe") ~ "Behavioral Issues",
      `Intake Condition` == "Under Age/Weight" ~ "Underage/Underweight",
      `Intake Condition` == "Normal" ~ "Normal",
      `Intake Condition` %in% c("Fractious", "Aged", "Welfare Seizures", "Feral") ~ "Other",
      TRUE ~ `Intake Condition`
    )
  )
