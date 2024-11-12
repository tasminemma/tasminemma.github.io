---
title: "tidying1"
author: "Tasmin Augustin"
date: "2024-06-24"
output: html_document
---

```{r setup, include=TRUE}
library(tidyverse)
library(unheadr)
library(janitor)
library(dplyr)

data_raw <- read.csv(file = "data/TeenTripleP_BaselineSurvey_Parent_Numeric(in).csv",
                     na.strings=c(""," ","NA"), # empty cells NA
                     strip.white = TRUE) %>%
  mash_colnames(1)%>%  # combine rows into one column name
  clean_names()        # column names in to lowercase and snake case
```

## data types, empties, typing errors, duplicates

```{r empties}

# removing empties

data_raw <- subset(data_raw, !(q17_please_enter_your_participant_id %in% NA)) 

```

```{r renaming variables}

# renaming demographic variables  

data_raw <- rename(data_raw, 
       progress = progress_progress,
       duration_sec = duration_in_seconds_duration_in_seconds,
       finished = finished_finished, 
       participant_id = q17_please_enter_your_participant_id,
       date_of_birth = q18_what_is_your_date_of_birth_please_use_the_following_format_dd_mm_yyyy,
       gender_assigned = q22_what_was_your_gender_assigned_at_birth,
       gender_identity = q23_what_gender_do_you_identify_as_selected_choice,
       gender_identity_text = q23_3_text_what_gender_do_you_identify_as_other_please_state_text,
       english_first_lang = q24_is_english_your_first_language,
       ethnicity = q25_what_is_your_ethnicity, # following variables are about child
       child_conditions_expected_12_months = q28_does_your_child_young_person_have_any_physical_or_mental_health_conditions_or_illnesses_lasting_or_expected_to_last_12_months_or_more,
       child_conditions_reduce_ability = q29_if_yes_does_your_child_young_person_s_condition_or_illness_do_any_of_their_conditions_or_illnesses_reduce_their_ability_to_carry_out_day_to_day_activities,
       child_sen = q31_does_your_child_young_person_have_special_educational_needs,
       child_sen_select = q30_if_yes_what_are_the_reason_for_his_her_special_education_needs_additional_support_needs_select_all_that_apply_selected_choice,
       child_sen_select_other = q30_17_text_if_yes_what_are_the_reason_for_his_her_special_education_needs_additional_support_needs_select_all_that_apply_other_reason_please_specify_text,
       child_dr_told_autism = q32_has_a_doctor_or_health_professional_ever_told_you_that_your_child_young_person_had_autism_asperger_s_syndrome_or_autism_spectrum_disorder,
       child_dr_told_intellectual_disability = q33_has_a_doctor_other_health_professional_or_education_professional_ever_told_you_that_your_child_young_person_has_a_learning_intellectual_disability,
       child_out_home_past_12_months = q64_has_your_child_young_person_been_placed_in_out_of_home_care_in_the_previous_12_months)

# variables after those are responses to questionnaires

## renaming SDQ variables

data_raw <- rename(data_raw,
                   pconsid = q1_1_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_1_considerate_of_other_peoples_feelings,
                   prestles = q1_2_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_2_restless_overactive_cannot_stay_still_for_long,
                   psomatic = q1_3_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_3_often_complains_of_headaches_stomach_aches_or_sickness,
                   pshares = q1_4_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_4_shares_readily_with_other_children_treats_toys_pencils_etc,
                   ptantrum = q1_5_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_5_often_has_temper_tantrums_or_hot_tempers,
                   ploner = q1_6_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_6_rather_solitary_tends_to_play_alone,
                   pobeys = q1_7_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_7_generally_obedient_usually_does_what_adults_request,
                   pworries = q1_8_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_8_many_worries_often_seems_worried,
                   pcaring = q1_9_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_9_helpful_if_someone_is_hurt_upset_or_feeling_ill,
                   pfidgety = q1_10_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_10_constantly_fidgeting_or_squirming,
                   pfriend = q1_11_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_11_has_at_least_one_good_friend,
                   pfights = q1_12_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_12_often_fights_with_other_children_or_bullies_them,
                   punhappy = q1_13_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_13_often_unhappy_down_hearted_or_tearful,
                   ppopular = q1_14_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_14_generally_liked_by_other_children,
                   pdistrac = q1_15_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_15_easily_distracted_concentration_wanders,
                   pclingy = q1_16_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_16_nervous_or_clingy_in_new_situations_easily_loses_confidence,
                   pkind = q1_17_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_17_kind_to_younger_children,
                   plies = q1_18_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_18_often_lies_or_cheats,
                   pbullied = q1_19_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_19_picked_on_or_bullied_by_other_children,
                   phelpout = q1_20_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_20_often_volunteers_to_help_others_parents_teachers_other_children,
                   preflect = q1_21_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_21_thinks_things_out_before_acting,
                   psteals = q1_22_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_22_steals_from_home_school_or_elsewhere,
                   poldbest = q1_23_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_23_gets_on_better_with_adults_than_with_other_children,
                   pafraid = q1_24_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_24_many_fears_easily_scared,
                   pattends = q1_25_strengths_and_difficulties_questionnaire_c_robert_goodman_2005_for_each_item_please_mark_the_box_for_not_true_somewhat_true_or_certainly_true_it_would_help_us_if_you_answered_all_items_as_best_you_can_even_if_you_are_not_absolutely_certain_or_the_item_seems_daft_please_give_your_answers_on_the_basis_of_your_childs_behaviour_over_the_last_six_months_25_sees_tasks_through_to_the_end_good_attention_span)

```

```{r data types}

# converting data types to the appropriate type 

data_raw <- type.convert(data_raw, 
                         trim_ws = TRUE,
                         as.is = TRUE) # do not turn char into Factor 

# birth dates as dates

data_raw$date_of_birth <- as.Date(data_raw$date_of_birth, format = "%d/%m/%Y")

```

```{r binary variables}

# changing binary values from 1 and 2 to 0 and 1
# they are also yes/no questions that coded yes as 1 and no as 2,
# so these 0s and 1s have to be flipped (so more 1 and 2 to 1 and 0)

data_raw <- data_raw %>%
   mutate(across(c(english_first_lang,
                   child_conditions_expected_12_months,
                   child_conditions_reduce_ability,
                   child_sen,
                   child_dr_told_autism,
                   child_dr_told_intellectual_disability,
                   child_out_home_past_12_months), ~ 2 - .))

```


```{r typing errors}

# correcting typing errors & standardizing format

data_raw[data_raw$participant_id=="cb1001", "participant_id"] <- "CB10-01" 

data_raw[data_raw$participant_id=="CB1401", "participant_id"] <- "CB14-01"

data_raw[data_raw$participant_id=="MNO4-01", "participant_id"] <- "MN04-01"

```


```{r duplicates}

# identifying duplicates
## data_raw$q17_please_enter_your_participant_id[duplicated(data_raw$q17_please_enter_your_participant_id)]
## Identified duplicates "BH04-02" "BH06-01" "BH06-01" "MN04-01"

# isolating duplicates
## data_raw[data_raw$q17_please_enter_your_participant_id=="relevant_id",]

# deletion based on ID and condition so it will be independent of order in the table
## luckily each duplicate also had 1 completed entry, therefore the condition is its completion

data_raw <- subset(data_raw, !(participant_id=="BH06-01" & finished==FALSE))

data_raw <- subset(data_raw, !(participant_id=="MN04-01" & finished==FALSE))

data_raw <- subset(data_raw, !(participant_id=="BH04-02" & finished==FALSE))

## ethnicity was different between "BH04-02" responses. Looking at the text data, the option 10 may have used to  
## read "10 - Asian or Asian British - Any other Black background" so it is reasonable to assume given the other 
## answer that the respondent meant "5 - Black or Black British - African" as in their incomplete response.
## therefore, replace that one answer with the answer provided in the deleted entry.

data_raw[data_raw$participant_id=="BH04-02", "ethnicity"] <- 5 

```

## recoding SEN multi-select into binary variables 

```{r separating sen selections into individual observations}

# separating SEN selections into individual observations
# longer table

data_sen <- data_raw%>%
  separate_rows(child_sen_select,
                sep = ",")
```

Using the `pivot_wider()` function to turn the selections into variables.

`values_fn` is a function applied to the value in each cell in the output. Mine is `= 1` to replace all non-NA values with 1. `values_fill` is used here to fill missing data_raw with `0`. 

Renamed using the Parent Baseline CRF

```{r turning SEN observations into variables/column names}

# turning SEN observations into variables/column names

data_sen <- data_sen %>%
  pivot_wider(names_from = child_sen_select,
              names_prefix = "sen",
              values_from = child_sen_select, 
              values_fn = ~1, 
              values_fill = 0) %>%
  rename( # options that were not selected (6, 7, 8, 9, 13, 14, 16) are commented out
    child_sen_dyslexia = sen1,
    child_sen_learning_difficulties = sen2,
    child_sen_adhd = sen3,
    child_sen_autism = sen4,
    child_sen_behavioural_hyperactivity = sen5,
    # child_sen_speech_language = sen6,
    # child_sen_sight = sen7,
    # child_sen_hearing= sen8 ,
    # child_sen_physical_disability = sen9,
    child_sen_medical_health = sen10,
    child_sen_mental_illness_depression = sen11,
    child_sen_gifted = sen12,
    # child_sen_english_additional_language = sen13,
    # child_sen_young_carer_disabled_sibling = sen14,
    child_sen_bullying = sen15,
    # child_sen_bereavement = sen16,
    child_sen_other = sen17,
  )
```

## SDQ scoring 

```{r 123 to 012}

# recoding 123 to 012 for scoring 

data_sdq <- data_sen %>%
   mutate(across(c(pconsid:pattends), ~ .x -1)) 
   # "." and ".x" above seems to refer to what I need it to, but not sure why

```



