# estimate_cate raises error for invalid treatment and/or response values

    Code
      estimate_cate(trial_tbl = dummy_tbl %>% dplyr::mutate(tx = paste0("Treatment ",
        tx)), estimation_method = "causalforest", aggregation_method = "studyindicator",
      study_col = "studyid", treatment_col = "tx", outcome_col = "response")
    Condition
      Error:
      ! tx does not possess one of the following classes: numeric | integer

---

    Code
      estimate_cate(trial_tbl = dummy_tbl %>% dplyr::mutate(tx = tx + 1),
      estimation_method = "causalforest", aggregation_method = "studyindicator",
      study_col = "studyid", treatment_col = "tx", outcome_col = "response")
    Condition
      Error:
      ! Treatment values must be 0, 1, or NA.

---

    Code
      estimate_cate(trial_tbl = dummy_tbl %>% dplyr::mutate(response = as.character(
        response)), estimation_method = "causalforest", aggregation_method = "studyindicator",
      study_col = "studyid", treatment_col = "tx", outcome_col = "response")
    Condition
      Error:
      ! response does not possess one of the following classes: numeric

# estimate_cate raises error for missing columns

    Code
      estimate_cate(trial_tbl = dummy_tbl %>% dplyr::select(-studyid),
      estimation_method = "causalforest", aggregation_method = "studyindicator",
      study_col = "studyid", treatment_col = "tx", outcome_col = "response")
    Condition
      Error:
      ! At least one of the following columns does not exist in the supplied table object: 
      
      studyid | tx | response

---

    Code
      estimate_cate(trial_tbl = dummy_tbl %>% dplyr::select(-tx), estimation_method = "causalforest",
      aggregation_method = "studyindicator", study_col = "studyid", treatment_col = "tx",
      outcome_col = "response")
    Condition
      Error:
      ! At least one of the following columns does not exist in the supplied table object: 
      
      studyid | tx | response

---

    Code
      estimate_cate(trial_tbl = dummy_tbl %>% dplyr::select(-response),
      estimation_method = "causalforest", aggregation_method = "studyindicator",
      study_col = "studyid", treatment_col = "tx", outcome_col = "response")
    Condition
      Error:
      ! At least one of the following columns does not exist in the supplied table object: 
      
      studyid | tx | response

---

    Code
      estimate_cate(trial_tbl = dummy_tbl, estimation_method = "causalforest",
        aggregation_method = "studyindicator", study_col = "studyid", treatment_col = "tx",
        outcome_col = "response", covariate_col = "foobar")
    Condition
      Error:
      ! At least one of the following columns does not exist in the supplied table object: 
      
      foobar

---

    Code
      estimate_cate(trial_tbl = dummy_tbl, estimation_method = "causalforest",
        aggregation_method = "studyindicator", study_col = "studyid", treatment_col = "tx",
        outcome_col = "response", covariate_col = "foobar")
    Condition
      Error:
      ! At least one of the following columns does not exist in the supplied table object: 
      
      foobar

