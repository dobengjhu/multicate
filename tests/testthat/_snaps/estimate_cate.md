# estimate_cate raises error for invalid variable values

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
      ! Treatment values must be 0 or 1.

---

    Code
      estimate_cate(trial_tbl = dummy_tbl %>% dplyr::mutate(response = as.character(
        response)), estimation_method = "causalforest", aggregation_method = "studyindicator",
      study_col = "studyid", treatment_col = "tx", outcome_col = "response")
    Condition
      Error:
      ! response does not possess one of the following classes: numeric | integer

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

---

    Code
      estimate_cate(trial_tbl = dummy_tbl_study_na, estimation_method = "causalforest",
        aggregation_method = "studyindicator", study_col = "studyid", treatment_col = "tx",
        outcome_col = "response")
    Condition
      Error:
      ! `study_col` cannot include missing values.

---

    Code
      estimate_cate(trial_tbl = dummy_tbl_treatment_na, estimation_method = "causalforest",
        aggregation_method = "studyindicator", study_col = "studyid", treatment_col = "tx",
        outcome_col = "response")
    Condition
      Error:
      ! `treatment_col` cannot include missing values.

---

    Code
      estimate_cate(trial_tbl = dummy_tbl_outcome_na, estimation_method = "causalforest",
        aggregation_method = "studyindicator", study_col = "studyid", treatment_col = "tx",
        outcome_col = "response")
    Condition
      Error:
      ! `outcome_col` cannot include missing values.

---

    Code
      estimate_cate(trial_tbl = dummy_tbl_covariate_na, estimation_method = "causalforest",
        aggregation_method = "studyindicator", study_col = "studyid", treatment_col = "tx",
        outcome_col = "response")
    Condition
      Error:
      ! Variables included in `covariate_col` cannot include missing values.

