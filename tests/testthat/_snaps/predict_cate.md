# predict.cate raises error for invalid treatment and/or response values

    Code
      predict.cate("Not a cate object", new_dummy_tbl)
    Condition
      Error:
      ! use only with "cate" objects

---

    Code
      predict.cate(dummy_cate_object_cf, "Not a tbl")
    Condition
      Error:
      ! newdata_tbl` must be a tibble or data.frame.

---

    Code
      predict.cate(dummy_cate_object_rang, new_dummy_tbl)
    Condition
      Error:
      ! Aggregation method must be 'studyspecific'.

---

    Code
      predict.cate(dummy_cate_object_si, new_dummy_tbl)
    Condition
      Error:
      ! Aggregation method must be 'studyspecific'.

---

    Code
      predict.cate(dummy_cate_object_cf, new_dummy_tbl, alpha = "not a number between 0 and 1")
    Condition
      Error:
      ! `alpha` must be a numeric value between 0 and 1.

---

    Code
      predict.cate(dummy_cate_object_cf, new_dummy_tbl, alpha = 999)
    Condition
      Error:
      ! `alpha` must be a numeric value between 0 and 1.

---

    Code
      predict.cate(dummy_cate_object_cf, new_dummy_tbl, alpha = -0.05)
    Condition
      Error:
      ! `alpha` must be a numeric value between 0 and 1.

---

    Code
      predict.cate(dummy_cate_object_cf, new_dummy_tbl %>% dplyr::select(-tx))
    Condition
      Error:
      ! New data must include same treatment and covariate columns used to fit original model

---

    Code
      predict.cate(dummy_cate_object_cf, new_dummy_tbl %>% dplyr::select(-var1))
    Condition
      Error:
      ! New data must include same treatment and covariate columns used to fit original model

---

    Code
      predict.cate(dummy_cate_object_cf_studies, new_dummy_tbl)
    Condition
      Error:
      ! Insufficient degrees of freedom; original model must include at least 3 studies.

