# plot.cate generates expected errors/warnings

    Code
      plot.cate("Not a cate object")
    Condition
      Error:
      ! use only with "cate" objects

---

    Code
      plot.cate(dummy_cate_object_sl, which_plot = 10)
    Condition
      Error:
      ! 'which_plot' must be a numeric value between 1 and 5

---

    Code
      plot.cate(dummy_cate_object_sl, which_plot = "3")
    Condition
      Error:
      ! 'which_plot' must be a numeric value between 1 and 5

