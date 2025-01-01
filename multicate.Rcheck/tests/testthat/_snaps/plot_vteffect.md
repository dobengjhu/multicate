# plot_vteffect generates expected errors/warnings

    Code
      plot_vteffect("Not a cate object", "var1")
    Condition
      Error:
      ! use only with "cate" objects

---

    Code
      plot_vteffect(dummy_cate_object_cf, 1234)
    Condition
      Error:
      ! `covariate_name` must be a string.

---

    Code
      plot_vteffect(dummy_cate_object_cf, "foobar")
    Condition
      Error:
      ! At least one of the following columns does not exist in the supplied table object: 
      
      foobar

