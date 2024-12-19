# assert_column_class works with both sing and multiple classes

    Code
      assert_column_class(dummy_tbl, c("studyid", "tx"), c("character", "integer"))
    Condition
      Error:
      ! Column class validation only works on one column at a time.

---

    Code
      assert_column_class(dummy_tbl, "studyid", "numeric")
    Condition
      Error:
      ! studyid does not possess one of the following classes: numeric

