# assert_column_names_exist works correctly when given a single column name

    Code
      assert_column_names_exist(dummy_tbl, "foobar")
    Condition
      Error:
      ! At least one of the following columns does not exist in the supplied table object: 
      
      foobar

# assert_column_names_exist works correctly when given more than one column names

    Code
      assert_column_names_exist(dummy_tbl, "foobar", "response")
    Condition
      Error:
      ! At least one of the following columns does not exist in the supplied table object: 
      
      foobar | response

