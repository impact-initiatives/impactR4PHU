# # Dummy df creation
# dummy_df <- tidyr::expand_grid(
# worry = c("Never (0 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)"),
# plans = c("Rarely (1-2 times)", "Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)", "Never (0 times)"),
# hands = c("Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)", "Never (0 times)", "Rarely (1-2 times)"),
# drink = c("Often (11-20 times)", "Always (>20 times)", "Never (0 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)"),
# interrupt = c("Never (0 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)"),
# clothes = c("Rarely (1-2 times)", "Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)", "Never (0 times)"),
# food = c("Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)", "Never (0 times)", "Rarely (1-2 times)"),
# body = c("Often (11-20 times)", "Always (>20 times)", "Never (0 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)"),
# angry = c("Never (0 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)"),
# sleep = c("Rarely (1-2 times)", "Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)", "Never (0 times)"),
# none = c("Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)", "Never (0 times)", "Rarely (1-2 times)"),
# shame = c("Often (11-20 times)", "Always (>20 times)", "Never (0 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)")

# )


# Test 1: Input Validation Tests
test_that("add_hwise validates input data", {
  # Test NULL data
  expect_error(
    add_hwise(data = NULL),
    "Please provide a data frame to argument 'data'."
  )
  
  # Test non-data.frame input
  expect_error(
    add_hwise(data = list(a = 1, b = 2)),
    "The 'data' argument must be a data frame."
  )
})



# Test 2: Missing Column Parameters
test_that("add_hwise requires all core HWISE-4 column parameters", {
  df <- data.frame(
    worry = "Never (0 times)",
    plans = "Never (0 times)",
    hands = "Never (0 times)",
    drink = "Never (0 times)"
  )
  
  # Missing hwise_worry_col
  expect_error(
    add_hwise(
      data = df,
      hwise_plans_col = "plans",
      hwise_hands_col = "hands",
      hwise_drink_col = "drink"
    ),
    "Please provide column names for the four core HWISE items"
  )
  
  # Missing hwise_plans_col
  expect_error(
    add_hwise(
      data = df,
      hwise_worry_col = "worry",
      hwise_hands_col = "hands",
      hwise_drink_col = "drink"
    ),
    "Please provide column names for the four core HWISE items"
  )
})

# Test 3: Non-existent Columns
test_that("add_hwise detects non-existent columns in data", {
  df <- data.frame(
    worry = "Never (0 times)",
    plans = "Never (0 times)",
    hands = "Never (0 times)"
  )
  
  expect_error(
    add_hwise(
      data = df,
      hwise_worry_col = "worry",
      hwise_plans_col = "plans",
      hwise_hands_col = "hands",
      hwise_drink_col = "nonexistent_col",
      never_val = "Never (0 times)",
      rarely_val = "Rarely (1-2 times)",
      sometimes_val = "Sometimes (3-10 times)",
      often_val = "Often (11-20 times)",
      always_val = "Always (>20 times)"
    ),
    "One or more specified core HWISE-4 columns are not found in the data frame."
  )
})

# Test 4: Missing Response Values. Warning for missing columns to calculate HWISE-12 supressed to avoid stoppoing the test. The test for the warning message is done in Test #9
test_that("add_hwise requires all response value parameters", {
  df <- data.frame(
    worry = "Never (0 times)",
    plans = "Never (0 times)",
    hands = "Never (0 times)",
    drink = "Never (0 times)"
  )
  
  suppressWarnings(
    expect_error(  
      add_hwise(
        data = df,
        hwise_worry_col = "worry",
        hwise_plans_col = "plans",
        hwise_hands_col = "hands",
        hwise_drink_col = "drink",
        never_val = "Never (0 times)",
        rarely_val = "Rarely (1-2 times)",
        sometimes_val = "Sometimes (3-10 times)"
        # Missing often_val and always_val
      ),
      "Please provide values for all response categories"
    )
  )
})

# Test 5: Invalid Response Values in Data. Warning for missing columns to calculate HWISE-12 supressed to avoid stoppoing the test. The test for the warning message is done in Test #9
test_that("add_hwise warns about invalid response values", {
  df <- data.frame(
    worry = c("Never (0 times)", "Invalid Response"),
    plans = c("Rarely (1-2 times)", "Never (0 times)"),
    hands = c("Sometimes (3-10 times)", "Never (0 times)"),
    drink = c("Often (11-20 times)", "Never (0 times)")
  )
  
  suppressWarnings(
    expect_warning(
      add_hwise(
        data = df,
        hwise_worry_col = "worry",
        hwise_plans_col = "plans",
        hwise_hands_col = "hands",
        hwise_drink_col = "drink",
        never_val = "Never (0 times)",
        rarely_val = "Rarely (1-2 times)",
        sometimes_val = "Sometimes (3-10 times)",
        often_val = "Often (11-20 times)",
        always_val = "Always (>20 times)"
      ),
      "Some rows contain values outside the specified response categories"
    )
  )
})

# Test 6: Correct HWISE-4 Score Calculation. Warning for missing columns to calculate HWISE-12 supressed to avoid stoppoing the test. The test for the warning message is done in Test #9
test_that("add_hwise calculates HWISE-4 scores correctly", {
  df <- data.frame(
    id = 1:5,
    worry = c("Never (0 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)", 
              "Often (11-20 times)", "Always (>20 times)"),
    plans = c("Never (0 times)", "Never (0 times)", "Never (0 times)", 
              "Never (0 times)", "Never (0 times)"),
    hands = c("Never (0 times)", "Never (0 times)", "Never (0 times)", 
              "Never (0 times)", "Never (0 times)"),
    drink = c("Never (0 times)", "Never (0 times)", "Never (0 times)", 
              "Never (0 times)", "Never (0 times)")
  )
  
  suppressWarnings(
    result <- add_hwise(
      data = df,
      hwise_worry_col = "worry",
      hwise_plans_col = "plans",
      hwise_hands_col = "hands",
      hwise_drink_col = "drink",
      never_val = "Never (0 times)",
      rarely_val = "Rarely (1-2 times)",
      sometimes_val = "Sometimes (3-10 times)",
      often_val = "Often (11-20 times)",
      always_val = "Always (>20 times)"
    )
  )
  expect_equal(result$hwise_worry_score, c(0, 1, 2, 3, 3))
  expect_equal(result$hwise4_score, c(0, 1, 2, 3, 3))
})

# Test 7: HWISE-4 Severity Categories. Warning for missing columns to calculate HWISE-12 supressed to avoid stoppoing the test. The test for the warning message is done in Test #9
test_that("add_hwise assigns correct HWISE-4 severity categories", {
  df <- data.frame(
    worry = c("Never (0 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)", "Sometimes (3-10 times)", "Often (11-20 times)"),
    plans = c("Never (0 times)", "Rarely (1-2 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)", "Often (11-20 times)"),
    hands = c("Never (0 times)", "Never (0 times)", "Sometimes (3-10 times)", "Sometimes (3-10 times)", "Often (11-20 times)"),
    drink = c("Never (0 times)", "Never (0 times)", "Never (0 times)", "Sometimes (3-10 times)", "Often (11-20 times)")
  )
  suppressWarnings(
    result <- add_hwise(
      data = df,
      hwise_worry_col = "worry",
      hwise_plans_col = "plans",
      hwise_hands_col = "hands",
      hwise_drink_col = "drink",
      never_val = "Never (0 times)",
      rarely_val = "Rarely (1-2 times)",
      sometimes_val = "Sometimes (3-10 times)",
      often_val = "Often (11-20 times)",
      always_val = "Always (>20 times)"
    )
  )
  expect_equal(result$hwise4_severity_cat, 
               c("No-to-marginal", "No-to-marginal", "Low", "Moderate", "Very High"))
  
})

# Test 8: HWISE-4 Insecurity Binary Category. Warning for missing columns to calculate HWISE-12 supressed to avoid stoppoing the test. The test for the warning message is done in Test #9
test_that("add_hwise assigns correct HWISE-4 insecurity binary category", {
  df <- data.frame(
    worry = c("Never (0 times)", "Sometimes (3-10 times)"),
    plans = c("Never (0 times)", "Rarely (1-2 times)"),
    hands = c("Never (0 times)", "Never (0 times)"),
    drink = c("Never (0 times)", "Never (0 times)")
  )

  suppressWarnings(
    result <- add_hwise(
      data = df,
      hwise_worry_col = "worry",
      hwise_plans_col = "plans",
      hwise_hands_col = "hands",
      hwise_drink_col = "drink",
      never_val = "Never (0 times)",
      rarely_val = "Rarely (1-2 times)",
      sometimes_val = "Sometimes (3-10 times)",
      often_val = "Often (11-20 times)",
      always_val = "Always (>20 times)"
    )
  )
  expect_equal(result$hwise4_insecure_cat, c(0, 0))
  expect_equal(result$hwise4_score, c(0, 3))
})

# Test 9: HWISE-12 Warning When Incomplete
test_that("add_hwise warns when HWISE-12 columns are incomplete", {
  df <- data.frame(
    worry = "Never (0 times)",
    plans = "Never (0 times)",
    hands = "Never (0 times)",
    drink = "Never (0 times)",
    interrupt = "Never (0 times)"
    # Missing other HWISE-12 columns
  )
  
  expect_warning(
    add_hwise(
      data = df,
      hwise_worry_col = "worry",
      hwise_plans_col = "plans",
      hwise_hands_col = "hands",
      hwise_drink_col = "drink",
      hwise_interrupt_col = "interrupt",
      never_val = "Never (0 times)",
      rarely_val = "Rarely (1-2 times)",
      sometimes_val = "Sometimes (3-10 times)",
      often_val = "Often (11-20 times)",
      always_val = "Always (>20 times)"
    ),
    "Not all columns are specified to calculate HWISE-12"
  )
})


# Test 10: Complete HWISE-12 Calculation
test_that("add_hwise calculates HWISE-12 scores correctly", {
  df <- data.frame(
    worry = "Never (0 times)",
    plans = "Rarely (1-2 times)",
    hands = "Sometimes (3-10 times)",
    drink = "Often (11-20 times)",
    interrupt = "Always (>20 times)",
    clothes = "Never (0 times)",
    food = "Rarely (1-2 times)",
    body = "Sometimes (3-10 times)",
    angry = "Often (11-20 times)",
    sleep = "Always (>20 times)",
    none = "Never (0 times)",
    shame = "Rarely (1-2 times)"
  )
  
  result <- add_hwise(
    data = df,
    hwise_worry_col = "worry",
    hwise_plans_col = "plans",
    hwise_hands_col = "hands",
    hwise_drink_col = "drink",
    hwise_interrupt_col = "interrupt",
    hwise_clothes_col = "clothes",
    hwise_food_col = "food",
    hwise_body_col = "body",
    hwise_angry_col = "angry",
    hwise_sleep_col = "sleep",
    hwise_none_col = "none",
    hwise_shame_col = "shame",
    never_val = "Never (0 times)",
    rarely_val = "Rarely (1-2 times)",
    sometimes_val = "Sometimes (3-10 times)",
    often_val = "Often (11-20 times)",
    always_val = "Always (>20 times)"
  )
  
  expect_equal(result$hwise12_score, 19)
  expect_equal(result$hwise12_severity_cat, "Moderate")
})

# Test 11: NA Handling. Warning for missing columns to calculate HWISE-12 supressed to avoid stoppoing the test. The test for the warning message is done in Test #9
test_that("add_hwise handles NA values correctly", {
  df <- data.frame(
    worry = c("Never (0 times)", NA),
    plans = c("Never (0 times)", "Rarely (1-2 times)"),
    hands = c("Never (0 times)", "Never (0 times)"),
    drink = c("Never (0 times)", "Never (0 times)")
  )
  
  suppressWarnings(
    result <- add_hwise(
      data = df,
      hwise_worry_col = "worry",
      hwise_plans_col = "plans",
      hwise_hands_col = "hands",
      hwise_drink_col = "drink",
      never_val = "Never (0 times)",
      rarely_val = "Rarely (1-2 times)",
      sometimes_val = "Sometimes (3-10 times)",
      often_val = "Often (11-20 times)",
      always_val = "Always (>20 times)"
    )
  )
  expect_equal(result$hwise4_score[1], 0)
  expect_true(is.na(result$hwise4_score[2]))
  expect_true(is.na(result$hwise4_severity_cat[2]))
})


# Test 12: Output Structure. Warning for missing columns to calculate HWISE-12 supressed to avoid stoppoing the test. The test for the warning message is done in Test #9
test_that("add_hwise returns correct output structure", {
  df <- data.frame(
    id = 1,
    worry = "Never (0 times)",
    plans = "Never (0 times)",
    hands = "Never (0 times)",
    drink = "Never (0 times)"
  )
  suppressWarnings(
    result <- add_hwise(
      data = df,
      hwise_worry_col = "worry",
      hwise_plans_col = "plans",
      hwise_hands_col = "hands",
      hwise_drink_col = "drink",
      never_val = "Never (0 times)",
      rarely_val = "Rarely (1-2 times)",
      sometimes_val = "Sometimes (3-10 times)",
      often_val = "Often (11-20 times)",
      always_val = "Always (>20 times)"
    )
  )
  expect_true(is.data.frame(result))
  expect_true("hwise4_score" %in% colnames(result))
  expect_true("hwise4_severity_cat" %in% colnames(result))
  expect_true("hwise4_insecure_cat" %in% colnames(result))
  expect_true("id" %in% colnames(result))
})


# Test 13: HWISE-12 Severity Categories
test_that("add_hwise assigns correct HWISE-12 severity categories", {
  df <- data.frame(
    worry = c("Never (0 times)", "Rarely (1-2 times)", "Rarely (1-2 times)", "Rarely (1-2 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)", "Sometimes (3-10 times)", "Sometimes (3-10 times)", "Often (11-20 times)", "Often (11-20 times)"),
    plans = c("Never (0 times)", "Rarely (1-2 times)", "Rarely (1-2 times)", "Never (0 times)", "Rarely (1-2 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)", "Sometimes (3-10 times)", "Often (11-20 times)", "Often (11-20 times)"),
    hands = c("Never (0 times)", "Never (0 times)", "Rarely (1-2 times)", "Rarely (1-2 times)", "Rarely (1-2 times)", "Never (0 times)", "Sometimes (3-10 times)", "Sometimes (3-10 times)", "Often (11-20 times)", "Often (11-20 times)"),
    drink = c("Never (0 times)", "Never (0 times)", "Never (0 times)", "Never (0 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)", "Sometimes (3-10 times)", "Sometimes (3-10 times)", "Often (11-20 times)", "Often (11-20 times)"),
    interrupt = c("Never (0 times)", "Never (0 times)", "Never (0 times)", "Rarely (1-2 times)", "Rarely (1-2 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)", "Sometimes (3-10 times)", "Sometimes (3-10 times)", "Often (11-20 times)"),
    clothes = c("Never (0 times)", "Never (0 times)", "Never (0 times)", "Never (0 times)", "Rarely (1-2 times)", "Never (0 times)", "Sometimes (3-10 times)", "Sometimes (3-10 times)", "Sometimes (3-10 times)", "Often (11-20 times)"),
    food = c("Never (0 times)", "Never (0 times)", "Never (0 times)", "Rarely (1-2 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)", "Never (0 times)", "Sometimes (3-10 times)", "Sometimes (3-10 times)", "Often (11-20 times)"),
    body = c("Never (0 times)", "Never (0 times)", "Never (0 times)", "Never (0 times)", "Rarely (1-2 times)", "Rarely (1-2 times)", "Never (0 times)", "Sometimes (3-10 times)", "Sometimes (3-10 times)", "Often (11-20 times)"),
    angry = c("Never (0 times)", "Never (0 times)", "Never (0 times)", "Rarely (1-2 times)", "Rarely (1-2 times)", "Never (0 times)", "Never (0 times)", "Never (0 times)", "Sometimes (3-10 times)", "Often (11-20 times)"),
    sleep = c("Never (0 times)", "Never (0 times)", "Never (0 times)", "Never (0 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)", "Never (0 times)", "Never (0 times)", "Rarely (1-2 times)", "Often (11-20 times)"),
    none = c("Never (0 times)", "Never (0 times)", "Never (0 times)", "Rarely (1-2 times)", "Rarely (1-2 times)", "Rarely (1-2 times)", "Never (0 times)", "Never (0 times)", "Rarely (1-2 times)", "Often (11-20 times)"),
    shame = c("Never (0 times)", "Never (0 times)", "Never (0 times)", "Never (0 times)", "Rarely (1-2 times)", "Never (0 times)", "Never (0 times)", "Never (0 times)", "Never (0 times)", "Often (11-20 times)")
  )
  
  result <- add_hwise(
    data = df,
    hwise_worry_col = "worry",
    hwise_plans_col = "plans",
    hwise_hands_col = "hands",
    hwise_drink_col = "drink",
    hwise_interrupt_col = "interrupt",
    hwise_clothes_col = "clothes",
    hwise_food_col = "food",
    hwise_body_col = "body",
    hwise_angry_col = "angry",
    hwise_sleep_col = "sleep",
    hwise_none_col = "none",
    hwise_shame_col = "shame",
    never_val = "Never (0 times)",
    rarely_val = "Rarely (1-2 times)",
    sometimes_val = "Sometimes (3-10 times)",
    often_val = "Often (11-20 times)",
    always_val = "Always (>20 times)"
  )
  
  expect_equal(result$hwise12_severity_cat, 
               c("No-to-marginal", "No-to-marginal", "Low", "Low", "Moderate", "Moderate", "Moderate", "Moderate", "High", "High")
              )
})
