test_that("create output from adam data", {
  
safety_statistics <-   safety_summary_adam(
     pharmaverseadam::adsl |> dplyr::filter(ARM != "Screen Failure"),
     pharmaverseadam::adae)

expect_equivalent(names(safety_statistics),c("GROUP","NON_SERIOUS","SERIOUS"))
obj_type <- sapply(safety_statistics, class)
expect_equivalent( obj_type,  rep("data.frame",3))

expect_equal( 
  safety_statistics$NON_SERIOUS$occurrences |> as.numeric() |> sum() +
    safety_statistics$SERIOUS$occurrences |> as.numeric() |> sum(), 
  nrow(pharmaverseadam::adae)  
)

expect_true( complete.cases(safety_statistics$GROUP) |> all())
expect_true( complete.cases(safety_statistics$NON_SERIOUS) |> all())
expect_true( complete.cases(safety_statistics$SERIOUS) |> all())
})
