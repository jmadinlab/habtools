test_that("fd works", {
  expect_length(
    fd(horseshoe, method = "area", lvec = c(0.125, 0.25, 0.5, 1, 2)),
    1)
})
