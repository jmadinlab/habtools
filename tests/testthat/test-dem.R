test_that("fd works", {
  expect_length(
    fd(horseshoe, method = "area", x = -470, y = 1266, L = 2, lvec = c(0.125, 0.25, 0.5, 1, 2)),
    1)
})
