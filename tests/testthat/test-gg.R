context("ggplot functions")

test_that("color plot functions make ggplot objects", {
  c1 <- hex("#1122dd")
  c2 <- rgb(runif(10), runif(10), runif(10))
  c3 <- hsl(runif(10, 0, 360), runif(10), runif(10))
  c4 <- hsv(runif(10, 0, 360), runif(10), runif(10))
  c5 <- as.yuv(rgb(runif(10), runif(10), runif(10)))

  expect_is(plothsl(c1), "gg")
  expect_is(plothsl(c2), "gg")
  expect_is(plothsl(c3), "gg")
  expect_is(plothsl(c4), "gg")
  expect_is(plothsl(c5), "gg")

  expect_is(plothsv(c1), "gg")
  expect_is(plothsv(c2), "gg")
  expect_is(plothsv(c3), "gg")
  expect_is(plothsv(c4), "gg")
  expect_is(plothsv(c5), "gg")
})
