context("colorspace functions")

test_that("color classes work", {
  c1 <- hex("#1122dd")
  c2 <- rgb(runif(1), runif(1), runif(1))
  c3 <- hsl(runif(1, 0, 360), runif(1), runif(1))
  c4 <- hsv(runif(1, 0, 360), runif(1), runif(1))
  c5 <- as.yuv(rgb(runif(1), runif(1), runif(1)))

  c6 <- hex(c("#112233", "#aabbcc"))
  c7 <- rgb(runif(10), runif(10), runif(10))
  c8 <- hsl(runif(10, 0, 360), runif(10), runif(10))
  c9 <- hsv(runif(10, 0, 360), runif(10), runif(10))
  c0 <- as.yuv(rgb(runif(10), runif(10), runif(10)))


  expect_is(c1, "color")
  expect_is(c2, "color")
  expect_is(c3, "color")
  expect_is(c4, "color")
  expect_is(c5, "color")

  expect_is(c1, "hex")
  expect_is(c2, "rgb")
  expect_is(c3, "hsl")
  expect_is(c4, "hsv")
  expect_is(c5, "yuv")

  expect_is(c6, "color")
  expect_is(c7, "color")
  expect_is(c8, "color")
  expect_is(c9, "color")
  expect_is(c0, "color")

  expect_is(c6, "hex")
  expect_is(c7, "rgb")
  expect_is(c8, "hsl")
  expect_is(c9, "hsv")
  expect_is(c0, "yuv")

})


test_that("color ranges work", {
  expect_error(hex("112233"))
  expect_error(rgb(2, 2, 2))
  expect_error(rgb(-1, 0, 0))
  expect_error(hsl(0, 2, 2))
  expect_error(hsv(0, 2, 2))
  expect_error(yuv(2, 2, 2))
  expect_error(yuv(-1, -1, -1))

  expect_equal(hsl(370, .5, .5), hsl(-350, .5, .5))
  expect_equal(hsv(370, .5, .5), hsv(-350, .5, .5))
})

test_that("color conversions are reflexive", {
  c1 <- hex("#1122dd")
  c2 <- rgb(runif(10), runif(10), runif(10))
  c3 <- hsl(runif(10, 0, 360), runif(10), runif(10))
  c4 <- hsv(runif(10, 0, 360), runif(10), runif(10))
  c5 <- as.yuv(rgb(runif(10), runif(10), runif(10)))

  expect_equal(c1, as.hex(as.hsl(c1)))
  expect_equal(c2, as.rgb(as.hsl(c2)))
  expect_equal(c3, as.hsl(as.yuv(c3)))
  expect_equal(c4, as.hsv(as.hex(c4)))
  expect_equal(c5, as.yuv(as.hsv(c5)))
})


test_that("color math works", {
  col1 <- hex("#1122dd")
  expect_equal(col1 + hex("#112211"), hex("#2244ee"))
  expect_error(col1 + hex("#000033"))
  hsl1 <- as.hsl(col1)

  col2 <- col1 + hsl(100, 0.1, 0.1)
  col3 <- as.yuv(col1) - hsl(0, 0, 0.1)
  col4 <- as.hsv(col1) - rgb(0, 0, 0.1)

  expect_is(col2, "hex")
  expect_is(col3, "yuv")
  expect_is(col4, "hsv")
})

