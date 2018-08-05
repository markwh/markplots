
`+.color` <- function(x, y) {
  out <- plus(x, y)
  out
}

`-.color` <- function(x, y) {
  out <- minus(x, y)
  out
}

plus <- function(x, y) {
  # convert x to class of y
  # classy <- ifelse(class(y)[1] == "hex", "rgb", class(y)[1])
  classy <- class(y)[1]
  convfun <- get(paste0("as.", classy))#, envir = "package:markplots")
  backfun <- get(paste0("as.", class(x)[1]))
  # perform matrix addition
  xmat <- as.matrix(convfun(x))
  ymat <- matrix(rep(as.matrix(y), length.out = nrow(xmat) * ncol(xmat)),
                 nrow = nrow(xmat), byrow = TRUE)
  res <- xmat + ymat
  rgbmat <- NULL
  if (classy == "hex") {
    rgbmat <- res
    res <- as.character(rgb2hex(res))
  }
  # Convert result to class of x
  # browser()
  out1 <- make_color(data = res, space = classy, rgb = rgbmat)

  out <- backfun(out1)
  out
}

minus <- function(x, y) {
  # convert x to class of y
  # classy <- ifelse(class(y)[1] == "hex", "rgb", class(y)[1])
  classy <- class(y)[1]
  convfun <- get(paste0("as.", classy))#, envir = "package:markplots")
  backfun <- get(paste0("as.", class(x)[1]))
  # perform matrix addition
  xmat <- as.matrix(convfun(x))
  ymat <- matrix(rep(as.matrix(y), length.out = nrow(xmat) * ncol(xmat)),
                 nrow = nrow(xmat), byrow = TRUE)
  res <- xmat - ymat
  if (classy == "hex") {
    rgbmat <- res
    res <- as.character(rgb2hex(res))
  }
  # Convert result to class of x
  out1 <- make_color(data = res, space = classy)
  out <- backfun(out1)
  out
}

times <- function(x, y) {
  if (!(is.numeric(y) && is.vector(y)))
    stop("y must be a numeric vector")

  classx <- class(x)[1]
  # backfun <- get(paste0("as.", classx))
  xmat <- as.matrix(x)
  yvec <- rep(y, length.out = nrow(xmat))
  ymat <- matrix(rep(yvec, length.out = nrow(xmat) * ncol(xmat)),
                 nrow = nrow(xmat), byrow = FALSE)
  res <- xmat * ymat

  if (classx == "hex") {
    rgbmat <- res
    res <- as.character(rgb2hex(res))
  }
  out <- make_color(data = res, space = classx)
  out
}

toCartesian <- function(color) {
  stopifnot(is(color, "hsl") || is(color, "hsv"))
  coldf <- as.data.frame(color)
  x <- with(coldf, S * cos(H * pi / 180))
  y <- with(coldf, S * sin(H * pi / 180))
  z <- coldf[, 3]
  out <- cbind(x = x, y = y, z = z)
  out
}
