
# hsl2rgb <- function(hsl) {
#   hsl[, 1] <- hsl[, 1] / 360
#   out <- (colorscience::HSL2RGB((hsl)))
#   out
# }

# hsv2rgb <- function(hsv) {
#   hsv <- matrix(hsv, ncol = 3)
#   hsv[, 1] <- (hsv[, 1] %% 360) / 360
#   out <- colorscience::HSV2RGB(hsv)
#   out <- out / 255
#   if (any(out < 0) || any(out > 1))
#     stop("Invalid HSV range.")
#
#   out
# }

hex2rgb <- function(hex) {
  rgb <- colorspace::hex2RGB(hex)
  out <- matrix(rgb@coords, ncol = 3)

  if (any(out < 0) || any(out > 1))
    stop("Invalid hex range.")

  out
}

yuv2rgb <- function(yuv) {
  convmat <- matrix(c(0.299, 0.587, 0.114,
                      -0.14713, -0.28886, 0.436,
                      0.615, -0.51499, -0.10001),
                    nrow = 3, byrow = TRUE)
  out <- yuv %*% t(solve(convmat))

  if (any(out < 0) || any(out > 1))
    stop("Invalid yuv range.")

  out
}

rgb2hsv <- function(rgb) {

  hsv <- colorscience::RGB2HSV(rgb * 255)
  hsv[, 1] <- hsv[, 1] * 360

  if (any(rgb < 0) || any(rgb > 1))
    stop("Invalid rgb range. All values must be in [0, 1].")

  out <- structure(.Data = hsv,
                   rgb = rgb,
                   class = c("hsv", "color"))
}

rgb2hsl <- function(rgb) {

  if (any(rgb < 0) || any(rgb > 1))
    stop("Invalid rgb range. All values must be in [0, 1].")

  hsl <- colorscience::RGB2HSL(rgb * 255)
  hsl[, 1] <- hsl[, 1] * 360

  out <- structure(.Data = hsl,
                   rgb = rgb,
                   class = c("hsl", "color"))
}

rgb2rgb <- function(rgb) {

  stopifnot(!is(rgb, "rgb"))
  stopifnot(is(rgb, "matrix"))

  # out <- attr(rgb, "rgb")
  out <- rgb
  out
}

inhex <- function(x) {
  stopifnot(all(x >= 0 & x <= 1))
  out <- sprintf("%02X", round(x * 255))
  out
}

rgb2hex <- function(rgb) {

  if (any(rgb < 0) || any(rgb > 1))
    stop("Invalid rgb range. All values must be in [0, 1].")

  r <- rgb[, 1]
  g <- rgb[, 2]
  b <- rgb[, 3]

  hex <- sprintf("#%s%s%s", inhex(r), inhex(g), inhex(b))
  out <- structure(.Data = hex,
                   rgb = rgb,
                   class = c("hex", "color"))
}

rgb2yuv <- function(rgb) {

  if (any(rgb < 0) || any(rgb > 1))
    stop("Invalid rgb range. All values must be in [0, 1].")


  convmat <- matrix(c(0.299, 0.587, 0.114,
                      -0.14713, -0.28886, 0.436,
                      0.615, -0.51499, -0.10001),
                    nrow = 3, byrow = TRUE)
  yuv <- rgb %*% t(convmat)
  colnames(yuv) <- c("Y", "U", "V")
  out <- structure(.Data = yuv,
                   rgb = rgb,
                   class = c("yuv", "color"))
}


