


rgb2yuv <- function(rgb) {
  if (is.vector(rgb))
    rgb <- matrix(rgb, nrow = 1)
  stopifnot(ncol(rgb) == 3)
  convmat <- matrix(c(0.299, 0.587, 0.114,
                      -0.14713, -0.28886, 0.436,
                      0.615, -0.51499, -0.10001),
                    nrow = 3, byrow = TRUE)
  out <- rgb %*% t(convmat)
  colnames(out) <- c("Y", "U", "V")
  out
}

hex2yuv <- function(hex) {
  rgbmat <- colorspace::hex2RGB(hex)@coords
  out <- rgb2yuv(rgbmat)
  out
}


yuv2rgb <- function(yuv) {
  if (is.vector(yuv))
    rgb <- matrix(yuv, nrow = 1)
  stopifnot(ncol(yuv) == 3)
  convmat <- matrix(c(0.299, 0.587, 0.114,
                      -0.14713, -0.28886, 0.436,
                      0.615, -0.51499, -0.10001),
                    nrow = 3, byrow = TRUE)
  out <- yuv %*% t(solve(convmat))
  colnames(out) <- c("R", "G", "B")
  out
}

yuv2hex <- function(yuv) {
  rgbvals <- yuv2rgb(yuv)
  red <- rgbvals[,1]
  green <- rgbvals[,2]
  blue <- rgbvals[,3]
  out <- rgb(red, green, blue, maxColorValue = 1.0)
  out
}


hex2hsv <- function(hex) {
  rgb <- as.data.frame(hex2RGB(hex)@coords)
  hsv <- with(rgb, rgb2hsv(r = R, g = G, b = B))
  out <- t(hsv)
  out
}


### plotting

plotuv = function(hex) {
  yuv <- as.data.frame(hex2yuv(hex))
  yuv$hex <- hex
  # yuv$Y = 1 + yuv$Y
  out <- ggplot(yuv, aes(x = U, y = V, color = hex, size = Y)) +
    geom_point() +
    scale_color_identity() +
    scale_size_continuous(limits = c(0, 1)) +
    coord_fixed(xlim = c(-0.6, 0.6), ylim = c(-0.6, 0.6)) +
    theme_minimal()
  out
}


ploths = function(hex) {
  hsv <- as.data.frame(hex2hsv(hex))
  hsv$hex <- hex
  # yuv$Y = 1 + yuv$Y
  out <- ggplot(hsv, aes(x = h * 2 * pi, y = s, color = hex, size = v)) +
    geom_point() +
    coord_polar(theta = "x") +
    scale_x_continuous(breaks = 0:3 / 2 * pi, labels = c(0, "pi/2", "pi", "3 pi/2"),
                       limits = c(0, 2 * pi)) +
    scale_size_continuous(limits = c(0, 0.005)) +
    scale_color_identity()
  out
}
