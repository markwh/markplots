
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

#' Plot in the HS plane of HSV or HSL space
#'
#' @import ggplot2
#' @export
ploths = function(hex) {
  if (!is(hex, "hex"))
    hex <- hex(hex)
  hsv <- as.data.frame(as.matrix(as.hsv(hex)))
  hsv$hex <- as.character(hex)
  out <- ggplot(hsv, aes(x = h * 2 * pi / 360, y = s, color = hex, size = v)) +
    geom_point() +
    coord_polar(theta = "x") +
    scale_x_continuous(breaks = 0:3 / 2 * pi, labels = c(0, "pi/2", "pi", "3 pi/2"),
                       limits = c(0, 2 * pi)) +
    scale_size_continuous(limits = c(0, 0.005)) +
    scale_color_identity()
  out
}

