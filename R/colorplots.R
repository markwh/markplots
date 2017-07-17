#' Plot in the UV plane of YUV space
#'
#' @import ggplot2
#' @export
plotuv = function(hex) {
  if (!is(hex, "hex"))
    hex <- hex(hex)
  yuv <- as.data.frame(as.yuv(hex))
  yuv$hex <- as.character(hex)
  out <- ggplot(yuv, aes(x = U, y = V, color = hex, size = Y)) +
    geom_point() +
    scale_color_identity() +
    scale_size_continuous(limits = c(0, 1)) +
    coord_fixed(xlim = c(-0.6, 0.6), ylim = c(-0.6, 0.6)) +
    theme_minimal()
  out
}

#' Plot in the HS plane of HSV space
#'
#' @import ggplot2
#' @export
plothsv = function(color) {
  if (!is(color, "color")) {
    if (is.character(color))
      color <- hex(color)
    else
      stop("argument must be a color object or a valid hex string")
  }
  hex <- as.hex(color)
  hsv <- as.data.frame(as.hsv(hex))
  hsv$hex <- as.character(hex)
  out <- ggplot(hsv, aes(x = H * 2 * pi / 360, y = S, color = hex, size = V)) +
    geom_point() +
    coord_polar(theta = "x") +
    scale_x_continuous(breaks = 0:3 / 2 * pi, labels = c(0, "pi/2", "pi", "3 pi/2"),
                       limits = c(0, 2 * pi)) +
    scale_y_continuous(breaks = 1:5 / 5, limits = c(0, 1)) +
    scale_size_continuous(limits = c(0, 1)) +
    scale_color_identity()
  out
}



#' Plot in the HS plane of HSL space
#'
#' @import ggplot2
#' @export
plothsl = function(color) {
  if (!is(color, "color")) {
    if (is.character(color))
      color <- hex(color)
    else
      stop("argument must be a color object or a valid hex string")
  }
  hex <- as.hex(color)
  hsl <- as.data.frame(as.hsl(hex))
  hsl$hex <- as.character(hex)
  out <- ggplot(hsl, aes(x = H * 2 * pi / 360, y = S, color = hex, size = L)) +
    geom_point() +
    coord_polar(theta = "x") +
    scale_x_continuous(breaks = 0:3 / 2 * pi, labels = c(0, "pi/2", "pi", "3 pi/2"),
                       limits = c(0, 2 * pi)) +
    scale_y_continuous(breaks = 1:5 / 5, limits = c(0, 1)) +
    scale_size_continuous(limits = c(0, 1)) +
    scale_color_identity()
  out
}
