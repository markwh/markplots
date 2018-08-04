
#' hexadecimal color
#'
#' @param hex a character vector, must be preceded by a #.
#' @export
hex <- function(hex) {
  hex <- toupper(hex)
  if (!all(grepl("^#", hex)))
    stop("Hex strings must be preceded by a #.")
  if (!all(grepl("^#([A-F0-9]){6}$", hex)))
    stop("Invalid hex code")
  rgb <- hex2rgb(hex)
  out <- make_color(data = hex, space = "hex", rgb = rgb)
  out
}

print.hex <- function(hex) {
  print(as.character(hex))
}

as.character.hex <- function(hex) {
  attr(hex, "hex")
}

#' Hue Saturation Lightness color
#'
#' @param h hue (in degrees), on [0, 360) (uses modulo if over)
#' @param s saturation, on [0, 1]
#' @param l lightness, on [0, 1]
#' @export
hsl <- function(h, s, l) {
  h <- h %% 360
  stopifnot(all(s >=0 & s <= 1))
  stopifnot(all(l >=0 & l <= 1))
  hsl <- cbind(H = h, S = s, L = l)
  out <- make_color(hsl, space = "hsl", rgb = hsl2rgb(hsl))
  out
}

# print.hsl <- function(hsl) {
#   hslmat <- as.matrix(hsl)
#   mostattributes(hslmat) <- NULL
#   print(hslmat)
# }


#' Hue Saturation Value color
#'
#' @param h hue (in degrees), on [0, 360) (uses modulo if over)
#' @param s saturation, on [0, 1]
#' @param v value, on [0, 1]
#' @export
hsv <- function(h, s, v) {
  h <- h %% 360
  stopifnot(all(s >=0 & s <= 1))
  stopifnot(all(v >=0 & v <= 1))
  hsv <- cbind(H = h, S = s, V = v)
  out <- make_color(hsv, space = "hsv", rgb = hsv2rgb(hsv))
  out
}

# print.hsv <- function(hsv) {
#   print(as.matrix(hsv))
# }


#' Red Green Blue
#'
#' @param r red intensity, on [0, 1]
#' @param g green intensity, on [0, 1]
#' @param b blue intensity, on [0, 1]
#' @export
rgb <- function(r, g, b) {
  stopifnot(all(r >= 0 & r <= 1))
  stopifnot(all(g >= 0 & g <= 1))
  stopifnot(all(b >= 0 & b <= 1))

  rgb <- cbind(R = r, G = g, B = b)
  out <- make_color(rgb, space = "rgb", rgb = rgb)
  out
}

# print.rgb <- function(rgb) {
#   print(as.matrix(rgb))
# }



#' YUV color
#'
#' @param y Y parameter of YUV colorspace
#' @param u U parameter of YUV colorspace
#' @param v V parameter of YUV colorspace
#' @export
yuv <- function(y, u, v) {
  yuv <- cbind(Y = y, U = u, V = v)
  out <- make_color(yuv, space = "yuv", rgb = yuv2rgb(yuv))
  out
}

# print.yuv <- function(yuv) {
#   print(as.matrix(yuv))
# }

make_color <- function(data, space = c("rgb", "yuv", "hsl", "hsv", "hex"),
                       rgb = NULL) {
  space = match.arg(space)
  # if (space == "hex")
    # browser()
  if (is.null(rgb)) {
    rgbfun <- get(paste0(space, "2rgb"))
    rgb <- rgbfun(data)
  }

  hex <- NULL
  if (space == "hex") {
    hex <- data
    data <- rgb
    space <- c("hex", "rgb")
  }

  out <- structure(.Data = data,
                   rgb = rgb,
                   hex = hex,
                   class = c(space, "color"))
}


# Conversion functions ----------------------------------------------------

convcolor <- function(color, convto = c("rgb", "yuv", "hsl", "hsv", "hex")) {
  convto <- match.arg(convto)
  stopifnot(is(color, "color"))

  rgb <- attr(color, "rgb")
  convfun <- get(paste0("rgb2", convto))
  outdat <- convfun(rgb)
  out <- make_color(data = outdat, space = convto, rgb = rgb)
  out
}

as.hsl <- function(color) {
  convcolor(color, "hsl")
}

as.hex <- function(color) {
  convcolor(color, "hex")
}

as.rgb <- function(color) {
  convcolor(color, "rgb")
}

as.hsv <- function(color) {
  convcolor(color, "hsv")
}

as.yuv <- function(color) {
  convcolor(color, "yuv")
}

print.color <- function(color) {
  print(as.matrix(color))
}

as.matrix.color <- function(color) {
  attr(color, "class") <- NULL
  attr(color, "rgb") <- NULL
  attr(color, "hex") <- NULL
  as.matrix.default(color)
}

as.data.frame.color <- function(color) {
  as.data.frame(as.matrix(color))
}


# plot --------------------------------------------------------------------

color_barplot <- function(color) {
  colordf <- data.frame(hex = attr(color, "hex"))
  ggplot(colordf) +
    geom_bar(aes(x = 1:nrow(colordf),
                 y = 1,
                 fill = hex), stat = "identity") +
    scale_fill_identity() + theme_minimal() +
    xlab("") + ylab("")
}


