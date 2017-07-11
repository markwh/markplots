
#' hexadecimal color
#'
#' @param hex a character vector, must be preceded by a #.
#' @export
hex <- function(hex) {
  hex <- toupper(hex)
  if (!all(grepl("^#", hex)))
    stop("Hex strings must be preceded by a #.")
  # if (!grepl("^#[[A-F]]))
  out <- structure(.Data = hex,
            rgb = hex2rgb(hex),
            class = c("hex", "color"))
  out
}

print.hex <- function(hex) {
  print(as.character(hex))
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
  out <- structure(.Data = hsl,
                   rgb = hsl2rgb(hsl),
                   class = c("hsl", "color"))
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
  out <- structure(.Data = hsv,
                   rgb = hsv2rgb(hsv),
                   class = c("hsv", "color"))
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
  out <- structure(.Data = rgb,
                   rgb = rgb,
                   class = c("rgb", "color"))
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
  out <- structure(.Data = yuv,
            rgb = yuv2rgb(yuv),
            class = c("yuv", "color"))
  out
}

# print.yuv <- function(yuv) {
#   print(as.matrix(yuv))
# }

make_color <- function(data, space = c("rgb", "yuv", "hsl", "hsv", "hex"),
                       rgb = NULL) {
  if (is.null(rgb)) {
    rgbfun <- get(paste0(space, "2rgb"))
    rgb <- rgbfun(data)
  }
  out <- structure(.Data = data,
                   rgb = rgb,
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

`+.color` <- function(x, y) {
  out <- plus(x, y)
  out
}

plus <- function(x, y) {
  # convert x to class of y
  convfun <- get(paste0("as.", class(y)[1]))#, envir = "package:markplots")
  classy <- ifelse(class(y)[1] == "hex", "rgb", class(y)[1])
  rgbfun <- get(paste0(classy, "2rgb"))
  backfun <- get(paste0("as.", class(x)[1]))
  # perform matrix addition
  res <- as.matrix(convfun(x)) + as.matrix(y)
  resrgb <- rgbfun(res)

  # Convert result to class of x
  out1 <- structure(.Data = res,
                    rgb = resrgb,
                    class = class(y))
  out <- backfun(out1)
  out
}

# hex("#aaee11") + yuv

print.color <- function(color) {
  print(as.matrix(color))
}

as.matrix.hex <- function(hex) {
  as.matrix(as.rgb(hex))
}

as.matrix.color <- function(color) {
  attr(color, c("class")) <- NULL
  attr(color, c("rgb")) <- NULL
  as.matrix.default(color)
}
