
#' A fixed version--colorscience does not work
#'
#' @param hsl hue, degrees on [0, 360); saturation, on [0, 1]; lightness, on [0, 1]
#'
#' @return Matrix of RGB values on [0, 1]

hsl2rgb <- function(hsl) {
  hsl <- matrix(hsl, ncol = 3)
  H <- (hsl[, 1] / 360) %% 1
  S <- hsl[, 2]
  L <- hsl[, 3]


  if (S == 0) {
    R <- G <- B <-  L
  } else {
    if (L < 0.5)
      var_2 <- L * (1 + S)
    else
      var_2 <- (L + S) - (S * L)

    var_1 <- 2 * L - var_2

    R <- Hue_2_RGB(var_1, var_2, H + (1 / 3))
    G <- Hue_2_RGB(var_1, var_2, H)
    B <- Hue_2_RGB(var_1, var_2, H - (1 / 3))
  }
  out <- cbind(R, G, B)

  if (any(out < 0) || any(out > 1))
    stop("Invalid hsl range.")

  out
}

Hue_2_RGB <- function(v1, v2, vH) {
  if (vH < 0)
    vH <- vH + 1
  if(vH > 1)
    vH <- vH - 1
  if ((6 * vH) < 1)
    return (v1 + (v2 - v1) * 6 * vH)
  if ((2 * vH) < 1)
    return (v2)
  if ((3 * vH) < 2)
    return (v1 + (v2 - v1) * ((2 / 3) - vH) * 6)
  return (v1)
}
