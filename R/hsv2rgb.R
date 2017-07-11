
#' A fixed version--colorscience does not work
#'
#' @param hsv hue, degrees on [0, 360); saturation, on [0, 1]; lightness, on [0, 1]
#'
#' @return Matrix of RGB values on [0, 1]

hsv2rgb <- function(hsv) {
  hsv <- matrix(hsv, ncol = 3)
  H <- (hsv[, 1] / 360) %% 1
  S <- hsv[, 2]
  V <- hsv[, 3]

  R <- G <- B <- var_1 <- var_2 <- rep(NA_real_, nrow(hsv))

  var_h <- H * 6
  var_i = floor(var_h)
  var_1 = V * (1 - S)
  var_2 = V * (1 - S * (var_h - var_i))
  var_3 = V * (1 - S * (1 - (var_h - var_i)))

  rgb <- matrix(nrow = nrow(hsv), ncol = 3)

  rgb[var_i == 0, ] <- matrix(c(    V[var_i == 0], var_3[var_i == 0],
                                    var_1[var_i == 0]),
                              nrow = 3)
  rgb[var_i == 1, ] <- matrix(c(var_2[var_i == 1],     V[var_i == 1],
                                var_1[var_i == 1]),
                              nrow = 3)
  rgb[var_i == 2, ] <- matrix(c(var_1[var_i == 2],     V[var_i == 2],
                                var_3[var_i == 2]),
                              nrow = 3)
  rgb[var_i == 3, ] <- matrix(c(var_1[var_i == 3], var_2[var_i == 3],
                                V[var_i == 3]),
                              nrow = 3)
  rgb[var_i == 4, ] <- matrix(c(var_3[var_i == 4], var_1[var_i == 4],
                                V[var_i == 4]),
                              nrow = 3)
  rgb[var_i == 5, ] <- matrix(c(    V[var_i == 5], var_1[var_i == 5],
                                    var_2[var_i == 5]),
                              nrow = 3)

  out <- rgb
  colnames(out) <- c("R", "G", "B")
  out
}




