
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

  R <- G <- B <- var_1 <- var_2 <- rep(NA_real_, nrow(hsl))


  S0 <- S == 0
  if (any(S0)) {
    R[S0] <- G[S0] <- B[S0] <- L[S0]
  }

  Lsm <- (L < 0.5 & !S0)
  Llg <- (!Lsm & !S0)

  var_2[Lsm] <- L[Lsm] * (1 + S[Lsm])
  var_2[Llg] <- (L[Llg] + S[Llg]) - (L[Llg] * S[Llg])
  var_1[!S0] <- 2 * L[!S0] - var_2[!S0]

  R[!S0] <- Hue_2_RGB(var_1[!S0], var_2[!S0], H[!S0] + (1 / 3))
  G[!S0] <- Hue_2_RGB(var_1[!S0], var_2[!S0], H[!S0])
  B[!S0] <- Hue_2_RGB(var_1[!S0], var_2[!S0], H[!S0] - (1 / 3))

  out <- cbind(R, G, B)

  if (any(out < 0) || any(out > 1))
    stop("Invalid hsl range.")
  colnames(out) <- c("R", "G", "B")
  out
}

Hue_2_RGB <- function(v1, v2, vH) {

  vsm <- vH < 0
  vlg <- vH > 1

  vH[vsm] <- vH[vsm] + 1
  vH[vlg] <- vH[vlg] - 1

  c1 <- vH < 2/3
  c2 <- vH < 1/2
  c3 <- vH < 1/6

  out <- v1
  out[c1] <- v1[c1] + (v2[c1] - v1[c1]) * ((2 / 3) - vH[c1]) * 6
  out[c2] <- v2[c2]
  out[c3] <- v1[c3] + (v2[c3] - v1[c3]) * 6 * vH[c3]


  out
}
