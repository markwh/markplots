# ggthemr objects


# Palettes ----------------------------------------------------------------


#' A light theme
#'
#' @describeIn theme_markdark light ggthemr palette
#' @export
marklightpal <- ggthemr_palette(swatch = c("#ffffff", lightswatch),
                                gradient = mypal$darkest[c(1, 3)],
                                background = "#efede8", #To match hugo site background
                                line = c("#222222", "#222222"),
                                gridline = "#c3c3c3",
                                text = rep("#222222", 2))


#' @describeIn theme_markdark dark ggthemr palette
#' @export
markdarkpal <- ggthemr_palette(swatch = c("#ffffff", myswatch3),
                               gradient = mypal$darkest[c(1, 3)],
                               background = "#555555",
                               line = c("#6e6e6e", "#6e6e6e"),
                               gridline = "#c3c3c3", text = rep("#c3c3c3", 2))

#' Adventure time palette and theme from ggthemr
#'
#' @export
atpal <- ggthemr_palette(swatch = c(atswatch[-2], pbswatch),
                         gradient = atswatch[c(1, 2)],
                         # background = mypal$lightest[1],
                         background = atswatch[2],
                         line = c("#6e6e6e", "#6e6e6e"),
                         gridline = "#c3c3c3", text = c("#444444", "#444444"))

#' @describeIn theme_emk ggthemr palette
emkpal_dark <- ggthemr_palette(swatch = emkswatch,
                               gradient = emkswatch[c(8, 9)],
                               background = emkswatch[1],
                               text = rep(emkswatch[9], 2),
                               line = rep(emkswatch[5], 2))

markpals <- list(
  light = marklightpal,
  dark = markdarkpal,
  at = atpal,
  emk = emkpal_dark
)



# Themes ------------------------------------------------------------------
#' @describeIn theme_markdark light ggthemr theme
#' @export
marklighttheme <- ggthemr(marklightpal, set_theme = FALSE, type = "outer")


#' @describeIn theme_markdark dark ggthemr theme
#' @export
markdarktheme <- ggthemr(markdarkpal, set_theme = FALSE, type = "outer")

#' @describeIn theme_at ggthemr theme
#' @export
attheme <- ggthemr(atpal, set_theme = FALSE, type = "outer")


#' @describeIn theme_emk ggthemr theme
emktheme <- ggthemr(palette = emkpal_dark, type = "outer",
                    line_weight = 0.75, set_theme = FALSE)

markthemes <- list(
  light = marklighttheme,
  dark = markdarktheme,
  at = attheme,
  emk = emktheme
)
