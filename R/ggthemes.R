# ggplot2 themes
# Modified from ggthemr_blogpost draft.

data(markthemes)

# Utility functions -------------------------------------------------------

#' @importFrom ggthemr ggthemr ggthemr_palette
orig_defaults <- ggthemr::ggthemr(set_theme = FALSE)$geom_defaults$orig

#' Reset ggplot2 geoms to defaulst
#'
#' @export
ggreset <- function() {
  for (one_geom_defaults in orig_defaults) {
    do.call(what = update_geom_defaults, args = one_geom_defaults)
  }
}


#' Adventure Time theme, palette, and scales
#'
#' @export
theme_at <- function() {
  update_geom_defaults("point", list(fill = atswatch[1], shape = 24))
  markthemes$at$theme +
    theme(text = element_text(family = "ShortStack"),
          plot.title=element_text(family = "PiecesOfEight",
                                  face = "plain", size = 20))
}


# Theme inspired by Grandma Kangas (EMK) ----------------------------------

#' ggplot theme inspired by Grandma Kangas
#'
#' @export
theme_emk <- function() {
  ggreset()
  update_geom_defaults("point",
                       list(fill = emkswatch[4], colour = emkswatch[4],
                            stroke = 0, shape = 21, size = 3))
  markthemes$emk$theme +
    theme(text = element_text(family = "Courgette", face = "bold"),
          # axis.text = element_text(color = "black"),
          panel.grid.major = element_line(size = 0.3, linetype = 1, color = "#03536A"),
          plot.title = element_text(family = "Cookie", face = "plain",
                                    size = 20))
}


#' My own personal themes
#'
#' @describeIn theme_markdark dark ggplot2 theme
#' @export
theme_markdark <- function() {
  ggreset()
  update_geom_defaults("point",
                       list(color = "#c3c3c3", shape = 21, alpha = 0.8,
                            size = 3, stroke = 0.5,
                            fill = mypal$mid[1]))
  markthemes$dark$theme +
    theme(text = element_text(family = "Cutive Mono", face = "plain"),
          axis.title = element_text(family = "Domine", face = "plain"),
          panel.grid.major = element_line(size = 0.1, linetype = 1),
          plot.title=element_text(family = "Domine", face = "plain", size = 20))
}

#' @describeIn theme_markdark light ggplot2 theme
#' @export
theme_marklight <- function() {
  ggreset()
  update_geom_defaults("point", list(color = "#222222", shape = 21, alpha = 0.8,
                                     size = 3, stroke = 0.5,
                                     fill = mypal$mid[1]))
  markthemes$light$theme +
    theme(text = element_text(family = "Cutive Mono", face = "plain"),
          axis.title = element_text(family = "Domine", face = "plain"),
          panel.grid.major = element_line(size = 0.5, linetype = 1),
          plot.title=element_text(family = "Domine", face = "plain", size = 20))
}


# Scales ------------------------------------------------------------------

#' @describeIn theme_markdark discrete color scale
#' @export
scale_color_markdark_d <- markthemes$dark$scales$scale_color_discrete

#' @describeIn theme_markdark continuous color scale
#' @export
scale_color_markdark_c <- markthemes$dark$scales$scale_color_continuous

#' @describeIn theme_markdark discrete color scale
#' @export
scale_color_marklight_d <- markthemes$light$scales$scale_color_discrete

#' @describeIn theme_markdark continuous color scale
#' @export
scale_color_marklight_c <- markthemes$light$scales$scale_color_continuous

#' @describeIn theme_markdark discrete fill scale
#' @export
scale_fill_markdark_d <- markthemes$dark$scales$scale_fill_discrete

#' @describeIn theme_markdark continuous fill scale
#' @export
scale_fill_markdark_c <- markthemes$dark$scales$scale_fill_continuous

#' @describeIn theme_markdark discrete fill scale
#' @export
scale_fill_marklight_d <- markthemes$light$scales$scale_fill_discrete

#' @describeIn theme_markdark continuous fill scale
#' @export
scale_fill_marklight_c <- markthemes$light$scales$scale_fill_continuous


#' @describeIn theme_emk discrete color scale
#' @export
scale_color_emk_d <- markthemes$emk$scales$scale_color_discrete

#' @describeIn theme_emk continuous color scale
#' @export
scale_color_emk_c <- markthemes$emk$scales$scale_color_continuous

#' @describeIn theme_emk discrete fill scale
#' @export
scale_fill_emk_d <- markthemes$emk$scales$scale_fill_discrete

#' @describeIn theme_emk continuous fill scale
#' @export
scale_fill_emk_c <- markthemes$emk$scales$scale_fill_continuous




#' Make the elements of a ggplot object larger
#'
#' @param plot ggplot object to embiggen
#' @param factor Factor by which to scale sizes
#' @param what What parts of the plot to embiggen?
#'
#' @importFrom ggplot2 last_plot
#' @export
embiggen <- function(plot = last_plot(), factor = 1.2,
                     what = c("text", "title", "points", "lines")) {

  what <- match.arg(what, several.ok = TRUE)
  thm <- plot$theme
  lyrs <- plot$layers

  if ("text" %in% what) {
    checkfun <- function(x, y) {
      y && !is.atomic(x) && inherits(x$size, "numeric")
    }
    hasnumsize <- purrr::map2_lgl(thm, grepl("text", names(thm)), checkfun)

    setfun <- function(x) {
      x$size <- x$size * factor
      x
    }
    thm[hasnumsize] <- purrr::map(thm[hasnumsize], setfun)
  }

  if ("title" %in% what) {
    thm$plot.title$size <- thm$plot.title$size * factor
  }

  if ("points" %in% what || "lines" %in% what) {
    setfun <- function(x) {
      x$geom$default_aes$size <- x$geom$default_aes$size * factor
      x
    }
    lyrs <- lapply(lyrs, setfun)
  }

  plot$theme <- thm
  plot$layers <- lyrs

  plot
}


#' Utility wrapper for color and fill scales
#'
#' @param themename Name of a theme with a \code{scale_xxx_[theme]_[c/d]} function.
#' @param ... Passed to scale function
#' @export
#' @describeIn fill_d discrete fill
fill_d <- function(themename, ...) {
  arglist <- list(...)
  funname <- sprintf("scale_fill_%s_d", themename)
  if (!exists(funname)) stop(sprintf("Function %s not found", funname))
  do.call(funname, args = arglist)
}

#' @describeIn fill_d continuous fill
#' @export
fill_c <- function(themename, ...) {
  arglist <- list(...)
  funname <- sprintf("scale_fill_%s_c", themename)
  if (!exists(funname)) stop(sprintf("Function %s not found", funname))
  do.call(funname, args = arglist)
}

#' @describeIn fill_d discrete color
#' @export
color_d <- function(themename, ...) {
  arglist <- list(...)
  funname <- sprintf("scale_color_%s_d", themename)
  if (!exists(funname)) stop(sprintf("Function %s not found", funname))
  do.call(funname, args = arglist)
}

#' @describeIn fill_d continuous color
#' @export
color_c <- function(themename, ...) {
  arglist <- list(...)
  funname <- sprintf("scale_color_%s_c", themename)
  if (!exists(funname)) stop(sprintf("Function %s not found", funname))
  do.call(funname, args = arglist)
}
