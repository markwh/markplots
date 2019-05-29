# Plots for testing and demonstrating themes/colors.

set.seed(8142)
#' @export
gg_point1 <- ggplot(mtcars, aes(x = disp, y = hp, fill = mpg, size = mpg)) +
  geom_point()

library(dplyr)
#' @importFrom dplyr "%>%"
#' @export
gg_point2 <- diamonds %>%
  sample_n(50) %>%
  filter(clarity %in% unique(clarity)[1:5]) %>%
  ggplot(aes(x=carat, y=price, fill=clarity)) +
  geom_point() +
  ggtitle("Hey! Look at this plot.") +
  ylab("Price, USD") +
  xlab("Carat")
#
#
#
# foo <- gg_point2 + theme_markdark() + scale_fill_markdark_d()
#
# embiggen(foo)
#
# bar <- gg_point2 + theme_marklight() +
#   scale_fill_marklight_d() +
#   ggtitle("Hey! Look at this plot.") +
#   ylab("Price, USD") +
#   xlab("Carat")
# embiggen(bar, what = "points")
