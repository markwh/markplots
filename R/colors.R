# Colors

# Adventure time colors
atswatch <- c("#FDB10F", "#26CEFE", "#186DFC", "#FBFBFB", "#9CE073")
pbswatch <- c("#CC007A", "#FF039A", "#FF8CD1", "#851B5A", "#FFD000")


# Grandma Erma colors
emkswatch <- c("#106077", "#CF4E30", "#951A1C", "#D5A333", "#003837",
              "#687C35", "#BFE461", "#88522E", "#ED7C80", "#E9C9BE")

# My own theme colors
data(mypal)
myswatch3 <- c(mypal$darkest[1:4], "#222222", mypal$mid[1:4])
lightswatch <- c(mypal$darkest[1:4], "#555555", mypal$mid[1:4])

plot_swatch <- function(swatch) {
  barplot(rep(1, length(swatch)), col = swatch)
}
