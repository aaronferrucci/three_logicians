# question synthesis by index
get_question <- function(index) {
  dat <- data.frame(
    A=c(0, 0, 0, 0, 1, 1, 1, 1),
    B=c(0, 0, 1, 1, 0, 0, 1, 1),
    C=c(0, 1, 0, 1, 0, 1, 0, 1)
  )
  dat$value <- ifelse(bitwAnd(2**(0:7), index), 1, 0)
  dat$source <- ifelse(dat$value, "yes", "no")
  dat$y <- factor(ifelse(dat$A == 1, "A:yes", "A:no"), levels=c("A:no", "A:yes"))
  dat$x <- factor(
    paste0(ifelse(dat$B == 1, "B:yes ", "B:no "), ifelse(dat$C == 1, "C:yes", "C:no")),
    levels=c("B:no C:no", "B:no C:yes", "B:yes C:no", "B:yes C:yes")
  )
  return(dat)
}

plotit <- function(dat) {
  # build the palette. Special casing is needed when both of "no", "yes" are
  # not present.
  colors <- c()
  if ("no" %in% dat$source) {
    colors <- append(colors, "red")
  }
  if ("yes" %in% dat$source) {
    colors <- append(colors, "green")
  }
  uniques <- length(unique(dat$source))

  lp <- levelplot(
    value ~ x+y,
    data=dat,
    xlab=NULL, ylab=NULL, colorkey=NULL,
    aspect="iso",
    col.regions=colorRampPalette(colors=colors)(uniques)
  )
  xp <- xyplot(y ~ x,
               data=dat,
               panel = function(y, x, ...) {
                 ltext(x = x, y = y, labels = dat$source, cex = 1, font = 2,
                       fontfamily = "HersheySans")
               }
  )
  p <- lp + xp
  
  return(p)
}