library(dplyr)
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

get_joke <- function(index) {
  dat <- get_question(index)

  # Short Circuiting
  # For each dat entry, if A is held at its value, do all rows with that A value
  # have the same $value? If s, A "short circuits" (the B and C values don't matter)
  # This is kind of an odd thing in R - a dataframe entry
  # depends on multiple other dataframe entries. A for loop works; is there an
  # more R-idiomatic method?
  ssA <- c()
  for (row in 1:nrow(dat)) {
    ssA <- append(ssA, ifelse(length(unique(dat[dat$A == dat$A[row],]$value)) == 1, T, F))
  }
  dat$ansA <- ifelse(ssA, dat$value*2-1, 0)

  ssAB <- c()
  for (row in 1:nrow(dat)) {
    ssAB <- append(ssAB, ssA[row] | ifelse(length(unique(dat[dat$A == dat$A[row] & dat$B == dat$B[row],]$value)) == 1, T, F))
  }
  dat$ansB <- ifelse(ssAB, dat$value * 2 - 1, 0)

  ansC <- c()
  for (row in 1:nrow(dat)) {
    ansC <- append(ansC, case_when(
      dat$ansA[row] != 0 ~ dat$ansA[row],
      dat$ansB[row] != 0 ~ dat$ansB[row],
      length(unique(dat[dat$ansA == 0 & dat$ansB == 0 & dat$C == dat$C[row],]$value)) == 1 ~ dat$value[row]*2-1,
      TRUE ~ 0
    ))
  }
  dat$ansC <- ansC

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

