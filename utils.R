library(lattice)
library(latticeExtra)
library(dplyr)

# 2-logician scenario
get_questionAB <- function(index) {
  dat <- data.frame(
    A=c(0, 0, 1, 1),
    B=c(0, 1, 0, 1)
  )
  dat$value <- ifelse(bitwAnd(2**(0:3), index), 1, 0)
  dat$source <- ifelse(dat$value, "yes", "no")
  dat$y <- factor(ifelse(dat$A == 1, "A:yes", "A:no"), levels=c("A:no", "A:yes"))
  dat$x <- factor(ifelse(dat$B == 1, "B:yes", "B:no"), levels=c("B:no", "B:yes"))
  return(dat)
}

get_jokeAB <- function(index) {
  dat <- get_questionAB(index)

  ssA <- c()
  for (row in 1:nrow(dat)) {
    ssA <- append(ssA, ifelse(length(unique(dat[dat$A == dat$A[row],]$value)) == 1, T, F))
  }
  dat$ansA <- ifelse(ssA, dat$value*2-1, 0)

  ansB <- c()
  for (row in 1:nrow(dat)) {
    ansB <- append(ansB, case_when(
      dat$ansA[row] != 0 ~ dat$ansA[row],
      length(unique(dat[dat$ansA == 0 & dat$B == dat$B[row],]$value)) == 1 ~ dat$value[row]*2-1,
      TRUE ~ 0
    ))
  }
  dat$ansB <- ansB

  return(dat)
}

# levelplot prefers a matrix for its input data ('x'), but I like data frames.
# Solution: produce question/joke data as data frames, convert to matrix for
# plotting.

# Here's how I used to build the 2-logician matrix (for joke 8, "AND"):
# m <- matrix(nrow=2, ncol=4, data=c(
#   -1, -1, # 00
#   -1, -1, # 01
#    0, -1, # 10
#    0,  1  # 11
# ))
#
# ... and here's what the input data looks like for joke 8, "AND":
# > j$ansA
# [1] -1 -1  0  0
# > j$ansB
# [1] -1 -1 -1  1
#
# c(rbind()) slices ansA and ansB into the needed order.

to_matrix <- function(joke) {
  # I'll handle the 2- and 3-input forms as case statements, probably some
  # commonality will emerge, then can refactor.
  m <- case_when(
    nrow(joke) == 4 ~ matrix(data=c(rbind(joke$ansA, joke$ansB)), nrow=2, ncol=4),
    # to do: 8
    T ~ c(0)
  )

  return(m)
}


# question synthesis by index
# 3-logician scenario
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

