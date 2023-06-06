library(lattice)
library(latticeExtra)

nz <- 3
x <- 2**1
y <- 2**2
d <- t(matrix(rep(1:2, length.out=x*y), nrow=x, ncol=y))

p <- levelplot(
  d,
  xlab=NULL,
  ylab=NULL,
  # scales=list(x=list(at=NULL), y=list(at=NULL)), # no ticks
  scales=list(x=list(at=1:y, labels=c("AB'", "AB", "A'B", "A'B'")), y=list(at=1:x, labels=c("C'", "C"))),
  cuts=nz-1, # to map 1:3 to white:black
  # colorkey=NULL,
  col.regions=colorRampPalette(colors=c("white", "black"))(nz)
)
# print(p)

# question synthesis by function
get_question_by_f <- function(f) {
  dat <- data.frame(
    A=c(0, 1, 0, 1, 0, 1, 0, 1),
    B=c(0, 0, 1, 1, 0, 0, 1, 1),
    C=c(0, 0, 0, 0, 1, 1, 1, 1)
  )
  dat$value <- ifelse(f(dat$A, dat$B, dat$C), 1, 0)
  dat$source <- ifelse(dat$value, "yes", "no")
  dat$x <- factor(
    paste0(ifelse(dat$A == 1, "A:yes ", "A:no "), ifelse(dat$B == 1, "B:yes", "B:no")),
    levels=c("A:no B:no", "A:yes B:no", "A:no B:yes", "A:yes B:yes")
  )
  dat$y <- factor(ifelse(dat$C == 1, "C:yes", "C:no"), levels=c("C:no", "C:yes"))

  return(dat)
}

and3 <- function(a, b, c) {
  return(bitwAnd(bitwAnd(a, b), c))
}
datAND <- get_question_by_f(function(a, b, c) bitwAnd(bitwAnd(a, b), c))

dat <- data.frame(
  A=c(0, 1, 0, 1, 0, 1, 0, 1),
  B=c(0, 0, 1, 1, 0, 0, 1, 1),
  C=c(0, 0, 0, 0, 1, 1, 1, 1)
)
dat$value <- ifelse(and3(dat$A, dat$B, dat$C), 1, 0)
dat$source <- ifelse(dat$value, "yes", "no")
dat$x <- factor(
  paste0(ifelse(dat$A == 1, "A:yes ", "A:no "), ifelse(dat$B == 1, "B:yes", "B:no")),
  levels=c("A:no B:no", "A:yes B:no", "A:no B:yes", "A:yes B:yes")
)
dat$y <- factor(ifelse(dat$C == 1, "C:yes", "C:no"), levels=c("C:no", "C:yes"))

lp <- levelplot(
  value ~ x+y,
  aspect="iso",
  data=dat,
  xlab=NULL, ylab=NULL, colorkey=NULL,
  col.regions=colorRampPalette(colors=c("red", "green"))(2)
)
xp <- xyplot(y ~ x, data=dat,
  panel = function(y, x, ...) {
    ltext(x = x, y = y, labels = dat$source, cex = 1, font = 2,
      fontfamily = "HersheySans")
    # panel.grid(h=1, v=3, x=2, y=4)
    #panel.abline(h=c(1.5), from=1.5, to=2.5)
    #panel.abline(v=c(1.5, 2.5, 3.5), from=0.5, to=2.5)
  }
  )

p <- lp + xp
print(p)

# joke result, "and" form, per logician desire
# the "backwards" row/col numbers make
# the data match the input format
m <- matrix(nrow=3, ncol=8, data=c(
   0, -1, -1,  # 100
   0,  0, -1,  # 110
  -1, -1, -1,  # 010
  -1, -1, -1,  # 000
   0, -1, -1,  # 101
   0,  0,  1,  # 111
  -1, -1, -1,  # 011
  -1, -1, -1   # 001
))

levelplot(
  m,
  col.regions=colorRampPalette(colors=c("red", "yellow", "green")),
  xlab="respondent",
  ylab="want beer?",
  colorkey=list(at=c(-1, -.33, .33, 1), labels=list(at=c(-0.67, 0, 0.67), labels=c("No", "I don't know", "Yes!"))),
  scales=list(
    x=list(at=1:3, labels=c("A", "B", "C")),
    y=list(at=1:8, labels=c(
      "A:yes B:no C:no", "A:yes B:yes C:no", "A:no B:yes C:no", "A:no B:no C:no",
      "A:yes B:no C:yes", "A:yes B:yes C:yes", "A:no B:yes C:yes", "A:no B:no C:yes"
    ))
  )
)

dat <- data.frame(
  y=c("A yes B no C no", "A yes B yes C no", "A no B yes C no", "A no B no C no",
      "A yes B no C yes", "A yes B yes C yes", "A no B yes C yes", "A no B no C yes"),
  value=c(0, 0, 0, 0, 0, 1, 0, 0)+1,
  source=c(0, 0, 0, 0, 0, 1, 0, 0)
)

# mockup of the entire joke in a single plot
# there are 256 bartender questions, 8 logician desires, and 3 output steps
# 256 * 8 * 3 = 6144
# can be shown in a square grid of 2*32=64 rows, 3*32=96 columns
# (2*32 * 3*32 = 6144)
# if I use an 8 row, 3 column arrangement for each joke, that's 32 jokes
# across the columns, 8 jokes down the rows.
x <- 2*32
y <- 3*32
d <- t(matrix(sample(1:3, size=x*y, replace=T), nrow=y, ncol=x))
levelplot(d,
  col.regions=colorRampPalette(colors=c("red", "yellow", "green")),
)

# function index plot
dat <- data.frame(
  A=c(0, 0, 0, 0, 1, 1, 1, 1),
  B=c(0, 0, 1, 1, 0, 0, 1, 1),
  C=c(0, 1, 0, 1, 0, 1, 0, 1)
)
dat$y <- factor(ifelse(dat$A == 1, "A:yes", "A:no"), levels=c("A:no", "A:yes"))
dat$x <- factor(
  paste0(ifelse(dat$B == 1, "B:yes ", "B:no "), ifelse(dat$C == 1, "C:yes", "C:no")),
  levels=c("B:no C:no", "B:no C:yes", "B:yes C:no", "B:yes C:yes")
)
dat$value <- 2**(0:7)
dat$source <- as.character(dat$value)

lp <- levelplot(
  value ~ x+y,
  data=dat,
  xlab=NULL, ylab=NULL, colorkey=NULL,
  aspect="iso",
  col.regions=colorRampPalette(colors=c("white", "gray"))(256)
)
xp <- xyplot(y ~ x,
             data=dat,
             panel = function(y, x, ...) {
               ltext(x = x, y = y, labels = dat$source, cex = 1, font = 2,
                     fontfamily = "HersheySans")
             }
)
lp + xp

source("utils.R")

dat <- get_question(128)
p <- plotit(dat)
print(p)

# Compute A's response, encoded as -1 (no), 0 (I don't know), 1 (yes)
# A's response depends on A's own answer (dat$A) and the question (dat$value)
get_answerA <- function(dat) {
  ansA <-
    c(
      ifelse(
        rep(
          length(unique(dat[dat$A == 0,]$value)) == 1,
          4
        ),
        ifelse(dat[dat$A == 0,]$value[1] == 1, 1, -1), rep(0, 4)
      ),
      ifelse(
        rep(
          length(unique(dat[dat$A == 1,]$value)) == 1,
          4
        ),
        ifelse(dat[dat$A == 1,]$value[1] == 1, 1, -1), rep(0, 4)
      )
    )
  return(ansA)
}

get_answerB <- function(dat) {
  ansB <-
    c(
      ifelse(dat$ansA != 0,
        dat$ansA, # if ansA is known, propagate
        5 # clearly wrong
      )
    )
  return(ansB)
}

# tests
library(RUnit)
# "odd number"
dat <- get_question(150)
dat$ansA <- get_answerA(dat)
checkEquals(c(0, 0, 0, 0, 0, 0, 0, 0), dat$ansA, "ansA for 'xor' (150)")

dat <- get_question(128)
dat$ansA <- get_answerA(dat)
checkEquals(c(-1, -1, -1, -1, 0, 0, 0, 0), dat$ansA, "ansA for 'and' (128)")

# "any"
dat <- get_question(254)
dat$ansA <- get_answerA(dat)
checkEquals(c(0, 0, 0, 0, 1, 1, 1, 1), dat$ansA, "ansA for 'any' (254)")

p <- plotit(dat)
print(p)

# Short Circuiting
# For each dat entry, if A is held at its value, do all rows with that A value
# have the same $value? This is kind of an odd thing in R - a dataframe entry
# depends on multiple other dataframe entries. A for loop works; is there an
# more R-idiomatic method?
dat <- get_question(128)
dat$ansA <- get_answerA(dat)

p <- plotit(dat)
print(p)
ssA <- c()
for (row in 1:nrow(dat)) {
  ssA <- append(ssA, ifelse(length(unique(dat[dat$A == dat$A[row],]$value)) == 1, T, F))
}
dat$ssA <- ssA
dat$ansA <- ifelse(dat$ssA, dat$value*2-1, 0)

ssAB <- c()
for (row in 1:nrow(dat)) {
  ssAB <- append(ssAB, dat$ssA[row] | ifelse(length(unique(dat[dat$A == dat$A[row] & dat$B == dat$B[row],]$value)) == 1, T, F))
}
dat$ssAB <- ssAB
dat$ansB <- ifelse(dat$ssAB, dat$value * 2 - 1, 0)
library(dplyr)
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
