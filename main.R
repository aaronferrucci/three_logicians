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
print(p)

dat <- data.frame(
  expand.grid(x=c("AB'", "AB", "A'B", "A'B'"), y=c("C'", "C")),
  value=c(0, 0, 0, 0, 0, 1, 0, 0)+1,
  source=c(0, 0, 0, 0, 0, 1, 0, 0)
)
lp <- levelplot(value ~ x+y, data=dat, xlab=NULL, ylab=NULL, colorkey=NULL,
  col.regions=colorRampPalette(colors=c("red", "green"))(2))
xp <- xyplot(y ~ x, data=dat,
  panel = function(y, x, ...) {
    ltext(x = x, y = y, labels = dat$source, cex = 1, font = 2,
      fontfamily = "HersheySans")
  })

p <- lp + xp
print(p)
