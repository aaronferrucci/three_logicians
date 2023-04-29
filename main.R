library(lattice)

nz <- 3
x <- 2**1
y <- 2**2
d <- t(matrix(rep(1:nz, length.out=x*y), nrow=x, ncol=y))

p <- levelplot(
  d,
  xlab=NULL,
  ylab=NULL,
  # scales=list(x=list(at=NULL), y=list(at=NULL)), # no ticks
  scales=list(x=list(at=1:y, labels=c("AB'", "AB", "A'B", "A'B'")), y=list(at=1:x, labels=c("C'", "C"))),
  cuts=nz-1, # to map 1:3 to white:black
  col.regions=colorRampPalette(colors=c("white", "black"))(nz),
  colorkey=NULL
)
print(p)