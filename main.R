library(lattice)

nz <- 3
rows <- 2**3
cols <- 2**2
d <- matrix(rep(1:nz, length.out=rows*cols), nrow=rows, ncol=cols)

p <- levelplot(
  d,
  xlab=NULL,
  ylab=NULL,
  # scales=list(x=list(at=NULL), y=list(at=NULL)), # no ticks
  scales=list(x=list(at=1:rows), y=list(at=1:cols)),
  cuts=nz-1, # to map 1:3 to white:black
  col.regions=colorRampPalette(colors=c("white", "black"))(nz),
  colorkey=NULL
)
print(p)