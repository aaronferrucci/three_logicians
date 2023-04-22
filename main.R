library(lattice)

d <- matrix(sample(c(1, 2, 3), size=8, replace=T))

p <- levelplot(
  d,
  xlab=NULL,
  ylab=NULL,
  scales=list(x=list(at=NULL),
              y=list(at=NULL)),
  col.regions=c("#FFFFFF", "#AAAAAA", "#555555", "#000000"),
  colorkey=NULL
)
print(p)