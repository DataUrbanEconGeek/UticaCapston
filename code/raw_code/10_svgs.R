library(svglite)

path1 <- "../../figures/exploratory_figures/01_yr-built-map-2.png"

svglite(file = path1)
plot(runif(10), runif(10))
dev.off()