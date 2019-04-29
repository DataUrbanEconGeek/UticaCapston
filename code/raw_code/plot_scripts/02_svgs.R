library(svglite)

path1 <- "../../figures/exploratory_figures/01_yr-built-map-3.png"

svglite(file = path1, pointsize = 6)
plot(year_map2)
dev.off()