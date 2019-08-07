install.packages("plot3D")
install.packages("plot3Drgl")
install.packages("scatterplot3d")
library(plot3D)
library(plot3Drgl)
library(rgl)

mtcars
scatter3D(mtcars$mpg, mtcars$cyl, mtcars$hp, theta = 45, 
          phi = 0, bty = "g", cex = 2, pch = 20,
          ticktype = "simple", type = "h")
plotrgl()


iris
hist3D(iris$Sepal.Length, iris$Petal.Width, colvarr=as.numeric(iris$Species))


library(scatterplot3d)
# create column indicating point color
mtcars$pcolor[mtcars$cyl==4] <- "red"
mtcars$pcolor[mtcars$cyl==6] <- "blue"
mtcars$pcolor[mtcars$cyl==8] <- "darkgreen"
with(mtcars, {
  s3d <- scatterplot3d(disp, wt, mpg,        # x y and z axis
                       color=pcolor, pch=19,        # circle color indicates no. of cylinders
                       type="h", lty.hplot=2,       # lines to the horizontal plane
                       scale.y=.75,                 # scale y axis (reduce by 25%)
                       main="3-D Scatterplot Example 4",
                       xlab="Displacement (cu. in.)",
                       ylab="Weight (lb/1000)",
                       zlab="Miles/(US) Gallon")
  s3d.coords <- s3d$xyz.convert(disp, wt, mpg)
  text(s3d.coords$x, s3d.coords$y,     # x and y coordinates
       labels=row.names(mtcars),       # text to plot
       pos=4, cex=.5)                  # shrink text 50% and place to right of points)
  # add the legend
  legend("topleft", inset=.05,      # location and inset
         bty="n", cex=.5,              # suppress legend box, shrink text 50%
         title="Number of Cylinders",
         c("4", "6", "8"), fill=c("red", "blue", "darkgreen"))
})





{rect3D(x0 = 0, y0 = 0.5, z0 = 0, x1 = 1, z1 = 5, 
       ylim = c(0, 1), bty = "g", facets = TRUE, 
       border = "red", col ="#7570B3", alpha=0.5,
       lwd = 2, phi = 20)
plotrgl()}



quakes
par(mfrow = c(1, 1))
panelfirst <- function(pmat) {
  zmin <- min(-quakes$depth)
  XY <- trans3D(quakes$long, quakes$lat,
                z = rep(zmin, nrow(quakes)), pmat = pmat)
  scatter2D(XY$x, XY$y, colvar = quakes$mag, pch = ".",
            cex = 2, add = TRUE, colkey = FALSE)
  xmin <- min(quakes$long)
  XY <- trans3D(x = rep(xmin, nrow(quakes)), y = quakes$lat,
                z = -quakes$depth, pmat = pmat)
  scatter2D(XY$x, XY$y, colvar = quakes$mag, pch = ".",
            cex = 2, add = TRUE, colkey = FALSE)
}
with(quakes, scatter3D(x = long, y = lat, z = -depth, colvar = mag,
                       pch = 16, cex = 1.5, xlab = "longitude", ylab = "latitude",
                       zlab = "depth, km", clab = c("Richter","Magnitude"),
                       main = "Earthquakes off Fiji", ticktype = "detailed",
                       panel.first = panelfirst, theta = 10, d = 2,
                       colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75))
)
plotrgl()

par (mfrow = c(1, 2))
arrows2D(x0 = runif(10), y0 = runif(10),
         x1 = runif(10), y1 = runif(10), colvar = 1:10,
         code = 3, main = "arrows2D")
arrows3D(x0 = runif(10), y0 = runif(10), z0 = runif(10),
         x1 = runif(10), y1 = runif(10), z1 = runif(10),
         colvar = 1:10, code = 1:3, main = "arrows3D", colkey = FALSE)
plotrgl()

