# # Experiments with coordinate systems and conversions
# teststring <- "Hello, world!"
# testnumber <- formatC(pi, format = "f", digits = 5L)
# plot(1:10)
# par("usr") # (x1, x2, y1, y2): extremes of user coordinates of plotting region
# par("plt") # (x1, x2, y1, y2): coordinates of plot region as fractions of current figure region
# par("pin") # (width, height): current plot dimensions in inches
# par("fin") # (width, height): current figure region dimensions in inches
# par("fig") # (x1, x2, y1, y2): NDC coords of figure region in the display region of the device
# 
# par("plt") * rep(par("fin"), each = 2) # (x1, x2, y1, y2): extremes of plotting region in inches from origin (presumably bottom left corner)
# 
# # Plotting region measurements in "user" dimensions
# par("usr")[2L] - par("usr")[1L] # plotting region width
# par("usr")[4L] - par("usr")[3L] # plotting region height
# 
# # Plotting region measurements in inches
# corner.inches <- par("plt") * rep(par("fin"), each = 2)
# corner.inches[2L] - corner.inches[1L] == par("pin")[1L] # plotting region width
# corner.inches[4L] - corner.inches[3L] == par("pin")[2L] # plotting region height
# 
# 
# # Coordinate systems for grconvertXY
# # "user"  : user coordinates
# # "inches": inches
# # "device": device coordinate system
# # "ndc"   : normalized device coordinates
# # "nfc"   : normalized figure coordinates
# # "npc"   : normalized plot coordinates
# # "nic"   : normalized inner region coordinates
# 
# plot(1:4)
# for(tp in c("in", "dev", "ndc", "nfc", "npc", "nic"))
#     print(grconvertX(c(1.0, 4.0), "user", tp))
# 
# par(new = TRUE)
# plot(x = 1, y = 4, pch = 19, col = "blue")
# xy.coords(x=1,y=4)







def.par <- par(no.readonly = TRUE)
set.seed(0724)
x <- rnorm(500, sd = 100)
x <- rnorm(500, sd = 0.01)

x.hist <- hist(x, plot = FALSE)
x.hist[["density"]] <- x.hist[["counts"]] / sum(x.hist[["counts"]])

x.boxplot.stats <- boxplot.stats(x)[["stats"]]

nf <- layout(matrix(c(2, 1), nrow = 2, ncol = 1, byrow = TRUE), height = c(1, 3))
par(mar = c(4.1, 4.1, 0, 2.1))
plot(x.hist, freq = FALSE, ylab = "Relative Frequency", xlim = range(pretty(x)), ylim = range(pretty(x.hist[["density"]])), main = NULL)
# box("plot")

par(mar = c(0, 4.1, 0, 2.1))
boxplot(x, frame = FALSE, axes = FALSE, horizontal = TRUE, ylim = range(pretty(x)), xaxt = "n", yaxt = "n")
# box("plot")
par(xpd = NA)
lines(x = x.boxplot.stats[2L] + c(0, 0),
      y = c(-2, 1.2),
      lwd = 3,
      col = "green3",
      lty = 2)

layout(1)
dev.off()

std.x <- std.y <- seq(from = 0, to = 1, by = 0.05)
points(x = std.x, y = std.y, pch = 19, col = "red")
conv.x <- grconvertX(std.x, to = "user")
conv.y <- grconvertY(std.y, to = "user")
points(x = conv.x, y = conv.y, pch = 19, col = "green3")

x <- rnorm(1000)
hist(x, xlim = c(-4,4))
usr <- par("usr")
clip(usr[1], -2, usr[3], usr[4])
hist(x, col = 'red', add = TRUE)
clip(2, usr[2], usr[3], usr[4])
hist(x, col = 'blue', add = TRUE)
do.call("clip", as.list(usr))  # reset to plot region

xinch

# # How the vertical axis for the horizontal boxplot is calculated
# n <- length(boxplot.stats(x)[["n"]]) # just 1 for us
# at <- 1L:n # gives 1
# ylim <- range(at, finite = TRUE) + c(-0.5, 0.5) # gives 0.5, 1.5
# ylim <- extendrange(ylim, f = 0.04) # gives 0.46, 1.54 == final answer
# # How to demonstrate
# boxplot(x, horizontal = TRUE)
# par("usr")[3L:4L] # these are y0 and y1, respectively
# abline(h = 1, col = "red", lty = "dotted")
# abline(h = 1.25, col = "red", lty = "dotted")
# abline(h = 1.5, col = "red", lty = "dotted")
# # OR
# boxplot(x, horizontal = TRUE, frame = FALSE)
# abline(h = par("usr")[3L:4L], lty = "dashed", col = "red", lwd = 3)
# abline(v = par("usr")[1L:2L], lty = "dashed", col = "red", lwd = 3)
# # The box width for this boxplot is from 0.8 to 1.2 on the vertical axis
# # By default, boxwex = 0.8. If width = NULL and varwidth = FALSE,
# # then width = 0.5 * boxwex. Therefore, by default, width = 0.4. In
# # the actual boxplot drawing function (bplt(), defined inside the
# # function bxp()), wid <- wid / 2, where wid = width when bplt() is
# # actually called later on. Then, the 4 corner points of the box are
# # determined based on whether (1) xlog = TRUE and (2) notch = TRUE.
# # Additionally, as seen above, if at = NULL (default), then at <- 1L:n.
# # For our case, xlog = FALSE and notch = FALSE so the y-coordinates
# # for our box are calculated as: at + wid * c(-1, 1, 1, -1). Since
# # width = 0.5 * 0.8 = 0.4 ==> wid = width / 2 = 0.4 / 2 = 0.2, then
# # at + wid * c(-1, 1, 1, -1) = 1 + 0.2 * c(-1, 1, 1, -1) =
# # c(0.8, 1.2, 1.2, 0.8). The x coordinates are taken from
# # stats = boxplot.stats(x)[["stats"]] as stats[c(2, 2, 4, 4)], i.e.,
# # c(lowerhinge, lowerhinge, upperhinge, upperhinge). These are then
# # passed to a polygon-drawing function, which produces the box.
# boxplot(x, horizontal = TRUE, frame = FALSE)
# polygon(y = c(0.8, 1.2, 1.2, 0.8), x = temp[["stats"]][c(2, 2, 4, 4)], border = "green", lwd = 3)




nf <- layout(matrix(c(2, 1), nrow = 2, ncol = 1, byrow = TRUE), height = c(1, 3))

par(mar = c(5.1, 4.1, 0, 2.1))
plot(x.hist, freq = FALSE, main = NULL, xlab = sprintf("Values of %s", sQuote(dataObjectName)), ylab = "Relative Frequency", xlim = range(pretty(x)), ylim = range(pretty(x.hist[["density"]])), col = col.fill.histogram, cex = cex, cex.axis = cex.axis)
if(plotDensityCurve) {
    lines(x.density[["x"]], x.density[["y"]], lty = lty.lines.density, lwd = lwd.lines.density, col = col.lines.density)
}
if(plotVerticalLines) {
    abline(v = x.verticalStats, lty = lty.lines.vertical, lwd = lwd.lines.vertical, col = col.lines.vertical)
}
if(plotStatsValues) {
    mtext(text = paste("Mean (SD)\n", formattedMeanSD, sep = ""), side = 1, line = line.statsLabel.bottom, at = min(x), col = col.statsLabel.bottom, font = font.statsLabel.bottom, cex = cex.statsLabel)
}

# title(main = sprintf("Plots for %s", sQuote(dataObjectName)), col.main = col.main, cex.main = cex.main, font.main = font.main, line = line.main)
par(mar = c(0, 4.1, 1.1, 2.1))
boxplot(x, frame = FALSE, axes = FALSE, horizontal = TRUE, ylim = range(pretty(x)), col = col.fill.boxplot, cex = cex, cex.axis = cex.axis)
if(plotVerticalLines) {
    segments(x0 = x.verticalStats, y0 = rep(0L, length.out = 4L), x1 = x.verticalStats, y1 = rep(1L, length.out = 4L), lty = lty.lines.vertical, lwd = lwd.lines.vertical, col = col.lines.vertical)
}
if(plotStatsValues) {
    oldXPD <- par()$xpd
    par(xpd = TRUE)
    mtext(text = paste("Q2", formattedQuartiles[1L], sep = "\n"), at = x.verticalStats[2L], side = 3, line = line.statsLabel.top, col = col.statsLabel.top, font = font.statsLabel.top, cex = cex.statsLabel)
    mtext(text = paste("Q1", formattedQuartiles[2L], sep = "\n"), at = x.verticalStats[3L], side = 3, line = line.statsLabel.top, col = col.statsLabel.top, font = font.statsLabel.top, adj = 1, cex = cex.statsLabel)
    mtext(text = paste("Q3", formattedQuartiles[3L], sep = "\n"), at = x.verticalStats[4L], side = 3, line = line.statsLabel.top, col = col.statsLabel.top, font = font.statsLabel.top, adj = 0, cex = cex.statsLabel)
    par(xpd = oldXPD)
}
layout(1)



## WORKING WITH THE **GRID** PACKAGE ##
require(grid)

# Fig. 1 in grid package vignette named "grid"
grid.show.viewport(viewport(x = 0.5, y = 0.5, width = 0.5, height = 0.25, angle = 45))

# Fig. 2 in grid package vignette named "grid"
grid.rect(gp <- gpar(lty = "dashed"))
vp1 <- viewport(x = 0, y = 0.5, w = 0.5, h = 0.5, just = c("left", "bottom"), name = "vp1")
vp2 <- viewport(x = 0.5, y = 0, w = 0.5, h = 0.5, just = c("left", "bottom"))
pushViewport(vp1)
grid.rect(gp = gpar(col = "grey"))
grid.text("Some drawing in graphics region 1", y = 0.8)
upViewport()
pushViewport(vp2)
grid.rect(gp = gpar(col = "grey"))
grid.text("Some drawing in graphics region 2", y = 0.8)
upViewport()
downViewport("vp1")
grid.text("MORE drawing in graphics region 1", y = 0.2)
popViewport()

# Fig. 3 in grid package vignette named "grid"
grid.rect(gp = gpar(lty = "dashed"))
vp <- viewport(width = 0.5, height = 0.5)
pushViewport(vp)
grid.rect(gp = gpar(col = "grey"))
grid.text("quarter of the page", y = 0.85)
pushViewport(vp)
grid.rect()
grid.text("quarter of the\nprevious viewport")
popViewport(2)


# set.seed(0804)
# x <- rnorm(100, sd = 100)
# x.hist <- hist(x, plot = FALSE)
# x.hist[["density"]] <- x.hist[["counts"]] / sum(x.hist[["counts"]])
# 
# vp.boxplot <- viewport(x = 0, y = 0.75, width = 1, height = 0.25, just = c("left", "bottom"))
# vp.histogram <- viewport(x = 0, y = 0, width = 1, height = 0.75)
# 
# pushViewport(vp.boxplot)
# grid.rect(gp = gpar(col = "grey"))
# grid.draw(boxplot(x, frame = FALSE, horizontal = TRUE, axes = FALSE, ylim = range(pretty(x))))


grid.rect(gp = gpar(lty = "dashed"))
x <- y <- 1:10
pushViewport(plotViewport(c(5.1, 4.1, 4.1, 2.1)))
pushViewport(dataViewport(x, y))
grid.rect()
grid.xaxis()
grid.yaxis()
grid.points(x, y)
grid.text("1:10", x = unit(-3, "lines"), rot = 90)
popViewport(2)


















