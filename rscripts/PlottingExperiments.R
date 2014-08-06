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



grid.rect(gp = gpar(lty = "dashed"))
vp.boxplot <- viewport(x = 0, y = 0.75, width = 1, height = 1, just = c("left", "bottom"))
vp.histogram <- viewport(x = 0, y = 0, width = 1, height = 0.75, just = c("left", "bottom"))
pushViewport(vp.boxplot)
grid.rect(gp = gpar(col = "grey"))
upViewport()
pushViewport(vp.histogram)
grid.rect(gp = gpar(col = "blue"))
pushViewport(plotViewport(c(5.1, 4.1, 0, 2.1)))
pushViewport(dataViewport(xData = x.hist[["mids"]], yData = x.hist[["density"]]))
grid.rect(gp = gpar(lty = "dotted"))
grid.xaxis(at = pretty(x.hist[["breaks"]]))
grid.yaxis(at = pretty(x.hist[["density"]]))


nB <- length(x.hist[["breaks"]])
rect(xleft = x.hist[["breaks"]][-nB], ybottom = 0, xright = x.hist[["breaks"]][-1L], ytop = x.hist[["density"]], col = NULL, border = par("fg"), lty = "solid")
histplot <- rectGrob(x = unit(x.hist[["breaks"]][-nB], "native"), y = unit(x.hist[["density"]], "native"), width = unit(diff(x.hist[["breaks"]]), "native"), height = unit(x.hist[["density"]], "native"), hjust = 0, name = "x.histogram")
grid.draw(gTree(name = "x.histogram"))
grid.get(gPath("x.histogram"))



set.seed(0805)
barData <- matrix(sample(1:4, 16, replace = TRUE), ncol = 4)
boxColors <- 1:4

bp <- function(barData) {
    nbars <- dim(barData)[2L]
    nmeasures <- dim(barData)[1L]
    barTotals <- rbind(rep(0, nbars), apply(barData, 2, cumsum))
    barYscale <- c(0, max(barTotals)*1.05)
    pushViewport(plotViewport(c(5, 4, 4, 1),
                              yscale = barYscale,
                              layout = grid.layout(1, nbars)))
    grid.rect()
    grid.yaxis()
    for (i in seq_len(nbars)) {
        pushViewport(viewport(layout.pos.col = i, yscale = barYscale))
        grid.rect(x = rep(0.5, nmeasures),
                  y = unit(barTotals[seq_len(nmeasures), i], "native"),
                  height = unit(diff(barTotals[, i]), "native"),
                  width = 0.8,
                  just = "bottom",
                  gp = gpar(fill = boxColors))
        popViewport()
    }
    popViewport()
}

legLabels <- c("Group A", "Group B", "Group C", "Something Longer")
boxSize <- unit(0.5, "inches")

leg <- function(legLabels) {
    nlabels <- length(legLabels)
    pushViewport(viewport(layout = grid.layout(nlabels, 1)))
    for (i in seq_len(nlabels)) {
        pushViewport(viewport(layout.pos.row = i))
        grid.rect(width = boxSize,
                  height = boxSize,
                  just = "bottom",
                  gp = gpar(fill = boxColors[i]))
        grid.text(legLabels[i], y = unit(0.5, "npc") - unit(1, "lines"))
        popViewport()
    }
    popViewport()
}

grid.rect(gp = gpar(lty = "dashed"))
legend.width <- max(unit(rep(1, length(legLabels)), "strwidth", as.list(legLabels)) + unit(2, "lines"),
                    unit(0.5, "inches") + unit(2, "lines"))
pushViewport(viewport(layout = grid.layout(1, 2, widths = unit.c(unit(1, "null"), legend.width))))
pushViewport(viewport(layout.pos.col = 1))
bp(barData)
popViewport()
pushViewport(viewport(layout.pos.col = 2))
pushViewport(plotViewport(c(5, 0, 4, 0)))
leg(legLabels)
popViewport(3)


# hg <- function(x) {
#     x.hist <- hist(x, plot = FALSE)
#     x.hist[["density"]] <- x.hist[["counts"]] / sum(x.hist[["counts"]])
#     nBars <- length(x.hist[["density"]])
#     # nMeasures <- 1L
#     # barHeights <- x.hist[["density"]]
#     # barYScale <- range(pretty(x.hist[["density"]])) + c(0, 0.1)
#     barYScale <- extendrange(range(pretty(x.hist[["density"]])), f = 0.04) + c(0, 0.1)
#     barXScale <- extendrange(range(pretty(x.hist[["breaks"]])), f = 0.04)
#     # pushViewport(plotViewport(c(5, 4, 0, 2),
#     #                           xscale = barXScale,
#     #                           yscale = barYScale,
#     #                           layout = grid.layout(1, nBars)))
#     pushViewport(plotViewport(c(5, 4, 0, 2),
#                               xscale = barXScale,
#                               yscale = barYScale))
#     grid.xaxis(at = pretty(range(x.hist[["breaks"]])))
#     grid.yaxis(at = pretty(range(x.hist[["density"]])))
#     grid.rect(x = unit(x.hist[["breaks"]][-length(x.hist[["breaks"]])], "native"),
#               y = unit(0, "native"),
#               height = unit(x.hist[["density"]], "native"),
#               width = unit(diff(x.hist[["breaks"]]), "native"),
#               just = c("left", "bottom"))
#     popViewport()
# }

plots <- function(x, boxHeight = 0.05) {
    x.hist <- hist(x, plot = FALSE)
    x.hist[["density"]] <- x.hist[["counts"]] / sum(x.hist[["counts"]])
    nBars <- length(x.hist[["density"]])
    barYScale <- extendrange(range(pretty(x.hist[["density"]])), f = 0.04) + c(0, 2 * boxHeight)
    barXScale <- extendrange(range(pretty(x.hist[["breaks"]])), f = 0.04)
    
    x.box <- boxplot.stats(x)
    boxYValue <- max(barYScale) - (1.1 * boxHeight)
    
    pushViewport(plotViewport(c(5, 4, 0, 2),
                              xscale = barXScale,
                              yscale = barYScale))
    grid.xaxis(at = pretty(range(x.hist[["breaks"]])))
    grid.yaxis(at = pretty(range(x.hist[["density"]])))
    grid.rect(x = unit(x.hist[["breaks"]][-length(x.hist[["breaks"]])], "native"),
              y = unit(0, "native"),
              height = unit(x.hist[["density"]], "native"),
              width = unit(diff(x.hist[["breaks"]]), "native"),
              just = c("left", "bottom"))
    
    grid.rect(x = unit(x.box[["stats"]][2L], "native"),
              y = unit(boxYValue - (boxHeight / 2), "native"),
              height = unit(boxHeight, "native"),
              width = unit(x.box[["stats"]][4L] - x.box[["stats"]][2L], "native"),
              just = c("left", "bottom"),
              gp = gpar(lty = "solid", lwd = 1))
    grid.segments(x0 = x.box[["stats"]][c(1L, 5L)],
                  y0 = c(0, 0) + boxYValue - (boxHeight / 2),
                  x1 = x.box[["stats"]][c(1L, 5L)],
                  y1 = c(0, 0) + boxYValue + (boxHeight / 2),
                  default.units = "native",
                  gp = gpar(lty = "solid", lwd = 1))
    grid.segments(x0 = x.box[["stats"]][3L],
                  y0 = boxYValue - (boxHeight / 2),
                  x1 = x.box[["stats"]][3L],
                  y1 = boxYValue + (boxHeight / 2),
                  default.units = "native",
                  gp = gpar(lty = "solid", lwd = 3))
    grid.segments(x0 = x.box[["stats"]][c(1L, 5L)],
                  y0 = c(0, 0) + boxYValue,
                  x1 = x.box[["stats"]][c(2L, 4L)],
                  y1 = c(0, 0) + boxYValue,
                  default.units = "native",
                  gp = gpar(lty = "dashed", lwd = 1))
    grid.points(x = x.box[["out"]],
                y = rep(boxYValue, length(x.box[["out"]])),
                pch = 1)
    popViewport()
}

set.seed(0805)
x <- rnorm(100, sd = 100)
# x.hist <- hist(x, plot = FALSE)
# x.hist[["density"]] <- x.hist[["counts"]] / sum(x.hist[["counts"]])
# hg(x)
# histvp <- viewport(x = 0,
#                    y = 0,
#                    width = 1,
#                    height = 0.75,
#                    just = c("left", "bottom"),
#                    xscale = range(pretty(x)),
#                    yscale = range(pretty(x.hist[["density"]])))
# x.boxplot.stats <- boxplot.stats(x)
plots(x)








def.par <- par(no.readonly = TRUE)
set.seed(0724)
x <- rnorm(500, sd = 100)
# x <- rnorm(500, sd = 0.01)

x.hist <- hist(x, plot = FALSE)
x.hist[["density"]] <- x.hist[["counts"]] / sum(x.hist[["counts"]])

x.boxplot.stats <- boxplot.stats(x)[["stats"]]

height.boxplot <- 1
height.histogram <- 3
verticalDeviceRatio.boxplot <- height.boxplot / (height.boxplot + height.histogram)
verticalDeviceRatio.histogram <- 1 - verticalDeviceRatio.boxplot

# nf <- layout(matrix(c(2, 1), nrow = 2, ncol = 1, byrow = TRUE), height = c(1, 3))
nf <- layout(matrix(c(2, 1), nrow = 2, ncol = 1, byrow = TRUE), height = c(height.boxplot, height.histogram))
par(mar = c(5.1, 4.1, 0, 2.1))
plot(x.hist, freq = FALSE, ylab = "Relative Frequency", xlim = range(pretty(x)), ylim = range(pretty(x.hist[["density"]])), main = NULL)
# box("plot")
par.hist <- par(no.readonly = TRUE)
# par.hist <- par()
min.hist <- grconvertY(par.hist$usr[3L], from = "user", to = "ndc")

par(mar = c(0, 4.1, 0, 2.1))
boxplot(x, frame = FALSE, axes = FALSE, horizontal = TRUE, ylim = range(pretty(x)), xaxt = "n", yaxt = "n")
# box("plot")
par.box <- par(no.readonly = TRUE)
# par.box <- par()
# max.box <- grconvertY(1.2, from = "user", to = "ndc")
par(xpd = NA)
clip(x1 = par("usr")[1L], x2 = par("usr")[2L], y1 = grconvertY(min.hist, from = "ndc", to = "user"), y2 = par("usr")[4L])
lines(x = x.boxplot.stats[2L] + c(0, 0), y = c(1.2, -10), lwd = 2, col = "green3", lty = "dashed")
# segments(x0 = x.boxplot.stats[2L], x1 = x.boxplot.stats[2L], y0 = grconvertY(min.hist, from = "ndc", to = "user"),y1 = grconvertY(max.box,from="ndc",to="user"),col="blue",lwd=2,lty="dashed")
# segments(x0 = x.boxplot.stats[2L], x1 = x.boxplot.stats[2L], y0 = grconvertY(min.hist, from = "ndc", to = "user"),y1 = -20, col = par("bg"), lwd = 2, lty = "solid")
# par(par.hist)
# par(fig = par.hist$fig)
# par(xpd = NA)
# lines(x = x.boxplot.stats[2L] + c(0, 0), y = c(par("usr")[3L], -10), lwd = 2, col = par("bg"), lty = "solid")


segments(x0=20,x1=20,y0=grconvertY(min.hist,from="ndc",to="user"),y1=grconvertY(max.box,from="ndc",to="user"),col="blue",lwd=2,lty="dashed")
lines(x = x.boxplot.stats[2L] + c(0, 0),
      y = c(-2, 1.2),
      lwd = 3,
      col = "green3",
      lty = 2)

layout(1)
dev.off()


xy.mean <- xy.coords(x = list(mean(x)), y = list(0))[c("x", "y")]
points(xy.mean, col = "blue", pch = 19, cex = 1.5)
par("fig")
par("plt")
# par("pin") / par("fin") == c(par("plt")[2L] - par("plt")[1L], par("plt")[4L] - par("plt")[3L])
zapsmall(c(par("pin") / par("fin") - c(par("plt")[2L] - par("plt")[1L], par("plt")[4L] - par("plt")[3L]), par("pin") / par("fin")))[1L:2L] == c(0L, 0L)

layout1.fig <- par("fig")
par(fig = c(0, 1, 0.75, 1))

# # For the histogram, the bottom of the line (on the actual axis,
# # not the bottoms of the bars) should be at y = par("mai")[1L] inches.
# lines(x = c(10,10), y = grconvertY(par("mai")[1L], from="in", to="user") + c(0, 0.2), lwd = 2, col = "green3")

lines(x = c(20,20), y = par("usr")[3L:4L], lwd = 2, col = "red")
lines(x = c(20,20), y = par("usr")[3L:4L], lwd = 2, col = "red", lty="dashed")



lines(x = c(10, 10), y = par("usr")[3L:4L], lwd = 2, col = "red")
lines(x = c(20, 20), y = c(par("usr")[3L],1.2), lwd = 2, col = "blue", lty = "dashed")
lines(x = c(20, 20), y = c(1.2, par("usr")[3L]), lwd = 2, col = "blue", lty = "dashed")

par(xpd = NA)
lines(x = c(30, 30), y = c(1.2, -10), lwd = 2, col = "blue", lty = "dashed")
lines()

linelength <- (diff(par.hist$plt[3L:4L])*verticalDeviceRatio.histogram) + ((1.2 - par.box$usr[3L])*verticalDeviceRatio.boxplot)






# Solution: Plot a histogram with extra space above it, then draw your
# own boxplot in pieces

def.par <- par(no.readonly = TRUE)
set.seed(0724)
x <- rnorm(500, sd = 100)
x <- rnorm(500, sd = 0.01)

x.hist <- hist(x, plot = FALSE)
x.hist[["density"]] <- x.hist[["counts"]] / sum(x.hist[["counts"]])
yAxisTicks <- pretty(range(x.hist[["density"]]))
yAxisTicksStep <- max(abs(diff(yAxisTicks)))

x.boxplot.stats <- boxplot.stats(x)

plot(x.hist,
     freq = FALSE,
     axes = FALSE,
     xlim = range(pretty(range(x))),
#      ylim = range(pretty(range(x.hist[["density"]]))) + c(0, min(0.05, 0.75*abs(diff(pretty(range(x.hist[["density"]])))))),
#      ylim = range(yAxisTicks) + c(0, min(0.05, 0.75*yAxisTicksStep)),
     ylim = range(yAxisTicks) + c(0, 0.75*yAxisTicksStep),
     xlab = paste("Values of ", sQuote("x"), sep = ""),
     ylab = "Relative Frequency",
     main = NULL)
# axis(side = 1,
#      at = pretty(range(x)))
# axis(side = 1,
#      at = pretty(range(x)),
#      pos = par("usr")[3L])
axis(side = 1,
     at = pretty(range(x)),
     pos = extendrange(range(yAxisTicks), f = 0.04))
axis(side = 2,
     at = yAxisTicks)
par(xpd = NA)
# boxHeight <- 0.8 * max(abs(diff(pretty(range(x.hist[["density"]])))))
boxHeight <- 0.8 * yAxisTicksStep
# boxYValue <- max(range(pretty(x.hist[["density"]]))) + (boxHeight)
# boxYValue <- par("usr")[4L]
# boxYValue <- max(x.hist[["density"]]) + max(abs(diff(pretty(range(x.hist[["density"]])))))
# boxYValue <- max(pretty(range(x.hist[["density"]]))) + max(abs(diff(pretty(range(x.hist[["density"]])))))
# boxYValue <- max(yAxisTicks) + yAxisTicksStep
boxYValue <- max(yAxisTicks) + boxHeight
rect(xleft = x.boxplot.stats[["stats"]][2L],
     ybottom = boxYValue - (boxHeight / 2),
     xright = x.boxplot.stats[["stats"]][4L],
     ytop = boxYValue + (boxHeight / 2),
     col = NA,
     border = par("fg"),
     lty = "solid",
     lwd = 1)
segments(x0 = x.boxplot.stats[["stats"]][c(1L, 3L, 5L)],
         y0 = boxYValue - (boxHeight / 2),
         x1 = x.boxplot.stats[["stats"]][c(1L, 3L, 5L)],
         y1 = boxYValue + (boxHeight / 2),
         col = par("fg"),
         lty = "solid",
         lwd = c(1, 3, 1))
segments(x0 = x.boxplot.stats[["stats"]][c(1L, 5L)],
         y0 = boxYValue,
         x1 = x.boxplot.stats[["stats"]][c(2L, 4L)],
         y1 = boxYValue,
         col = par("fg"),
         lty = "dashed",
         lwd = 1)
points(x = x.boxplot.stats[["out"]],
       y = rep(boxYValue, times = length(x.boxplot.stats[["out"]])))

# mtext(text = paste("Mean (SD)\n", signif(mean(x), digits = 2L), " (", signif(sd(x), digits = 2L), ")", sep = ""), side = 1, line = 3, at = min(x))
mtext(text = paste("Mean (SD)\n", signif(mean(x), digits = 2L), " (", signif(sd(x), digits = 2L), ")", sep = ""), side = 1, line = 3, at = min(pretty(range(x))))
# mtext(text = c(paste("Q1\n", signif(x.boxplot.stats[["stats"]][2L], digits = 2L), sep = ""), paste("Q2\n", signif(x.boxplot.stats[["stats"]][3L], digits = 2L), sep = ""), paste("Q3\n", signif(x.boxplot.stats[["stats"]][4L], digits = 2L), sep = "")),
#       side = 3,
#       line = 0,
#       adj = c(1, 0.5, 0))
mtext(text = paste("Q1\n", signif(x.boxplot.stats[["stats"]][2L], digits = 2L), sep = ""),
      side = 3,
      at = x.boxplot.stats[["stats"]][2L],
      line = 1.5,
      adj = 1)
mtext(text = paste("Q2\n", signif(x.boxplot.stats[["stats"]][3L], digits = 2L), sep = ""),
      side = 3,
      at = x.boxplot.stats[["stats"]][3L],
      line = 1.5)
mtext(text = paste("Q3\n", signif(x.boxplot.stats[["stats"]][4L], digits = 2L), sep = ""),
      side = 3,
      at = x.boxplot.stats[["stats"]][4L],
      line = 1.5,
      adj = 0)

# segments(x0 = x.boxplot.stats[["stats"]][2L:4L],
#          y0 = boxYValue + (boxHeight / 2),
#          x1 = x.boxplot.stats[["stats"]][2L:4L],
#          y1 = min(extendrange(range(yAxisTicks), f = 0.04)),
#          col = "green3",
#          lty = "solid",
#          lwd = 2)
clip(x1 = min(pretty(range(x))),
     x2 = max(pretty(range(x))),
     y1 = extendrange(range(yAxisTicks), f = 0.04)[1L],
     y2 = boxYValue + (boxHeight / 2))
abline(v = c(mean(x), x.boxplot.stats[["stats"]][2L:4L]),
         col = "green3",
         lty = c("solid", "dashed", "dashed", "dashed"),
         lwd = 3)









