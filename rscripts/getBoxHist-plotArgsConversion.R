# # This is intended to be a temporary sandbox file as I move from
# # explicitly specifying every plotting option to utilizing both the
# # '...' and 'pars' parameter (see: args(bxp)).
# 
# # Define the getBoxHist() function
# # getBoxHist <- function(x, na.rm = TRUE, dataObjectName = NULL, digits = 2L, plotDensityCurve = TRUE, plotVerticalLines = TRUE, plotStatsValues = TRUE, col.fill = NULL, col.fill.boxplot = col.fill, col.fill.histogram = col.fill, lty.lines = c("solid", "solid", "dashed", "dashed", "dashed"), lwd.lines = 2L, col.lines = c("red", gray(0.7)), col.lines.density = NULL, col.lines.vertical = NULL, lty.lines.density = NULL, lty.lines.vertical = NULL, lwd.lines.density = NULL, lwd.lines.vertical = NULL, line.statsLabel.top = -1.5, line.statsLabel.bottom = 3L, col.statsLabel = col.lines.vertical, col.statsLabel.top = col.statsLabel, col.statsLabel.bottom = col.statsLabel, font.statsLabel = 1L, font.statsLabel.top = font.statsLabel, font.statsLabel.bottom = font.statsLabel, cex = par("cex"), cex.axis = par("cex.axis"), cex.statsLabel = cex, cex.statsLabel.top = cex.statsLabel, cex.statsLabel.bottom = cex.statsLabel, bg = par("bg"), fg = par("fg")) {
# getBoxHist <- function(x, na.rm = TRUE, dataObjectName = NULL, digits = 2L, plotDensityCurve = TRUE, plotVerticalLines = TRUE, plotStatsValues = TRUE, line.statsLabel.top = -1.5, line.statsLabel.bottom = 3L, col.fill = NULL, col.fill.boxplot = col.fill, col.fill.histogram = col.fill, lty.lines = c("solid", "solid", "dashed", "dashed", "dashed"), lwd.lines = 2L, col.lines = c("red", gray(0.7)), col.lines.density = NULL, col.lines.vertical = NULL, lty.lines.density = NULL, lty.lines.vertical = NULL, lwd.lines.density = NULL, lwd.lines.vertical = NULL, col.statsLabel = col.lines.vertical, col.statsLabel.top = col.statsLabel, col.statsLabel.bottom = col.statsLabel, font.statsLabel = 1L, font.statsLabel.top = font.statsLabel, font.statsLabel.bottom = font.statsLabel, cex = par("cex"), cex.axis = par("cex.axis"), cex.statsLabel = cex, cex.statsLabel.top = cex.statsLabel, cex.statsLabel.bottom = cex.statsLabel, bg = par("bg"), fg = par("fg")) {
#     def.par <- par(no.readonly = TRUE)
#     on.exit(layout(1), add = TRUE)
#     on.exit(par(def.par), add = TRUE)
#     
#     defaultIfNULL <- function(option, default) {
#         if(is.null(option) || length(option) == 0L) {
#             default
#         } else {
#             option
#         }
#     }
#     
#     if(length(dataObjectName) < 1L) {
#         dataObjectName <- deparse(substitute(x))
#     }
#     
#     col.fill.histogram <- defaultIfNULL(col.fill.histogram, col.fill)
#     col.fill.boxplot <- defaultIfNULL(col.fill.boxplot, col.fill)
#     
#     x.boxplot.stats <- boxplot.stats(x)[["stats"]]
#     # Retrieved 2014-07-22: http://stackoverflow.com/a/9122859
#     x.hist <- hist(x, plot = FALSE)
#     x.hist[["density"]] <- x.hist[["counts"]] / sum(x.hist[["counts"]])
#     if(plotDensityCurve) {
#         x.density <- density(x)
#         x.density[["y"]] <- x.density[["y"]] * diff(x.hist[["breaks"]])[1L]
#         lty.lines.density <- defaultIfNULL(lty.lines.density, defaultIfNULL(lty.lines[1L], par("lty")))
#         lwd.lines.density <- defaultIfNULL(lwd.lines.density, defaultIfNULL(lwd.lines[1L], par("lwd")))
#         col.lines.density <- defaultIfNULL(col.lines.density, defaultIfNULL(col.lines[1L], par("col")))
#     }
#     if(plotVerticalLines || plotStatsValues) {
#         x.mean <- mean(x, na.rm = na.rm)
#         x.sd <- sd(x, na.rm = na.rm)
#         x.whisker.minimum <- x.boxplot.stats[1L]
#         x.lowerhinge <- x.boxplot.stats[2L]
#         x.median <- x.boxplot.stats[3L]
#         x.upperhinge <- x.boxplot.stats[4L]
#         x.whisker.maximum <- x.boxplot.stats[5L]
#         x.verticalStats <- c(x.mean, x.median, x.lowerhinge, x.upperhinge)
#         # formattedMean <- formatC(x.mean, format = "f", digits = digits)
#         # formattedSD <- formatC(x.sd, format = "f", digits = digits)
#         formattedMean <- signif(x.mean, digits = digits)
#         formattedSD <- signif(x.sd, digits = digits)
#         formattedMeanSD <- paste(formattedMean, " (", formattedSD, ")", sep = "")
#         # formattedQuartiles <- formatC(c(x.median, x.lowerhinge, x.upperhinge), format = "f", digits = digits)
#         formattedQuartiles <- signif(c(x.median, x.lowerhinge, x.upperhinge), digits = digits)
#         lty.lines.vertical <- defaultIfNULL(lty.lines.vertical, defaultIfNULL(lty.lines[-1L], defaultIfNULL(lty.lines, par("lty"))))
#         lwd.lines.vertical <- defaultIfNULL(lwd.lines.vertical, defaultIfNULL(lwd.lines[-1L], defaultIfNULL(lwd.lines, par("lwd"))))
#         col.lines.vertical <- defaultIfNULL(col.lines.vertical, defaultIfNULL(col.lines[-1L], defaultIfNULL(col.lines, par("col"))))
#         col.statsLabel.top <- defaultIfNULL(col.statsLabel.top, defaultIfNULL(col.statsLabel, col.lines.vertical))
#         col.statsLabel.bottom <- defaultIfNULL(col.statsLabel.bottom, defaultIfNULL(col.statsLabel, col.lines.vertical))
#     }
#     
#     nf <- layout(matrix(c(2, 1), nrow = 2, ncol = 1, byrow = TRUE), height = c(1, 3))
#     
#     par(mar = c(5.1, 4.1, 0, 2.1))
#     par(bg = bg)
#     par(fg = fg)
#     plot(x.hist, freq = FALSE, main = NULL, xlab = sprintf("Values of %s", sQuote(dataObjectName)), ylab = "Relative Frequency", xlim = range(pretty(x)), ylim = range(pretty(x.hist[["density"]])), col = col.fill.histogram, cex = cex, cex.axis = cex.axis)
#     if(plotDensityCurve) {
#         lines(x.density[["x"]], x.density[["y"]], lty = lty.lines.density, lwd = lwd.lines.density, col = col.lines.density)
#     }
#     if(plotVerticalLines) {
#         abline(v = x.verticalStats, lty = lty.lines.vertical, lwd = lwd.lines.vertical, col = col.lines.vertical)
#     }
#     if(plotStatsValues) {
#         mtext(text = paste("Mean (SD)\n", formattedMeanSD, sep = ""), side = 1, line = line.statsLabel.bottom, at = min(x), col = col.statsLabel.bottom, font = font.statsLabel.bottom, cex = cex.statsLabel)
#     }
# 
#     # title(main = sprintf("Plots for %s", sQuote(dataObjectName)), col.main = col.main, cex.main = cex.main, font.main = font.main, line = line.main)
#     par(mar = c(0, 4.1, 1.1, 2.1))
#     boxplot(x, frame = FALSE, axes = FALSE, horizontal = TRUE, ylim = range(pretty(x)), col = col.fill.boxplot, cex = cex, cex.axis = cex.axis)
#     if(plotVerticalLines) {
#         segments(x0 = x.verticalStats, y0 = rep(0L, length.out = 4L), x1 = x.verticalStats, y1 = rep(1L, length.out = 4L), lty = lty.lines.vertical, lwd = lwd.lines.vertical, col = col.lines.vertical)
#     }
#     if(plotStatsValues) {
#         oldXPD <- par()$xpd
#         par(xpd = TRUE)
#         mtext(text = paste("Q2", formattedQuartiles[1L], sep = "\n"), at = x.verticalStats[2L], side = 3, line = line.statsLabel.top, col = col.statsLabel.top, font = font.statsLabel.top, cex = cex.statsLabel)
#         mtext(text = paste("Q1", formattedQuartiles[2L], sep = "\n"), at = x.verticalStats[3L], side = 3, line = line.statsLabel.top, col = col.statsLabel.top, font = font.statsLabel.top, adj = 1, cex = cex.statsLabel)
#         mtext(text = paste("Q3", formattedQuartiles[3L], sep = "\n"), at = x.verticalStats[4L], side = 3, line = line.statsLabel.top, col = col.statsLabel.top, font = font.statsLabel.top, adj = 0, cex = cex.statsLabel)
#         par(xpd = oldXPD)
#     }
#     layout(1)
# }
# 
# 
# 
# 
# 
# x, na.rm = TRUE, dataObjectName = NULL, digits = 2L, plotDensityCurve = TRUE, plotVerticalLines = TRUE, plotStatsValues = TRUE, 
# cat(gsub(pattern = ", ", replacement = "\n", x = 'col.fill = NULL, col.fill.boxplot = col.fill, col.fill.histogram = col.fill, lty.lines = c("solid", "solid", "dashed", "dashed", "dashed"), lwd.lines = 2L, col.lines = c("red", gray(0.7)), col.lines.density = NULL, col.lines.vertical = NULL, lty.lines.density = NULL, lty.lines.vertical = NULL, lwd.lines.density = NULL, lwd.lines.vertical = NULL, line.statsLabel.top = -1.5, line.statsLabel.bottom = 3L, col.statsLabel = col.lines.vertical, col.statsLabel.top = col.statsLabel, col.statsLabel.bottom = col.statsLabel, font.statsLabel = 1L, font.statsLabel.top = font.statsLabel, font.statsLabel.bottom = font.statsLabel, cex = par("cex"), cex.axis = par("cex.axis"), cex.statsLabel = cex, cex.statsLabel.top = cex.statsLabel, cex.statsLabel.bottom = cex.statsLabel, bg = par("bg"), fg = par("fg")'), sep = "")
# 
# # We will need to isolate which options go to:
# # -- plot.histogram
# # -- bxp
# # -- abline
# # -- segments
# # -- mtext
# # -- lines
# 
# 
# 
# ## ALTERED PARAMETERS ##
# # col
# # - *
# # - fill
# # - - *
# # - - boxplot
# # - - histogram
# # - lines
# # - - *
# # - - density
# # - - vertical
# # - statsLabel
# # - - *
# # - - top
# # - - bottom
# 
# # lty
# # - *
# # - lines
# # - - *
# # - - density
# # - - vertical
# 
# # lwd
# # - *
# # - lines
# # - - *
# # - - density
# # - - vertical
# 
# # cex
# # - *
# # - axis
# # - statsLabel
# # - - *
# # - - top
# # - - bottom
# 
# # font
# # - statsLabel
# # - - *
# # - - top
# # - - bottom
# 
# 
# 
# 
# # Copied directly from 'bxp'
# pars <- c(list(...), pars)
# pars <- pars[unique(names(pars))]
# pcycle <- function(p, def1, def2 = NULL) {
#     rep(if (length(p)) {
#             p
#         } else if (length(def1)) {
#             def1
#         } else {
#             def2
#         }, length.out = n)
# p <- function(sym) pars[[sym, exact = TRUE]]
# ax.pars <- pars[names(pars) %in% c("xaxt", "yaxt", "xaxp", "yaxp", "las", "cex.axis", "col.axis", "format")]
# do.call("title", pars[names(pars) %in% c("main", "cex.main", "col.main", "sub", "cex.sub", "col.sub", "xlab", "ylab", "cex.lab", "col.lab")])
# 
# boxlty <- pcycle(pars$boxlty, p("lty"), par("lty"))
# boxlwd <- pcycle(pars$boxlwd, p("lwd"), par("lwd"))
# boxcol <- pcycle(pars$boxcol, border)
# boxfill <- pcycle(pars$boxfill, par("bg"))
# boxwex <- pcycle(pars$boxwex, 0.8 * {
#     if (n <= 1) 
#         1
#     else stats::quantile(diff(sort(if (xlog) 
#         log(at)
#     else at)), 0.1)
# })
# medlty <- pcycle(pars$medlty, p("lty"), par("lty"))
# medlwd <- pcycle(pars$medlwd, 3 * p("lwd"), 3 * par("lwd"))
# medpch <- pcycle(pars$medpch, NA_integer_)
# medcex <- pcycle(pars$medcex, p("cex"), par("cex"))
# medcol <- pcycle(pars$medcol, border)
# medbg <- pcycle(pars$medbg, p("bg"), par("bg"))
# whisklty <- pcycle(pars$whisklty, p("lty"), "dashed")
# whisklwd <- pcycle(pars$whisklwd, p("lwd"), par("lwd"))
# whiskcol <- pcycle(pars$whiskcol, border)
# staplelty <- pcycle(pars$staplelty, p("lty"), par("lty"))
# staplelwd <- pcycle(pars$staplelwd, p("lwd"), par("lwd"))
# staplecol <- pcycle(pars$staplecol, border)
# staplewex <- pcycle(pars$staplewex, 0.5)
# outlty <- pcycle(pars$outlty, "blank")
# outlwd <- pcycle(pars$outlwd, p("lwd"), par("lwd"))
# outpch <- pcycle(pars$outpch, p("pch"), par("pch"))
# outcex <- pcycle(pars$outcex, p("cex"), par("cex"))
# outcol <- pcycle(pars$outcol, border)
# outbg <- pcycle(pars$outbg, p("bg"), par("bg"))
# outwex <- pcycle(pars$outwex, 0.5)
# 
# 
# set.seed(0801)
# x <- rnorm(100, sd = 100)
# # x <- rnorm(100, sd = 0.01)
# 
# 
# # getBoxHist <- function(x, na.rm = TRUE, dataObjectName = NULL, digits = 2L, plotDensityCurve = TRUE, plotVerticalLines = TRUE, plotStatsValues = TRUE, line.statsLabel.top = -1.5, line.statsLabel.bottom = 3L, col.fill = NULL, col.fill.boxplot = col.fill, col.fill.histogram = col.fill, lty.lines = c("solid", "solid", "dashed", "dashed", "dashed"), lwd.lines = 2L, col.lines = c("red", gray(0.7)), col.lines.density = NULL, col.lines.vertical = NULL, lty.lines.density = NULL, lty.lines.vertical = NULL, lwd.lines.density = NULL, lwd.lines.vertical = NULL, col.statsLabel = col.lines.vertical, col.statsLabel.top = col.statsLabel, col.statsLabel.bottom = col.statsLabel, font.statsLabel = 1L, font.statsLabel.top = font.statsLabel, font.statsLabel.bottom = font.statsLabel, cex = par("cex"), cex.axis = par("cex.axis"), cex.statsLabel = cex, cex.statsLabel.top = cex.statsLabel, cex.statsLabel.bottom = cex.statsLabel, bg = par("bg"), fg = par("fg")) {
# getBoxHist <- function(x, na.rm = TRUE, dataObjectName = NULL, digits = 2L, plotDensityCurve = TRUE, plotVerticalLines = TRUE, plotStatsValues = TRUE, pars = list(line.statsValues.top = -1.5, line.statsValues.bottom = 3L), ...) {
#     def.par <- par(no.readonly = TRUE)
#     on.exit(layout(1), add = TRUE)
#     on.exit(par(def.par), add = TRUE)
#     
#     pars <- c(list(...), pars)
#     pars <- uniquePars <- pars[unique(names(pars))]
#     
#     # Currently not accepting any titles for any plots
#     if (any(grepl(pattern = "main$", x = names(pars)))) {
#         pars[grep(pattern = "main$", x = names(pars))] <- NULL
#     }
#     
# #     defaultIfNULL <- function(option, default) {
# #         if(is.null(option) || length(option) == 0L) {
# #             default
# #         } else {
# #             option
# #         }
# #     }
# #     pcycle <- function(p, def1, def2 = NULL) rep(if (length(p)) p else if (length(def1)) def1 else def2, length.out = 1L)
#     pcycle <- function(p, def1, def2 = NULL) rep(if (length(p)) p else if (length(def1)) def1 else def2, length.out = max(length(p), length(def1), length(def2)))
#     p <- function(sym) pars[[sym, exact = TRUE]]
#     
#     if(length(dataObjectName) < 1L) {
#         dataObjectName <- deparse(substitute(x))
#     }
#     
#     line.statsValues.top <- pcycle(pars[["line.statsValues.top"]], p("line.statsValues")[1L], -1.5)
#     line.statsValues.bottom <- pcycle(pars[["line.statsValues.bottom"]], if (length(p("line.statsValues"))) p("line.statsValues")[2L] else p("line.statsValues"), 3L)
#     
# #     col.fill.histogram <- defaultIfNULL(col.fill.histogram, col.fill)
# #     col.fill.boxplot <- defaultIfNULL(col.fill.boxplot, col.fill)
#     col.fill <- pcycle(pars[["col.fill"]], p("fill"))
#     col.fill.histogram <- pcycle(pars[["col.fill.histogram"]], col.fill)
#     col.fill.boxplot <- pcycle(pars[["col.fill.boxplot"]], col.fill)
#     
# #     if (length(pars[["col.fill"]] < 1L)) {
# #         if (length(p("fill")) < 1L) {
# #             col.fill <- NULL
# #         } else {
# #             col.fill <- p("fill")
# #         }
# #     } else {
# #         col.fill <- pars[["col.fill"]]
# #     }
#     
#     col.lines <- pcycle(pars[["col.lines"]], p("col"), par("col"))
#     lty.lines <- pcycle(pars[["lty.lines"]], p("lty"), par("lty"))
#     lwd.lines <- pcycle(pars[["lwd.lines"]], p("lwd"), par("lwd"))
#     
#     x.boxplot.stats <- boxplot.stats(x)[["stats"]]
#     # Retrieved 2014-07-22: http://stackoverflow.com/a/9122859
# #     x.hist <- hist(x, plot = FALSE)
#     args.histogram <- pars[c("breaks", "include.lowest", "right", "nclass")]
#     x.hist <- do.call(what = "hist",
#                       args = c(list(x = x,
#                                     plot = FALSE,
#                                     warn.unused = FALSE),
#                                args.histogram))
#     x.hist[["density"]] <- x.hist[["counts"]] / sum(x.hist[["counts"]])
#     xlim <- pcycle(pars[["xlim"]], p("xlim"), range(pretty(x)))
# #     xlim <- pcycle(pars[["xlim"]], range(pretty(x)), p("xlim"))
#     xlab <- pcycle(pars[["xlab"]], p("xlab.hist"), sprintf("Values of %s", sQuote(dataObjectName)))
#     ylim.histogram <- pcycle(pars[["ylim.histogram"]], range(pretty(x.hist[["density"]])))
#     ylim.boxplot   <- pcycle(pars[["ylim.boxplot"]], extendrange(range(1), f = 0.04))
#     ylab <- pcycle(pars[["ylab"]], p("ylab.hist"), "Relative Frequency")
#     if(plotDensityCurve) {
#         x.density <- density(x)
#         x.density[["y"]] <- x.density[["y"]] * diff(x.hist[["breaks"]])[1L]
# #         lty.lines.density <- defaultIfNULL(lty.lines.density, defaultIfNULL(lty.lines[1L], par("lty")))
# #         lwd.lines.density <- defaultIfNULL(lwd.lines.density, defaultIfNULL(lwd.lines[1L], par("lwd")))
# #         col.lines.density <- defaultIfNULL(col.lines.density, defaultIfNULL(col.lines[1L], par("col")))
#         lty.lines.density <- pcycle(pars[["lty.lines.density"]], lty.lines[1L], par("lty"))
#         lwd.lines.density <- pcycle(pars[["lwd.lines.density"]], lwd.lines[1L], par("lwd"))
#         col.lines.density <- pcycle(pars[["col.lines.density"]], col.lines[1L], par("col"))
#     }
#     if(plotVerticalLines || plotStatsValues) {
#         x.mean <- mean(x, na.rm = na.rm)
#         x.sd <- sd(x, na.rm = na.rm)
#         x.whisker.minimum <- x.boxplot.stats[1L]
#         x.lowerhinge <- x.boxplot.stats[2L]
#         x.median <- x.boxplot.stats[3L]
#         x.upperhinge <- x.boxplot.stats[4L]
#         x.whisker.maximum <- x.boxplot.stats[5L]
#         x.verticalStats <- c(x.mean, x.median, x.lowerhinge, x.upperhinge)
#         # formattedMean <- formatC(x.mean, format = "f", digits = digits)
#         # formattedSD <- formatC(x.sd, format = "f", digits = digits)
#         formattedMean <- signif(x.mean, digits = digits)
#         formattedSD <- signif(x.sd, digits = digits)
#         formattedMeanSD <- paste(formattedMean, " (", formattedSD, ")", sep = "")
#         # formattedQuartiles <- formatC(c(x.median, x.lowerhinge, x.upperhinge), format = "f", digits = digits)
#         # formattedQuartiles <- signif(c(x.median, x.lowerhinge, x.upperhinge), digits = digits)
#         formattedQ1 <- paste(signif(x.lowerhinge, digits = digits), sep = "")
#         formattedQ2 <- paste(signif(x.median, digits = digits), sep = "")
#         formattedQ3 <- paste(signif(x.upperhinge, digits = digits), sep = "")
#         formattedQuartiles <- c(formattedQ1, formattedQ2, formattedQ3)
# #         lty.lines.vertical <- defaultIfNULL(lty.lines.vertical, defaultIfNULL(lty.lines[-1L], defaultIfNULL(lty.lines, par("lty"))))
# #         lwd.lines.vertical <- defaultIfNULL(lwd.lines.vertical, defaultIfNULL(lwd.lines[-1L], defaultIfNULL(lwd.lines, par("lwd"))))
# #         col.lines.vertical <- defaultIfNULL(col.lines.vertical, defaultIfNULL(col.lines[-1L], defaultIfNULL(col.lines, par("col"))))
# #         col.statsLabel.top <- defaultIfNULL(col.statsLabel.top, defaultIfNULL(col.statsLabel, col.lines.vertical))
# #         col.statsLabel.bottom <- defaultIfNULL(col.statsLabel.bottom, defaultIfNULL(col.statsLabel, col.lines.vertical))
#         lty.lines.vertical <- pcycle(pars[["lty.lines.vertical"]], if (length(lty.lines) > 1L) lty.lines[-1L] else lty.lines, par("lty"))
#         lwd.lines.vertical <- pcycle(pars[["lwd.lines.vertical"]], if (length(lwd.lines) > 1L) lwd.lines[-1L] else lwd.lines, par("lwd"))
#         col.lines.vertical <- pcycle(pars[["col.lines.vertical"]], if (length(col.lines) > 1L) col.lines[-1L] else col.lines, par("col"))
#         col.statsValues <- pcycle(pars[["col.statsValues"]], col.lines.vertical, par("col"))
#         col.statsValues.top <- pcycle(pars[["col.statsValues.top"]], col.statsValues, col.lines.vertical)
#         col.statsValues.bottom <- pcycle(pars[["col.statsValues.bottom"]], col.statsValues, col.lines.vertical)
#         
#         cex.statsValues <- pcycle(pars[["cex.statsValues"]], p("cex"), par("cex"))
#         cex.statsValues.top <- pcycle(pars[["cex.statsValues.top"]], cex.statsValues, 1)
#         cex.statsValues.bottom <- pcycle(pars[["cex.statsValues.bottom"]], cex.statsValues, 1)
#         font.statsValues <- pcycle(pars[["font.statsValues"]], p("font"), par("font"))
#         font.statsValues.top <- pcycle(pars[["font.statsValues.top"]], if (length(font.statsValues) > 1L) font.statsValues[1L] else font.statsValues, 1)
#         font.statsValues.bottom <- pcycle(pars[["font.statsValues.bottom"]], if (length(font.statsValues) > 1L) font.statsValues[-1L] else font.statsValues, 1)
#     }
#     
# #     args.histogram <- pars[names(pars) %in% c("cex", "cex.axis", "cex.lab", "xaxt", "yaxt", "xaxp", "yaxp", "las", "col.axis", "col.lab", "format", "font", "font.lab", "font.axis")]
# #     args.histogram <- list()
#     args.plot.histogram <- pars[c("border", "angle", "density", "axes", "labels", "add", "ann", "col.axis", "cex", "cex.axis", "cex.lab", "col.lab", "font.lab", "font.axis")]
#     
#     nf <- layout(matrix(c(2, 1), nrow = 2, ncol = 1, byrow = TRUE), height = c(1, 3))
#     
# #     par(mar = c(5.1, 4.1, 0, 2.1))
# #     par(bg = bg)
# #     par(fg = fg)
#     bg <- pcycle(pars[["bg"]], par("bg"))
#     fg <- pcycle(pars[["fg"]], par("fg"))
#     par(mar = c(5.1, 4.1, 0, 2.1), bg = bg, fg = fg)
# #     plot(x.hist, freq = FALSE, main = NULL, xlab = sprintf("Values of %s", sQuote(dataObjectName)), ylab = "Relative Frequency", xlim = range(pretty(x)), ylim = range(pretty(x.hist[["density"]])), col = col.fill.histogram, cex = cex, cex.axis = cex.axis)
#     do.call(what = "plot",
#             args = c(list(x = x.hist,
#                           freq = FALSE,
#                           col = col.fill.histogram,
#                           main = NULL,
#                           xlim = xlim,
#                           ylim = ylim.histogram,
#                           xlab = xlab,
#                           ylab = ylab),
#                      args.plot.histogram))
#     if(plotDensityCurve) {
# #         lines(x.density[["x"]], x.density[["y"]], lty = lty.lines.density, lwd = lwd.lines.density, col = col.lines.density)
#         do.call(what = "lines",
#                 args = list(x = x.density[["x"]],
#                             y = x.density[["y"]],
#                             lty = lty.lines.density,
#                             lwd = lwd.lines.density,
#                             col = col.lines.density))
#     }
#     if(plotVerticalLines) {
# #         abline(v = x.verticalStats, lty = lty.lines.vertical, lwd = lwd.lines.vertical, col = col.lines.vertical)
#         do.call(what = "abline",
#                 args = list(v = x.verticalStats,
#                             lty = lty.lines.vertical,
#                             lwd = lwd.lines.vertical,
#                             col = col.lines.vertical))
#     }
#     if(plotStatsValues) {
# #         mtext(text = paste("Mean (SD)\n", formattedMeanSD, sep = ""), side = 1, line = line.statsValues.bottom, at = min(x), col = col.statsValues.bottom, font = font.statsValues.bottom, cex = cex.statsValues)
#         do.call(what = "mtext",
#                 args = list(text = paste("Mean (SD)\n", formattedMeanSD, sep = ""),
#                             side = 1,
#                             line = line.statsValues.bottom,
#                             at = min(xlim),
#                             col = col.statsValues.bottom,
#                             font = font.statsValues.bottom,
#                             cex = cex.statsValues.bottom))
#     }
# 
#     # title(main = sprintf("Plots for %s", sQuote(dataObjectName)), col.main = col.main, cex.main = cex.main, font.main = font.main, line = line.main)
#     par(mar = c(0, 4.1, 1.1, 2.1))
#     args.plot.boxplot <- pars[c("border", "width", "varwidth", "outline")]
# #     boxplot(x, frame = FALSE, axes = FALSE, horizontal = TRUE, ylim = range(pretty(x)), col = col.fill.boxplot, cex = cex, cex.axis = cex.axis)
# #     do.call(what = "boxplot",
# #             args = list(x = x,
# #                         frame = FALSE,
# #                         axes = FALSE,
# #                         horizontal = TRUE,
# #                         ylim = xlim,
# #                         col = col.fill.boxplot,
# #                         cex = cex))
#     do.call(what = "boxplot",
#             args = c(list(x = x,
#                           frame = FALSE,
#                           axes = FALSE,
#                           horizontal = TRUE,
#                           ylim = xlim,
#                           col = col.fill.boxplot),
#                      args.plot.boxplot))
#     if(plotVerticalLines) {
# #         segments(x0 = x.verticalStats, y0 = rep(0L, length.out = 4L), x1 = x.verticalStats, y1 = rep(1L, length.out = 4L), lty = lty.lines.vertical, lwd = lwd.lines.vertical, col = col.lines.vertical)
#         do.call(what = "segments",
#                 args = list(x0 = x.verticalStats,
#                             y0 = c(0L, 0L, 0L, 0L),
#                             x1 = x.verticalStats,
#                             y1 = c(1L, 1L, 1L, 1L),
#                             lty = lty.lines.vertical,
#                             lwd = lwd.lines.vertical,
#                             col = col.lines.vertical))
#     }
#     if(plotStatsValues) {
#         oldXPD <- par()$xpd
#         par(xpd = TRUE)
# #         mtext(text = paste("Q2", formattedQuartiles[1L], sep = "\n"), at = x.verticalStats[2L], side = 3, line = line.statsValues.top, col = col.statsValues.top, font = font.statsValues.top, cex = cex.statsValues)
# #         mtext(text = paste("Q1", formattedQuartiles[2L], sep = "\n"), at = x.verticalStats[3L], side = 3, line = line.statsValues.top, col = col.statsValues.top, font = font.statsValues.top, adj = 1, cex = cex.statsValues)
# #         mtext(text = paste("Q3", formattedQuartiles[3L], sep = "\n"), at = x.verticalStats[4L], side = 3, line = line.statsValues.top, col = col.statsValues.top, font = font.statsValues.top, adj = 0, cex = cex.statsValues)
#         do.call(what = "mtext",
#                 args = list(text = paste("Q1", formattedQuartiles[1L], sep = "\n"),
#                             at = x.verticalStats[3L],
#                             side = 3,
#                             line = line.statsValues.top,
#                             col = col.statsValues.top,
#                             font = font.statsValues.top,
#                             cex = cex.statsValues,
#                             adj = 1))
#         do.call(what = "mtext",
#                 args = list(text = paste("Q2", formattedQuartiles[1L], sep = "\n"),
#                             at = x.verticalStats[2L],
#                             side = 3,
#                             line = line.statsValues.top,
#                             col = col.statsValues.top,
#                             font = font.statsValues.top,
#                             cex = cex.statsValues))
#         do.call(what = "mtext",
#                 args = list(text = paste("Q3", formattedQuartiles[3L], sep = "\n"),
#                             at = x.verticalStats[4L],
#                             side = 3,
#                             line = line.statsValues.top,
#                             col = col.statsValues.top,
#                             font = font.statsValues.top,
#                             cex = cex.statsValues,
#                             adj = 0))
#         par(xpd = oldXPD)
#     }
#     layout(1)
# }






getBoxHist <- function(x, na.rm = TRUE, dataObjectName = NULL, digits = 2L, plotDensityCurve = TRUE, plotVerticalLines = TRUE, plotStatsValues = TRUE, pars = list(line.statsValues.top = -1.5, line.statsValues.bottom = 3L), ...) {
    def.par <- par(no.readonly = TRUE)
    on.exit(layout(1), add = TRUE)
    on.exit(par(def.par), add = TRUE)
    
    pars <- c(list(...), pars)
    pars <- uniquePars <- pars[unique(names(pars))]
    
    # Currently not accepting any titles for any plots
    if (any(grepl(pattern = "main$", x = names(pars)))) {
        pars[grep(pattern = "main$", x = names(pars))] <- NULL
    }
    
    pcycle <- function(p, def1, def2 = NULL) rep(if (length(p)) p else if (length(def1)) def1 else def2, length.out = max(length(p), length(def1), length(def2)))
    p <- function(sym) pars[[sym, exact = TRUE]]
    
    if(length(dataObjectName) < 1L) {
        dataObjectName <- deparse(substitute(x))
    }
    
    line.statsValues.top <- pcycle(pars[["line.statsValues.top"]], p("line.statsValues")[1L], -1.5)
    line.statsValues.bottom <- pcycle(pars[["line.statsValues.bottom"]], if (length(p("line.statsValues"))) p("line.statsValues")[2L] else p("line.statsValues"), 3L)
    
    col.fill <- pcycle(pars[["col.fill"]], p("fill"))
    col.fill.histogram <- pcycle(pars[["col.fill.histogram"]], col.fill)
    col.fill.boxplot <- pcycle(pars[["col.fill.boxplot"]], col.fill)
    
    col.lines <- pcycle(pars[["col.lines"]], p("col"), par("col"))
    lty.lines <- pcycle(pars[["lty.lines"]], p("lty"), par("lty"))
    lwd.lines <- pcycle(pars[["lwd.lines"]], p("lwd"), par("lwd"))
    
    x.boxplot.stats <- boxplot.stats(x)[["stats"]]
    args.histogram <- pars[c("breaks", "include.lowest", "right", "nclass")]
    x.hist <- do.call(what = "hist",
                      args = c(list(x = x,
                                    plot = FALSE,
                                    warn.unused = FALSE),
                               args.histogram))
    # Retrieved 2014-07-22 (slight modification): http://stackoverflow.com/a/9122859
    x.hist[["density"]] <- x.hist[["counts"]] / sum(x.hist[["counts"]])
    xlim <- pcycle(pars[["xlim"]], p("xlim"), range(pretty(x)))
    xlab <- pcycle(pars[["xlab"]], p("xlab.hist"), sprintf("Values of %s", sQuote(dataObjectName)))
    ylim.histogram <- pcycle(pars[["ylim.histogram"]], range(pretty(x.hist[["density"]])))
    ylim.boxplot   <- pcycle(pars[["ylim.boxplot"]], extendrange(range(1), f = 0.04))
    ylab <- pcycle(pars[["ylab"]], p("ylab.hist"), "Relative Frequency")
    if(plotDensityCurve) {
        x.density <- density(x)
        x.density[["y"]] <- x.density[["y"]] * diff(x.hist[["breaks"]])[1L]
        lty.lines.density <- pcycle(pars[["lty.lines.density"]], lty.lines[1L], par("lty"))
        lwd.lines.density <- pcycle(pars[["lwd.lines.density"]], lwd.lines[1L], par("lwd"))
        col.lines.density <- pcycle(pars[["col.lines.density"]], col.lines[1L], par("col"))
    }
    if(plotVerticalLines || plotStatsValues) {
        x.mean <- mean(x, na.rm = na.rm)
        x.sd <- sd(x, na.rm = na.rm)
        x.whisker.minimum <- x.boxplot.stats[1L]
        x.lowerhinge <- x.boxplot.stats[2L]
        x.median <- x.boxplot.stats[3L]
        x.upperhinge <- x.boxplot.stats[4L]
        x.whisker.maximum <- x.boxplot.stats[5L]
        x.verticalStats <- c(x.mean, x.median, x.lowerhinge, x.upperhinge)
        # formattedMean <- formatC(x.mean, format = "f", digits = digits)
        # formattedSD <- formatC(x.sd, format = "f", digits = digits)
        formattedMean <- signif(x.mean, digits = digits)
        formattedSD <- signif(x.sd, digits = digits)
        formattedMeanSD <- paste(formattedMean, " (", formattedSD, ")", sep = "")
        # formattedQuartiles <- formatC(c(x.median, x.lowerhinge, x.upperhinge), format = "f", digits = digits)
        # formattedQuartiles <- signif(c(x.median, x.lowerhinge, x.upperhinge), digits = digits)
        formattedQ1 <- paste(signif(x.lowerhinge, digits = digits), sep = "")
        formattedQ2 <- paste(signif(x.median, digits = digits), sep = "")
        formattedQ3 <- paste(signif(x.upperhinge, digits = digits), sep = "")
        formattedQuartiles <- c(formattedQ1, formattedQ2, formattedQ3)
        
        lty.lines.vertical <- pcycle(pars[["lty.lines.vertical"]], if (length(lty.lines) > 1L) lty.lines[-1L] else lty.lines, par("lty"))
        lwd.lines.vertical <- pcycle(pars[["lwd.lines.vertical"]], if (length(lwd.lines) > 1L) lwd.lines[-1L] else lwd.lines, par("lwd"))
        col.lines.vertical <- pcycle(pars[["col.lines.vertical"]], if (length(col.lines) > 1L) col.lines[-1L] else col.lines, par("col"))
        col.statsValues <- pcycle(pars[["col.statsValues"]], col.lines.vertical, par("col"))
        col.statsValues.top <- pcycle(pars[["col.statsValues.top"]], col.statsValues, col.lines.vertical)
        col.statsValues.bottom <- pcycle(pars[["col.statsValues.bottom"]], col.statsValues, col.lines.vertical)
        
        cex.statsValues <- pcycle(pars[["cex.statsValues"]], p("cex"), par("cex"))
        cex.statsValues.top <- pcycle(pars[["cex.statsValues.top"]], cex.statsValues, 1)
        cex.statsValues.bottom <- pcycle(pars[["cex.statsValues.bottom"]], cex.statsValues, 1)
        font.statsValues <- pcycle(pars[["font.statsValues"]], p("font"), par("font"))
        font.statsValues.top <- pcycle(pars[["font.statsValues.top"]], if (length(font.statsValues) > 1L) font.statsValues[1L] else font.statsValues, 1)
        font.statsValues.bottom <- pcycle(pars[["font.statsValues.bottom"]], if (length(font.statsValues) > 1L) font.statsValues[-1L] else font.statsValues, 1)
    }
    
    args.plot.histogram <- pars[c("border", "angle", "density", "axes", "labels", "add", "ann", "col.axis", "cex", "cex.axis", "cex.lab", "col.lab", "font.lab", "font.axis")]
    
    nf <- layout(matrix(c(2, 1), nrow = 2, ncol = 1, byrow = TRUE), height = c(1, 3))
    
    bg <- pcycle(pars[["bg"]], par("bg"))
    fg <- pcycle(pars[["fg"]], par("fg"))
    par(mar = c(5.1, 4.1, 0, 2.1), bg = bg, fg = fg)
    do.call(what = "plot",
            args = c(list(x = x.hist,
                          freq = FALSE,
                          col = col.fill.histogram,
                          main = NULL,
                          xlim = xlim,
                          ylim = ylim.histogram,
                          xlab = xlab,
                          ylab = ylab),
                     args.plot.histogram))
    if(plotDensityCurve) {
        do.call(what = "lines",
                args = list(x = x.density[["x"]],
                            y = x.density[["y"]],
                            lty = lty.lines.density,
                            lwd = lwd.lines.density,
                            col = col.lines.density))
    }
    if(plotVerticalLines) {
        do.call(what = "abline",
                args = list(v = x.verticalStats,
                            lty = lty.lines.vertical,
                            lwd = lwd.lines.vertical,
                            col = col.lines.vertical))
    }
    if(plotStatsValues) {
        do.call(what = "mtext",
                args = list(text = paste("Mean (SD)\n", formattedMeanSD, sep = ""),
                            side = 1,
                            line = line.statsValues.bottom,
                            at = min(xlim),
                            col = col.statsValues.bottom,
                            font = font.statsValues.bottom,
                            cex = cex.statsValues.bottom))
    }

    # title(main = sprintf("Plots for %s", sQuote(dataObjectName)), col.main = col.main, cex.main = cex.main, font.main = font.main, line = line.main)
    par(mar = c(0, 4.1, 1.1, 2.1))
    args.plot.boxplot <- pars[c("border", "width", "varwidth", "outline")]
    do.call(what = "boxplot",
            args = c(list(x = x,
                          frame = FALSE,
                          axes = FALSE,
                          horizontal = TRUE,
                          ylim = xlim,
                          col = col.fill.boxplot),
                     args.plot.boxplot))
    if(plotVerticalLines) {
        do.call(what = "segments",
                args = list(x0 = x.verticalStats,
                            y0 = c(0L, 0L, 0L, 0L),
                            x1 = x.verticalStats,
                            y1 = c(1L, 1L, 1L, 1L),
                            lty = lty.lines.vertical,
                            lwd = lwd.lines.vertical,
                            col = col.lines.vertical))
    }
    if(plotStatsValues) {
        oldXPD <- par()$xpd
        par(xpd = TRUE)
        do.call(what = "mtext",
                args = list(text = paste("Q1", formattedQuartiles[1L], sep = "\n"),
                            at = x.verticalStats[3L],
                            side = 3,
                            line = line.statsValues.top,
                            col = col.statsValues.top,
                            font = font.statsValues.top,
                            cex = cex.statsValues,
                            adj = 1))
        do.call(what = "mtext",
                args = list(text = paste("Q2", formattedQuartiles[1L], sep = "\n"),
                            at = x.verticalStats[2L],
                            side = 3,
                            line = line.statsValues.top,
                            col = col.statsValues.top,
                            font = font.statsValues.top,
                            cex = cex.statsValues))
        do.call(what = "mtext",
                args = list(text = paste("Q3", formattedQuartiles[3L], sep = "\n"),
                            at = x.verticalStats[4L],
                            side = 3,
                            line = line.statsValues.top,
                            col = col.statsValues.top,
                            font = font.statsValues.top,
                            cex = cex.statsValues,
                            adj = 0))
        par(xpd = oldXPD)
    }
    layout(1)
}




