# Experimenting with output options, including:
# -- pander
# -- xtable
# ?? Hmisc
# -- rtf
# ?? stargazer
# -- rapport


library(pander)
# # Copied directly from 'Examples' section of the help page entry for
# # 'Pandoc' in the package 'pander' (ver. 0.3.8)
## Not run: 
## Initialize a new Pandoc object
myReport <- Pandoc$new()

## Add author, title and date of document
myReport$author <- 'Anonymous'
myReport$title  <- 'Demo'

## Or it could be done while initializing
myReport <- Pandoc$new('Anonymous', 'Demo')

## Add some free text
myReport$add.paragraph('Hello there, this is a really short tutorial!')

## Add maybe a header for later stuff
myReport$add.paragraph('# Showing some raw R objects below')

## Adding a short matrix
myReport$add(matrix(5,5,5))

## Or a table with even # TODO: caption
myReport$add.paragraph('Hello table:')
myReport$add(table(mtcars$am, mtcars$gear))

## Or a "large" data frame which barely fits on a page
myReport$add(mtcars)

## And a simple linear model with Anova tables
ml <- with(lm(mpg ~ hp + wt), data = mtcars)
myReport$add(ml)
myReport$add(anova(ml))
myReport$add(aov(ml))

## And do some principal component analysis at last
myReport$add(prcomp(USArrests))

## Sorry, I did not show how Pandoc deals with plots:
myReport$add(plot(1:10)) # TODO: caption

## Want to see the report? Just print it:
myReport

## Exporting to pdf (default)
myReport$export()

## Or to docx in tempdir():
myReport$format <- 'docx'
myReport$export(tempfile())

## You do not want to see the generated report after generation?
myReport$export(open = FALSE)

## End(Not run)

testout <- descriptiveStatsDF(iris)
# myReport <- Pandoc$new()
# myReport$author <- "Ted"
# myReport$title  <- "Test Document"
# myReport$date   <- ""
myReport <- Pandoc$new(author = "Ted", title = "Pander Test Document", date = "")
myReport$add(testout)
myReport$format <- "docx"
myReport$export()

myReport$format <- "pdf"
myReport$export()


# ## Example 'pander' outputs ##
# outfile <- paste(getwd(), "PanderTestDocument", sep = "/")
# myReport <- Pandoc$new(author = "Author Unknown", title = "Pander Test Document", date = as.character(Sys.Date()))
# # myReport$add(head(iris))
# myReport$add(descriptiveStatsDF(iris))
# # myReport$format <- "pdf" # this is the default
# myReport$export(outfile)
# 
# myReport$format <- "rtf"
# myReport$export(outfile)
# 
# myReport$format <- "doc"
# myReport$export(outfile)
# 
# myReport$format <- "docx"
# myReport$export(outfile)
# ## End 'pander' examples ##


library(rtf)
testout <- descriptiveStatsDF(iris)
outfile <- paste(getwd(), "RTFTest.doc", sep = "/")
# Modified from 'RTF' help page section 'Examples' in the package
# 'rtf' (ver. 0.4-11)
rtf <- RTF(file = outfile, width = 8.5, height = 11, omi = c(1, 1, 1, 1), font.size = 10)

addHeader(this = rtf, title = "RTF Test Document", subtitle = strftime(Sys.time(), format = "%Y-%m-%d"))
addPlot(this = rtf, plot.fun = plot, width = 6, height = 6, res = 300, x = iris[, 1L], y = iris[, 2L])

addTable(this = rtf, dat = head(iris), font.size = 10, row.names = FALSE, NA.string = "-", col.widths = rep(1L, 5L))
addTable(this = rtf, dat = testout$Categorical, font.size = 10, row.names = TRUE)

addPageBreak(this = rtf, width = 8.5, height = 11, omi = c(1, 1, 1, 1))
addSessionInfo(this = rtf)

done(rtf)
view(rtf)

# ## Example 'rtf' outputs ##
# rtf <- RTF(file = paste(getwd(), "RTFTestDocument.rtf", sep = "/"))
# addHeader(rtf, title = "RTF Test Document", subtitle = "Unknown Author", font.size = 16)
# # addTable(rtf, head(iris), row.names = FALSE, col.justify = c(rep("R", 4), "C"), header.col.justify = "C")
# addTable(rtf, descriptiveStatsDF(iris)$Categorical)
# addNewLine(rtf, n = 2)
# addTable(rtf, descriptiveStatsDF(iris)$Continuous)
# done(rtf)
# ## End 'rtf' examples ##


