# Example scenarios for calling certain functions; these are used
# to ensure that the output is as expected. In order for these to
# run properly, both the functions and the "fake" data (whenever not
# already supplied in R) must be defined first.
# 
# Note that any code that was used in the fine-tuning/debugging
# process of writing these functions was left commented-out in the
# script file with the function definitions.

## Examples for the variableType() function ##
variableType(TRUE)                                      # "binary"
variableType(rep(FALSE, 20))                            # "binary"
variableType(rbinom(100, 1, 0.5))                       # "binary"
variableType(letters)                                   # "key"
variableType(gl(5, 6))                                  # "categorical"
variableType(rnorm(100, 0, 1))                          # "continuous"
variableType(rnorm(5, 0, 1))                            # "continuous"
variableType(1:5)                                       # "key"
variableType(sample(1:5, 1000, TRUE))                   # "categorical"
variableType(sample(1:5, 40, TRUE))                     # "categorical"
variableType(sample(1:5, 10, TRUE))                     # "categorical"
variableType(sample(1:20, 100, TRUE))                   # "continuous"
variableType(sample(1:8, 100, TRUE))                    # "categorical"
variableType(letters[sample(1:26, 100, TRUE)])          # "categorical"
variableType(as.Date(-1000:100, origin = "2000-01-01")) # "date"
variableType(Sys.Date())                                # "date"
variableType(FALSE)                                     # "binary"
variableType(NA)                                        # Warning + "NA"
variableType(NA, NAIsError = FALSE)                     # Warning + "NA"
variableType(NA, NAIsError = TRUE)                      # ERROR
## End examples for variableType() ##


## Examples for the checkColumns() function ##
cn <- c(colnames(iris), colnames(mtcars))
checkColumns(x = iris, columns = cn[1:6])
checkColumns(x = iris, columns = cn[1:7])
checkColumns(x = iris, columns = cn)
checkColumns(x = iris, columns = c(cn,letters))
checkColumns(x = mtcars, columns = cn)
checkColumns(x = mtcars, columns = letters)

ci <- seq_len(ncol(iris)+ncol(mtcars))
checkColumns(x = iris, columns = ci[1:6])
checkColumns(x = iris, columns = ci[1:6], keepColumnNames = FALSE)
checkColumns(x = iris, columns = ci[1:7])
checkColumns(x = iris, columns = ci[1:7], keepColumnNames = FALSE)
checkColumns(x = iris, columns = ci)
checkColumns(x = mtcars, columns = ci)
checkColumns(x = mtcars, columns = ci, keepColumnNames = FALSE)

# Define 'zmat' and 'znmat' first, then start testing
testcn <- c("Col1", "Col5", "Column10")
checkColumns(x = zmat, columns = testcn)
checkColumns(x = znmat, columns = testcn)

testci <- c(1, 6, 70, 3, 19)
checkColumns(x = zmat, columns = testci)
checkColumns(x = zmat, columns = testci, keepColumnNames = FALSE)
checkColumns(x = znmat, columns = testci)

checkColumns(x = data.frame(z), columns = "all")
## End examples for checkColumns() ##


## Examples using the by(), with(), and aggregate() functions ##
by(data = DFFH[, "age"], INDICES = DFFH[, "doc"], FUN = mean)
by(data = DFFH[, "wtkg"], INDICES = DFFH[, c("doc", "sex")], FUN = median)
by(data = DFFH[, "htcm"], INDICES = DFFH[, "state"], FUN = summary)
by(data = DFFH[, "age"], INDICES = DFFH[, c("doc", "sex", "state", "boo")], FUN = sd)
with(DFFH, by(DFFH, doc, summary))
by(data = DFFH, INDICES = DFFH[, "doc"], FUN = summary)
tapply(X = DFFH$htcm, INDEX = DFFH$sex, FUN = mean)
tapply(X = DFFH$htcm, INDEX = list(DFFH$sex, DFFH$doc), FUN = mean)
tapply(X = DFFH$htcm, INDEX = list(DFFH$sex, DFFH$doc, DFFH$state), FUN = mean)
aggregate(x = DFFH, by = list(Doctor = DFFH$doc), FUN = mean)
aggregate(x = DFFH[, sapply(DFFH, is.numeric)], by = list(Doctor = DFFH$doc), FUN = mean)
aggregate(x = DFFH[, sapply(DFFH, is.numeric)], by = list(Doctor = DFFH$doc, Gender = DFFH$sex), FUN = IQR)
## End examples for by(), with(), and aggregate()


## Examples of output for the descriptiveStatsDF() function ##
# Testing functionality (vectors)
descriptiveStatsDF(x = z, stats = c("mean", "sd"), na.rm = FALSE)
descriptiveStatsDF(x = zn, stats = c("mean", "sd"), na.rm = TRUE)
descriptiveStatsDF(x = zn, stats = c("VARI", "IQR", "fivenum"), na.rm = TRUE)
descriptiveStatsDF(x = zn, stats = "mode", na.rm = TRUE)
descriptiveStatsDF(x = zn, stats = "mode", na.rm = TRUE, silent = TRUE)
descriptiveStatsDF(x = z, stats = "mode", na.rm = TRUE)
descriptiveStatsDF(x = z, stats = c("min", "max", "range"), na.rm = TRUE)
descriptiveStatsDF(x = z, stats = c("min", "max", "range"), na.rm = TRUE, digits = 4)
descriptiveStatsDF(x = z, stats = c("min", "max", "range"), na.rm = TRUE, digits = 10)
descriptiveStatsDF(x = z, stats = "mode", na.rm = TRUE, digits = 10)
descriptiveStatsDF(x = x, stats = c("n", "mean", "q3"), na.rm = TRUE)
descriptiveStatsDF(x = y, stats = c("range", "default"), na.rm = TRUE)

# Testing functionality (matrices)
descriptiveStatsDF(x = zmat, columns = 1, stats = c("mean", "sd", "range", "mode"), na.rm = FALSE)
descriptiveStatsDF(x = zmat, columns = c(1, 2, 5), stats = c("mean", "sd", "IQR"), na.rm = FALSE)
descriptiveStatsDF(x = zmat, columns = c("Col1", "Col2", "Col5"), stats = c("mean", "sd", "IQR"), na.rm = FALSE)
descriptiveStatsDF(x = zmat, columns = c("Col1", "Col2", "Col5"), stats = c("mean", "sd", "IQR"), na.rm = FALSE, digits = 3)
descriptiveStatsDF(x = znmat, columns = c(2, 4, 6), stats = c("VAR", "MEDIAN", "FIVENUM"), na.rm = FALSE)
descriptiveStatsDF(x = znmat, columns = c(2, 4, 6), stats = c("VAR", "MEDIAN", "FIVENUM"), na.rm = TRUE)
descriptiveStatsDF(x = znmat, columns = c(2, 4, 6), stats = c("VAR", "MEDIAN", "FIVENUM"), na.rm = TRUE, digits = 4)
descriptiveStatsDF(x = zmat, stats = "all")
descriptiveStatsDF(x = zmat, stats = "all", silent = TRUE)
descriptiveStatsDF(x = zmat, stats = "all", digits = 5, silent = TRUE)
descriptiveStatsDF(x = znmat, columns = c(1, 5, 19))
descriptiveStatsDF(x = znmat, columns = c(1, 5, 19), na.rm = TRUE)

# Testing functionality (data frame)
descriptiveStatsDF(x = iris, columns = c(1, 3), stats = c("mean", "sd"), na.rm = TRUE, digits = 4)
descriptiveStatsDF(x = iris, columns = c("Sepal.Length", "Petal.Length", "Missing"), stats = c("mean", "sd", "pvalue"), na.rm = TRUE)
descriptiveStatsDF(x = iris, columns = c("Sepal.Length", "Petal.Length", "Missing"), stats = c("mean", "sd", "pvalue"), na.rm = TRUE, silent = TRUE)
descriptiveStatsDF(x = xyxxdf, na.rm = TRUE)
descriptiveStatsDF(x = xyxxdf, na.rm = TRUE, digits = 3)
descriptiveStatsDF(x = iris, columns = c("Sepal.Length", "Petal.Length", "Missing"), stats = c("mean", "sd", "pvalue"), na.rm = TRUE)
descriptiveStatsDF(x = iris, columns = c("Sepal.Length", "Petal.Length", "Missing"), stats = c("mean", "sd", "pvalue"), na.rm = TRUE, silent = TRUE)
descriptiveStatsDF(x = iris, columns = c("Sepal.Length", "Petal.Length", "Missing"), stats = c("mean", "sd", "pvalue"), na.rm = TRUE, silent = TRUE, digits = 3)
descriptiveStatsDF(x = xdf, silent = TRUE)

descriptiveStatsDF(x = iris)
descriptiveStatsDF(x = iris, stats = "all")
descriptiveStatsDF(x = DFFH)
descriptiveStatsDF(x = DFFH, stats = "all")
descriptiveStatsDF(x = DFFH, stats = "default", columns = 1:4)
descriptiveStatsDF(x = DFFH, stats = "default", columns = 1:4, keepColumnNames = TRUE)
descriptiveStatsDF(x = DFFH, stats = "default", columns = 1:4, keepColumnNames = FALSE)
descriptiveStatsDF(x = DFFH, stats = "default", columns = c("key", "sex", "age", "state"), keepColumnNames = FALSE)

descriptiveStatsDF(x = DFFH, stats = c("quantiles", "summary"))
descriptiveStatsDF(x = DFFH, stats = c("quantiles", "summary"), quantile.probs = c(0.05, 0.10, 0.50, 0.90, 0.95), silent = TRUE)

# Testing functionality (lists, factors)
descriptiveStatsDF(x = zmat, columns = c(6, 2), stats = c("mean", "var", "n"))
descriptiveStatsDF(x = zlist, columns = c(6, 2), stats = c("mean", "var", "n"))
descriptiveStatsDF(x = factordf, silent = TRUE)
## End examples for descriptiveStatsDF() ##

## Some more examples of output for the descriptiveStatsDF() function ##
descriptiveStatsDF(x = exVector, stats = c("n", "mean", "sd"), na.rm = TRUE)
descriptiveStatsDF(x = exCharacter, stats = c("n", "mean", "sd"), na.rm = TRUE)
descriptiveStatsDF(x = exLogical, stats = c("n", "mean", "sd"), na.rm = TRUE)
descriptiveStatsDF(x = exInteger, stats = c("n", "mean", "sd"), na.rm = TRUE)
descriptiveStatsDF(x = exFactor, stats = c("n", "mean", "sd"), na.rm = TRUE)
descriptiveStatsDF(x = exVector, stats = c("all"), na.rm = TRUE)
descriptiveStatsDF(x = exVector, stats = c("default"), na.rm = TRUE)
descriptiveStatsDF(x = exVector, na.rm = TRUE)
descriptiveStatsDF(x = exVector, stats = c("var", "min", "quartiles"), na.rm = TRUE)
descriptiveStatsDF(x = exVector, stats = c("var", "min", "quartiles"), na.rm = TRUE, digits = 2L)
descriptiveStatsDF(x = exDate, na.rm = TRUE)
descriptiveStatsDF(x = DFFH$htcm, stats = "mean", na.rm = TRUE)
## End some more examples for descriptiveStatsDF() ##





