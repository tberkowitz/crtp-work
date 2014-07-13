# Generate fake data
set.seed(1234)
testsample <- sample(1:20, size = 1000, replace = TRUE)
x <- tabulate(testsample)
testsample2 <- c(testsample, 19, 19, 7)
y <- tabulate(testsample2)
xx <- x; xx[3L] <- NA_integer_

xdf <- data.frame(x)
xyxxdf <- cbind(xdf, y, xx)

set.seed(0528)
z <- zn <- rnorm(n = 100)
zn[sample(x = 1:length(zn), size = 5, replace = FALSE)] <- NA_real_

zmat <- matrix(z, nrow = 10, ncol = 10)
colnames(zmat) <- paste("Col", 1:10, sep = "")
znmat <- matrix(zn, nrow = 5, ncol = 20)
zlist <- as.list(as.data.frame(zmat))

set.seed(0607)
factordf <- cbind(xyxxdf, factor1=sample(x = letters[1:4], size = 20, replace = TRUE))

# Use the 'iris' data set for testing data frames as well

nRows <- 500
set.seed(1234)
DFFH <- data.frame(key = seq_len(nRows),
                   sex = factor(c("F", "M")[rbinom(n = nRows, size = 1, prob = 0.5) + 1L]),
                   age = sample(x = 18:80, size = nRows, replace = TRUE),
                   state = sample(x = state.name, size = nRows, replace = TRUE),
                   doa = as.Date(-1*sample(x = 1:1000, size = nRows, replace = TRUE), origin = "2014-06-01"),
                   htcm = sample(x = 150:200, size = nRows, replace = TRUE),
                   wtkg = sample(x = 400:1300/10, size = nRows, replace = TRUE),
                   doc = factor(c("Smith", "Brown", "Johnson", "Anderson", "Williams")[sample(x = 1:5, size = nRows, replace = TRUE)]),
                   boo = as.logical(rbinom(n = nRows, size = 1, p = 0.5)),
                   stringsAsFactors = FALSE)
rm(nRows)

nRows <- 500
set.seed(0709)
factorsdf <- data.frame("f1" = factor(letters[sample(1:3, nRows, TRUE)]),
                        "f2" = factor(sample(c("alpha", "bravo", "charlie", "delta", "echo"), nRows, TRUE)),
                        "f3" = factor(sample(state.name, nRows, TRUE)),
                        "f4" = factor(sample(c("x", "y", NA_character_), nRows, TRUE)),
                        "f5" = factor(sample(state.name[1:7], nRows, TRUE)))
rm(nRows)

# Generate some more fake data/values to use to test functions
set.seed(123)
exVector <- rnorm(n = 100, mean = 0, sd = 1)
exMatrix <- matrix(exVector, nrow = 10)
exDF <- data.frame(exMatrix)
exList <- as.list(exDF)
exArray <- array(data = exVector, dim = c(5, 5, 4))

NAIndices <- sort(sample(x = 1:100, size = 10, replace = FALSE))
exVectorNA <- exVector; exVectorNA[NAIndices] <- NA_integer_
exMatrixNA <- matrix(exVectorNA, nrow = 10)
exDFNA <- data.frame(exMatrixNA)
exListNA <- as.list(exDFNA)
exArrayNA <- array(data = exVectorNA, dim = c(5, 5, 4))


set.seed(321)
exNumeric <- rnorm(n = 100, mean = 0, sd = 1)
exInteger <- sample(x = 1:26, size = 100, replace = TRUE)
exCharacter <- LETTERS[exInteger]
exFactor <- as.factor(exCharacter)
exDate <- as.Date(x = exInteger*100, origin = "2000-01-01", format = "%Y-%m-%d")
exLogical <- exNumeric > 0L
exFormula <- formula(exVector ~ exFactor)
exExpression <- expression(x + 1)
exName <- as.name(exCharacter)
exSymbol <- as.symbol(exCharacter)

