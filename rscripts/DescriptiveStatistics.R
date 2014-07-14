## Goals for the end result ##
# Input data set can be vector, matrix, list, data frame
# Input data can be numeric, character, factor, logical, integer, Date, complex?
# Input data can be named/have named dimensions
# User can request specific statistics or all statistics or nothing (to get a default set of statistics)
# User can request statistics for some or all variables
# *** User can request statistics BY FACTOR LEVELS ***
# *** User can specify 'key' variable to ignore for analyses ***
# User can request decimal places for floating-point results (default = 2)
# ??? User can request categorical or continuous output only ???
# User can request to ignore all warnings
## End goals ##

### SOURCE "DATA" ###
source(paste(getwd(), "FakeData.R", sep = "/"))

### FUNCTION DEFINITIONS ###
# Define the MFV() function
MFV <- function(x, outputValue, na.rm = getOption("na.rm", default = FALSE), silent = FALSE){
    if(silent){
        oldWarn <- options()$warn
        options("warn" = -1)
        on.exit(options("warn" = oldWarn))
    }
    
    if(!missing(outputValue) && is.character(outputValue)){
        if(outputValue == ""){
            outputValue <- "all"
        } else {
            outputValue <- match.arg(arg = tolower(outputValue), choices = c("minimum", "maximum", "all"), several.ok = FALSE)
        }
    }
    
    uniqueX <- unique(x)
    if(length(x) == length(uniqueX)){
        warning("There were no duplicate values in the data. Result is the first data value.")
        if(na.rm){
            x <- x[!is.na(x)]
            return(x[1L])
        } else {
            return(x[1L])
        }
    }
    
    if(na.rm){            # Running this prevents you from being
        x <- x[!is.na(x)] # able to run deparse(substitute(x)) and
    }                     # get the object name later on.
    
    freqs <- as.vector(table(x)) # NB: table() sorts frequencies by their unique values in ascending order
    maxFreq <- max(freqs)
    if(identical(length(unique(freqs)), 1L)){
        warning(sprintf("All data values occur with frequency %1d. Only the first value will be returned.", as.integer(unique(freqs))))
        return(x[1L])
    }
    
    if(!missing(outputValue)){
        if(outputValue == "minimum"){
            mfv <- min(sort(uniqueX)[(freqs - maxFreq) == 0])
        } else if(outputValue == "maximum"){
            mfv <- max(sort(uniqueX)[(freqs - maxFreq) == 0])
        } else {
            mfv <- (sort(uniqueX)[(freqs - maxFreq) == 0])
        }
    } else {
        mfv <- (sort(uniqueX)[(freqs - maxFreq) == 0])
    }
    return(mfv)
}

# Define the checkStats() function
checkStats <- function(stats = "default", dataObjectName = NULL){
    
    # Supply a data object name if none is provided
    if(length(dataObjectName) == 0L){
        dataObjectName <- "the data set"
    }
    
    # Make sure all of the values specified for "stats" are
    # lower case (to facilitate string matching)
    lowStats <- tolower(stats)
    
    # List out all of the options for "stats" that will be
    # accepted/recognized by the function
    possibleStats <- c("default", "all", "n", "obs", "nobs", "missing", "nmissing", "na", "mean", "average", "avg", "median", "mode", "sd", "standard deviation", "variance", "iqr", "minimum", "maximum", "fivenum", "interquartile range", "range", "std", "std dev", "mfv", "q1", "quartile1", "q3", "quartile3", "quartiles", "quantiles", "q2", "quartile2", "summary")
        # ??"freqs", "frequencies"??
    if(!any(pmatch(x = lowStats, table = possibleStats, nomatch = 0L) > 0L)) {
        stop(paste(strwrap(gettextf("I need at least one valid statistic to be specified before I can do anything with %s. Please check for spelling errors.", sQuote(dataObjectName)), width = 0.95 * getOption("width")), collapse = "\n    "))
    }
    validStats <- match.arg(arg = lowStats, choices = possibleStats, several.ok = TRUE)
    if(length(lowStats) != length(validStats)) {
        # Use "stats" instead of "lowStats" so the printed
        # values resemble the original user input, not the
        # tolower()ed argument(s)
        invalidStats <- stats[-match(x = possibleStats, table = lowStats, nomatch = 0L)]
        warning(paste(strwrap(gettextf("At least one specified statistic was invalid. Only recognized statistics were used for the results. I did not recognize: %s.", paste(invalidStats, collapse = ", "))), collapse = "\n    "))
    }
    
    # Check which statistics options selected
    requestedStatsNames <- c("n", "mean", "median", "mode", "sd", "variance", "minimum", "maximum", "range", "iqr", "fivenum", "quartile1", "quartile3", "quartiles", "quantiles", "summary")
    requestedStats <- rep(FALSE, times = length(requestedStatsNames))
    names(requestedStats) <- requestedStatsNames
    
    # Check for "n"
    requestedStats["n"] <- any(c("n", "obs", "nobs", "missing", "nmissing", "na", "default", "all") %in% validStats)
    
    # Check for "mean"
    requestedStats["mean"] <- any(c("mean", "average", "default", "all") %in% validStats)
    
    # Check for "median"
    requestedStats["median"] <- any(c("median", "q2", "quartile2", "default", "all") %in% validStats)
    
    # Check for "mode"
    requestedStats["mode"] <- any(c("mode", "mfv", "all") %in% validStats)
    
    # Check for "sd"
    requestedStats["sd"] <- any(c("sd", "standard deviation", "std", "std dev", "default", "all") %in% validStats)
    
    # Check for "variance"
    requestedStats["variance"] <- any(c("variance", "all") %in% validStats)
    
    # Check for "minimum"
    requestedStats["minimum"] <- any(c("minimum", "default", "all") %in% validStats)
    
    # Check for "maximum"
    requestedStats["maximum"] <- any(c("maximum", "default", "all") %in% validStats)
    
    # Check for "range"
    requestedStats["range"] <- any(c("range", "all") %in% validStats)
    
    # Check for "iqr"
    requestedStats["iqr"] <- any(c("iqr", "interquartile range", "all") %in% validStats)
    
    # Check for "fivenum"
    if(requestedStats["fivenum"] <- any(c("fivenum", "all") %in% validStats)){
        requestedStats[c("minimum", "median", "maximum")] <- FALSE
    }
    
    # Check for "quartile1"
    requestedStats["quartile1"] <- any(c("q1", "quartile1") %in% validStats)
    
    # Check for "quartile3"
    requestedStats["quartile3"] <- any(c("q3", "quartile3") %in% validStats)
    
    # Check for "quartiles"
    if(requestedStats["quartiles"] <- any(c("quartiles", "all") %in% validStats)){
        requestedStats[c("quartile1", "median", "quartile3")] <- FALSE
    }
    
    # Check for "quantiles"
    requestedStats["quantiles"] <- any(c("quantiles", "all") %in% validStats)
    
    # Check for "summary"
    if(requestedStats["summary"] <- any(c("summary", "all") %in% validStats)) {
        requestedStats[c("minimum", "quartile1", "mean", "median", "quartile3", "maximum", "quartiles")] <- FALSE
    }

    return(requestedStats)
}

# Define the variableType() function - check if continuous/categorical/binary/etc.
variableType <- function(x, NAIsError = FALSE){
    x <- x[!is.na(x)]
    if(length(x) < 1L){
        if(NAIsError) {
            stop(strwrap(gettextf("The variable does not have enough non-missing values for me to determine its type."), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""))
        } else {
            warning(strwrap(gettextf("The variable does not have enough non-missing values for me to determine its type."), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""))
            return(NA_character_)
        }
    }
    if((length(unique(x)) == 2L && length(x) > 2L && !(is.factor(x) || is.character(x))) || is.logical(x)){
        "binary"
    } else if(any(class(x) %in% c("Date", "POSIXt", "POSIXct", "POSIXlt"))) {
        "date"
    } else if(is.character(x) || is.factor(x) || (is.integer(x) && length(unique(x)) <= 10L)){
        "categorical"
    } else {
        "continuous"
    }
}


# Define the quantile.datesOK() function
quantile.datesOK <- function(x, probs = seq(from = 0, to = 1, by = 0.25), type = 7L, dateFormat = "%Y-%m-%d", ...){
    require("stats")
    if(missing(probs) || length(probs) == 0L) {
        probs <- seq(from = 0, to = 1, by = 0.25)
    }
    probs <- sort(unique(probs[(!is.na(probs)) & (probs >= 0L)]))
    if(any(probs > 1L)){
        probs <- probs / 100
    }
    if(missing(type) || is.null(type) || !(as.integer(type[1L]) %in% 1:9)){
        type <- 7L
    }
#     if(any(class(x) %in% c("POSIXt", "Date"))){
#         # Modified from the code for stats:::quantile.POSIXt
#         strftime(.POSIXct(quantile(unclass(as.POSIXct(x)), probs = probs, type = type, ...), attr(x, "tzone")), format = dateFormat)
#     } else {
#         quantile(x, probs = probs, type = type, ...)
#     }
    if(any(class(x) %in% c("POSIXt", "Date"))){
        x <- unclass(x)
    }
    quantile(x, probs = probs, type = type, ...)
}

# Define the fivenum.datesOK() function
fivenum.datesOK <- function(x, na.rm = TRUE) {
    y <- if(isDateVar <- any(class(x) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt"))) {
        unclass(x)
    } else {
        x
    }
    results.fivenum.datesOK <- fivenum(x = y, na.rm = na.rm)
#     if(isDateVar) {
#         class(results.fivenum.datesOK) <- class(x)
#     }
    return(results.fivenum.datesOK)
}


# Define the checkColumns() function
checkColumns <- function(x, columns = "all", dataObjectName = NULL, keepColumnNames = TRUE, ignore = NULL){
    # Define the function pasteWrap(), custom-made function meant
    # only for use inide this (checkColumns()) function to faciliate
    # readable error and warning printing to the console
    pasteWrap <- function(msg, dataObjectName, what, width = 0.95 * getOption("width")) {
        paste(strwrap(gettextf(msg, sQuote(dataObjectName), paste(what, collapse = ", ")), width = width), collapse = "\n    ")
    }
    
    # If 'x' doesn't have columns (for some weird reason), then
    # make it a data frame
    if(is.null(dim(x)[2L])) {
        x <- data.frame(x)
    }
    
    # If (somehow) 'columns' is NULL (or otherwise a vector of
    # length 0), then set 'columns' == "all"
    if(!(length(columns) > 0L)) {
        columns <- "all"
    }
    
    # If the 'dataObjectName' argument is longer than 1L, only take
    # the first element
    dataObjectName <- dataObjectName[1L]
    
    # If nothing is specified for the 'dataObjectName' argument or if
    # the value specified is an empty string, use the value of the
    # 'x' argument instead
    if((length(dataObjectName) == 0L) || (dataObjectName == "")){
        dataObjectName <- deparse(substitute(x))
    }
    
    # Store the column names (if they exist) to make subsequent
    # code more legible
    if(is.integer(type.convert(dimnames(x)[[2L]], as.is = TRUE))) {
        oldColumnNames <- NULL
    } else {
        oldColumnNames <- dimnames(x)[[2L]]
    }
    
    # In the event of 'columns' = "all" (the default choice)
    if(any(columns == "all")) {
        if(!is.null(oldColumnNames)) {
            columns <- oldColumnNames
        } else {
            columns <- seq_len(ncol(x))
        }
    }
    
    # Just to be safe
    columns <- unique(na.omit(unlist(columns)))
    
    # Accounting for complex numbers being used to specify 'columns'
    # (HOW WOULD THIS EVEN HAPPEN???)
    if(is.complex(columns)) {
        columns <- unique(as.integer(Re(columns)))
    }
    
    # If told to ignore column(s), then exclude it/them from 'columns'
    if(length(ignore) > 0) {
        if(is.character(ignore) && is.numeric(columns)) {
            ignore <- which(oldColumnNames %in% ignore)
        } else if(is.numeric(ignore) && is.character(columns)) {
            ignore <- unique(as.integer(ignore[ignore > 0L & ignore <= ncol(x)]))
            ignore <- oldColumnNames[ignore]
        }
    }
    columns <- setdiff(columns, ignore)
    
    # Convert "columns" (the column specifier, not the actual
    # column [yet]) from factor to character (or numeric?)
    if(mode(columns) != "numeric" || is.factor(columns)){
        
        # Make sure the specified column names are treated
        # as character if they aren't numeric and/or are
        # factors
        columns <- as.character(columns)
        
        # If user specifies columns by name
            if(!all(match(x = columns, table = colnames(x), nomatch = 0L) > 0L)){
                if(!any(match(x = columns, table = colnames(x), nomatch = 0L) > 0L)){
                    stop(paste(strwrap(gettextf("None of the specified columns exists in the data set %s.", sQuote(dataObjectName)), width = 0.95 * getOption("width")), collapse = "\n    "))
                } else {
                    invalidColumns <- columns[!(match(x = columns, table = colnames(x), nomatch = 0L) > 0L)]
                    # ngettext() might evaluate BOTH msg1 and msg2 for
                    # all conditions --> does not like simply using
                    # paste(invalidColumns) if length(invalidColumns)>1
                    # since you get more than one string back after
                    # evaluating msg1 --> specify to only ever paste()
                    # the first element of invalidColumns for msg1
                    warning(ngettext(n = length(invalidColumns),
                                     msg1 = pasteWrap(msg = "The following column does not exist in %s and will be ignored: %s.", dataObjectName = dataObjectName, what = invalidColumns[1L]),
                                     msg2 = pasteWrap(msg = "The following columns do not exist in %s and will be ignored: %s.", dataObjectName = dataObjectName, what = invalidColumns)))
                    columns <- columns[match(x = columns, table = colnames(x), nomatch = 0L) > 0L]
                }
            }
            columnNames <- columns
    } else {
        # If "columns" *is* numeric, make sure it's integer. Why?
        # Because I can. Also, check to make sure the values are
        # within the bounds of the original data object's
        # dimensions.
        columns <- unique(as.integer(columns))

        # Stop everything if none of the columns are valid
        if(any(columns > ncol(x)) || any(columns < 1L)){
            if(all(columns > ncol(x)) || all(columns < 1L) || is.null(columns)) {
                stop(ngettext(n = length(columns),
                                msg1 = paste(strwrap(gettextf("The specified column does not exist in the data set %s.", sQuote(dataObjectName)), width = 0.95 * getOption("width")), collapse = "\n    "),
                                msg2 = paste(strwrap(gettextf("None of the specified columns exists in the data set %s.", sQuote(dataObjectName)), width = 0.95 * getOption("width")), collapse = "\n    ")))
            } else {
            # Make note of which specified values are invalid
            # column selections, generate a warning about which
            # were invalid, and remove the invalid ones from
            # the variable
            invalidColumns <- as.integer(columns[which((columns > ncol(x)) | (columns < 1L))])
            warning(ngettext(n = length(invalidColumns),
                             msg1 = pasteWrap(msg = "The following column index does not exist in the data set %s and will be ignored: %s.", dataObjectName = dataObjectName, what = invalidColumns[1L]),
                             msg2 = pasteWrap(msg = "The following column indices do not exist in the data set %s and will be ignored: %s.", dataObjectName = dataObjectName, what = invalidColumns)))
            columns <- columns[!(columns %in% invalidColumns)]
            }
        }
        if(keepColumnNames && !is.null(oldColumnNames)) {
            columnNames <- oldColumnNames[columns]
        } else {
            columnNames <- paste("Column ", columns, sep = "")
        }
    }
    if(all(grepl(pattern = "^[XV]", x = columnNames))) {
        columnNames <- gsub(pattern = "^[XV]([[:digit:]]*)", replacement = "Column \\1", x = columnNames)
    }
    return(list("validColumns" = columns, "validColumnNames" = columnNames))
}


# # Used the following to debug the getResults.categorical() function
# x <- factorsdf
# na.rm <- TRUE
# silent <- FALSE
# digits <- 2L
# emptyCellSymbol <- ""
# maxLevels <- 10L
# na.exclude <- FALSE
# statsAreRows <- TRUE
# # End debugging values for getResults.categorical() function

# Define the getResults.categorical() function
getResults.categorical <- function(x, na.rm = FALSE, emptyCellSymbol = "", maxLevels = 10L, na.exclude = na.rm, statsAreRows = TRUE){
    # Initialize an empty list object named "results"
    results <- list()
    
    # Check if each factor has missing values
    hasNA <- sapply(X = x, FUN = anyNA)
    
    # If there are no missing values for any of the factors, then
    # don't bother worrying about including information about them
    # in the factor levels frequencies table, regardless of the
    # specified value of 'na.exclude'
    if(!any(hasNA)) {
        na.exclude <- TRUE
    }
    
    if(na.exclude) {
        results[["N"]] <- sapply(X = x, FUN = function(x){length(x) - sum(is.na(x))})
        results[["Missing"]] <- sapply(X = x, FUN = function(x){sum(is.na(x))})
        results[["Unique"]] <- sapply(X = x, FUN = function(x){length(unique(x[!is.na(x)]))})
        table.useNA <- "no"
    } else {
        results[["N"]] <- sapply(X = x, FUN = length)
        results[["Missing"]] <- sapply(X = x, FUN = function(x){sum(is.na(x))})
        results[["Unique"]] <- sapply(X = x, FUN = function(x){length(unique(x))})
        table.useNA <- "always"
    }
    
    # What should be put in the empty "cells" of the factor levels
    # frequencies table? (Only accept the first element if a vector
    # of length greater than 1 is specified)
    emptyCellSymbol <- as.character(emptyCellSymbol[1L])
    
    # Create a row of empty strings to serve as a "buffer" between
    # the overall results and the factor levels frequencies table
    bufferRow <- rep(emptyCellSymbol, times = ncol(x))
    
    # Note which row in the output data frame will correspond to the
    # "buffer" row (will be used to remove default row name for the
    # "buffer" row so it displays a blank)
    bufferRowIndex <- length(results) + 1L
    
    # Determine how many levels there are for each factor and then
    # adding 1L to account for missing values (if necessary, i.e. if
    # at least one factor has missing values AND 'na.exclude' is FALSE) 
    numLevels <- sapply(X = x, FUN = function(x){length(levels(factor(x)))}) + (max(hasNA) * !na.exclude)
    
    # Format the results of using the table() function on each factor
    # to display the factor level name, frequency, and corresponding
    # percent frequency
    formattedList <- lapply(lapply(x, table, useNA = table.useNA), function(x){x <- if(na.exclude){unclass(x[!is.na(x)])} else {unclass(x)}; paste(names(x), " (", x, ", ", formatC(100*x/sum(x), digits = 1L, format = "f", width = 3L), "%)", sep = "")})
    
    # If any factors have missing values AND 'na.exclude' is FALSE,
    # then (for only those factors) include the first 'maxLevels'-1L
    # levels and append the "NA" level as well, with a warning that
    # this has happened
    if(any(numLevels > maxLevels)) {
        for(i in which(numLevels > maxLevels)) {
            if(max(hasNA) * !na.exclude) {
                formattedList[[i]] <- formattedList[[i]][c(seq_len(maxLevels - 1L), numLevels[i])]
            } else {
                formattedList[[i]] <- formattedList[[i]][seq_len(maxLevels)]
            }
        }
        warning(paste(strwrap(gettextf("At least one factor has too many levels to display. Only the first %d levels were included in the output. You can change this behavior by increasing the %s argument.", as.integer(maxLevels), sQuote("maxLevels")), width = 0.95 * getOption("width")), collapse = "\n    "))
    }
    # Number of levels for each item in the list (i.e. each factor)
    # after being formatted; should be an integer from 1 through
    # 'maxLevels'
    numFormattedLevels <- sapply(formattedList, FUN = length)
    
    # Greatest number of levels out of all items in 'formattedList',
    # i.e. the greatest number of elements out of all "formatted"
    # factors <=> the number of rows to account for in output DF
    maxFormattedLevels <- min(maxLevels, max(numFormattedLevels))
    
    # Create a CATegory LEVELS Data Frame; this will be rbind()ed
    # to the 'resultsDF' object (see below)
    catLevelsDF <- data.frame(mapply(FUN = function(x, sep, times){c(x, rep(paste(sep[1L]), times = times))}, x = formattedList, sep = emptyCellSymbol, times = maxFormattedLevels - numFormattedLevels), row.names = paste("Level ", seq_len(maxFormattedLevels), ":", sep = ""), check.names = FALSE, stringsAsFactors = FALSE)
    
    # Create a RESULTS Data Frame using the 2-/3-element list of
    # the counts of the data ("N", "Missing", "Unique")
    resultsDF <- data.frame(t(data.frame(results, check.names = FALSE, stringsAsFactors = FALSE)), check.names = FALSE, stringsAsFactors = FALSE)
    
    if(statsAreRows) {
        # rbind() the first results ('resultsDF') with the factor
        # levels frequencies ('catLevelsDF'), using the 'bufferRow'
        # as a visual separator between them
        resultsDF <- rbind(resultsDF, bufferRow, catLevelsDF)
        
        # Rename the row name for 'bufferRow' to display as blank
        rownames(resultsDF)[bufferRowIndex] <- ""
    } else {
        # Omit the buffer row if the stats are listed as columns
        resultsDF <- data.frame(cbind(t(resultsDF), t(catLevelsDF)), check.names = FALSE, stringsAsFactors = FALSE)
    }
    
    # Return the results for the categorical variables
    return(resultsDF)
}


# # Debugging values for the getResults.continuous() function
# x <- DFFH[, c("key", "age", "htcm", "wtkg")]
# requestedStats <- checkStats("default")
# na.rm <- TRUE
# silent <- FALSE
# digits <- 2L
# quantile.probs <- 0:4/4
# quantile.type <- 7L
# MFV.outputValue <- "minimum"
# statsAreRows <- TRUE
# # End debugging values for getResults.continuous() function

# Define the getResults.continuous() function
getResults.continuous <- function(x, requestedStats, na.rm = getOption("na.rm", default = FALSE), silent = FALSE, digits = 2L, quantile.probs = 0:4/4, quantile.type = 7L, MFV.outputValue = "minimum", statsAreRows = TRUE) {
    # Initialize an empty list object named "results"
    results <- list()
    
    # If "n" is requested
    if(requestedStats["n"]){
        results[["N"]] <- sapply(X = x, FUN = function(x){length(x) - sum(is.na(x))})
        results[["Missing"]] <- sapply(X = x, FUN = function(x){sum(is.na(x))})
        results[["Unique"]] <- sapply(X = x, FUN = function(x){length(unique(x[!is.na(x)]))})
    }
    
    # If "mean" is requested
    if(requestedStats["mean"]){
        results[["Mean"]] <- sapply(X = x, FUN = mean, na.rm = na.rm)
    }
    
    # If "sd" is requested
    if(requestedStats["sd"]){
        results[["Std. Dev."]] <- sapply(X = x, FUN = function(x){sqrt(var(x, na.rm = na.rm))})
    }
    
    # If "var" is requested
    if(requestedStats["variance"]){
        results[["Variance"]] <- sapply(X = x, FUN = var, na.rm = na.rm)
    }
    
    # If "mode" (i.e., the most frequent value) is requested
    if(requestedStats["mode"]){
        results[["Mode"]] <- sapply(X = x, FUN = MFV, outputValue = MFV.outputValue, na.rm = na.rm, silent = silent)
    }
    
    # If "min" is requested
    if(requestedStats["minimum"]){
        results[["Minimum"]] <- sapply(X = x, FUN = min, na.rm = na.rm)
    }
    
    # If "quartile1" is requested
    if(requestedStats["quartile1"]){
        results[["First Quartile"]] <- sapply(X = x, FUN = function(x){unname(quantile.datesOK(x = x, probs = 1/4, na.rm = na.rm, type = quantile.type))})
    }
    
    # If "median" is requested
    if(requestedStats["median"]){
        results[["Median"]] <- sapply(X = x, FUN = median, na.rm = na.rm)
    }
    
    # If "quartile3" is requested
    if(requestedStats["quartile3"]){
        results[["Third Quartile"]] <- sapply(X = x, FUN = function(x){unname(quantile.datesOK(x = x, probs = 3/4, na.rm = na.rm, type = quantile.type))})
    }
    
    # If "quartiles" is requested and "fivenum" is not
    if(requestedStats["quartiles"] && !requestedStats["fivenum"]){
        results[["First Quartile"]] <- sapply(X = x, FUN = function(x){unname(quantile.datesOK(x = x, probs = 1/4, na.rm = na.rm, type = quantile.type))})
        results[["Median"]] <- sapply(X = x, FUN = median, na.rm = na.rm)
        results[["Third Quartile"]] <- sapply(X = x, FUN = function(x){unname(quantile.datesOK(x = x, probs = 3/4, na.rm = na.rm, type = quantile.type))})
    }
    
    # If "max" is requested
    if(requestedStats["maximum"]){
        results[["Maximum"]] <- sapply(X = x, FUN = max, na.rm = na.rm)
    }
    
    # If "range" is requested
    if(requestedStats["range"]){
        results[["Range"]] <- sapply(X = x, FUN = function(x){diff(range(x, na.rm = na.rm))})
    }
    
    # If "iqr" is requested
    if(requestedStats["iqr"]){
        results[["IQR"]] <- sapply(X = x, FUN = IQR, na.rm = na.rm, type = quantile.type)
    }
    
    # If "fivenum" is requested
    if(requestedStats["fivenum"]) {
#         results.fivenum <- sapply(X = x, FUN = fivenum, na.rm = na.rm)
        results.fivenum <- sapply(X = x, FUN = fivenum.datesOK, na.rm = na.rm)
        
        # If neither "quartiles" nor "summary" are requested
        if(!(requestedStats["quartiles"] || requestedStats["summary"])) {
            results[["Minimum"]] <- results.fivenum[1L, ]
            results[["Lower Hinge"]] <- results.fivenum[2L, ]
            results[["Median"]] <- results.fivenum[3L, ]
            results[["Upper Hinge"]] <- results.fivenum[4L, ]
            results[["Maximum"]] <- results.fivenum[5L, ]
        } else
        # If "quartiles" *is* requested
            if(requestedStats["quartiles"]) {
            results[["Minimum"]] <- results.fivenum[1L, ]
            results[["First Quartile"]] <- sapply(X = x, FUN = function(x){unname(quantile.datesOK(x, probs = 1/4, na.rm = na.rm, type = quantile.type))})
            results[["Lower Hinge"]] <- results.fivenum[2L, ]
            results[["Median"]] <- results.fivenum[3L, ]
            results[["Third Quartile"]] <- sapply(X = x, FUN = function(x){unname(quantile.datesOK(x, probs = 3/4, na.rm = na.rm, type = quantile.type))})
            results[["Upper Hinge"]] <- results.fivenum[4L, ]
            results[["Maximum"]] <- results.fivenum[5L, ]
        }
    }
    
    # If "summary" is requested
    if(requestedStats["summary"]){
        variableHasMissing <- sapply(X = x, FUN = anyNA)
        if(any(variableHasMissing) && !all(variableHasMissing)) {
            missingNo  <- rbind(sapply(X = x[, !variableHasMissing, drop = FALSE], FUN = summary), "NA's" = 0L)
            missingYes <- sapply(X = x[,  variableHasMissing, drop = FALSE], FUN = summary)
            results.summary <- cbind(missingNo, missingYes)[, colnames(x), drop = FALSE]
        } else {
            results.summary <- sapply(X = x, FUN = summary)
        }
        if(requestedStats["fivenum"]) {
            results[["Minimum"]] <- results.summary[1L, ]
            results[["First Quartile"]] <- results.summary[2L, ]
            results[["Lower Hinge"]] <- results.fivenum[2L, ]
            results[["Median"]] <- results.summary[3L, ]
            results[["Mean"]] <- results.summary[4L, ]
            results[["Third Quartile"]] <- results.summary[5L, ]
            results[["Upper Hinge"]] <- results.fivenum[4L, ]
            results[["Maximum"]] <- results.summary[6L, ]
        } else {
            results[["Minimum"]] <- results.summary[1L, ]
            results[["First Quartile"]] <- results.summary[2L, ]
            results[["Median"]] <- results.summary[3L, ]
            results[["Mean"]] <- results.summary[4L, ]
            results[["Third Quartile"]] <- results.summary[5L, ]
            results[["Maximum"]] <- results.summary[6L, ]
        }
        if(any(variableHasMissing)) {
            results[["NA's"]] <- as.integer(results.summary[7L, ])
        }
    }
    
    # If "quantiles" is requested
    if(requestedStats["quantiles"]){
        results[["Quantiles"]] <- rep("", times = ncol(x))
        results.quantiles <- sapply(X = x, FUN = quantile.datesOK, probs = quantile.probs, na.rm = na.rm, type = quantile.type)
        results.quantiles.dn <- dimnames(results.quantiles)
        results.quantiles <- split(results.quantiles, seq_len(nrow(results.quantiles)))
        names(results.quantiles) <- results.quantiles.dn[[1L]]
        results <- c(results, lapply(results.quantiles, `names<-`, results.quantiles.dn[[2L]]))
    }
    
    # Pretty up the results if requested
    if(!is.null(digits)){
        digits <- as.integer(max(digits[1L], 0L))
        results <- lapply(X = results, FUN = function(x){
            if(is.double(x)) {
                formatC(x, format = "f", digits = digits)
            } else if(!is.integer(x)) {
                as.character(x)
            } else {
                x
            }
        })
    }
    
    if(statsAreRows) {
        # Convert list to data frame
        resultsDF <- data.frame(t(data.frame(results, check.names = FALSE, stringsAsFactors = FALSE)), check.names = FALSE, stringsAsFactors = FALSE)
    } else {
        results[["Quantiles"]] <- NULL
        resultsDF <- data.frame(results, check.names = FALSE, stringsAsFactors = FALSE)
    }
    
    # Return the 'resultsDF' data frame
    return(resultsDF)
}


# Define the getResults.byFactors() function
# getResults.byFactors <- function(x.byFactors, x.continuous, ...) {
getResults.byFactors <- function(x.byFactors, x.continuous, requestedStats, na.rm = TRUE, silent = FALSE, digits = 2L, quantile.probs = 0:4/4, quantile.type = 7L, MFV.outputValue = "minimum", statsAreRows = TRUE) {
    if(!is.data.frame(x.byFactors) || !is.data.frame(x.continuous)) {
        x.byFactors <- as.data.frame(x.byFactors, optional = TRUE, stringsAsFactors = TRUE)
        x.continuous <- as.data.frame(x.continuous, optional = TRUE)
    }
#     # NB: Make sure to specify the first argument as:
#     #     x.byFactors <- x[, byFactors, drop = FALSE]
#     # OR
#     #     x.byFactors <- x.categorical[, byFactors, drop = FALSE]
#     allCombinations <- levels(interaction(x.byFactors, sep = ","))
#     
#     factorLevels <- lapply(x.byFactors, levels)
#     byFactorNames <- colnames(x.byFactors)
#     
#     # Initialize an empty list object named "results"
#     results <- list()
#     
#     # If "n" is requested
#     if(requestedStats["n"]){
# #         results[["N"]] <- sapply(X = x, FUN = function(x){length(x) - sum(is.na(x))})
# #         results[["Missing"]] <- sapply(X = x, FUN = function(x){sum(is.na(x))})
# #         results[["Unique"]] <- sapply(X = x, FUN = function(x){length(unique(x[!is.na(x)]))})
#         results[["N"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = function(x){length(x) - sum(is.na(x))})
#         results[["Missing"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = function(x){sum(is.na(x))})
#         results[["Unique"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = function(x){length(unique(x[!is.na(x)]))})
#     }
#     
#     # If "mean" is requested
#     if(requestedStats["mean"]){
# #         results[["Mean"]] <- sapply(X = x, FUN = mean, na.rm = na.rm)
#         results[["Mean"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = mean, na.rm = na.rm)
#     }
#     
#     # If "sd" is requested
#     if(requestedStats["sd"]){
# #         results[["Std. Dev."]] <- sapply(X = x, FUN = function(x){sqrt(var(x, na.rm = na.rm))})
#         results[["Std. Dev."]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = sd, na.rm = na.rm)
#     }
#     
#     # If "var" is requested
#     if(requestedStats["variance"]){
# #         results[["Variance"]] <- sapply(X = x, FUN = var, na.rm = na.rm)
#         results[["Variance"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = var, na.rm = na.rm)
#     }
#     
#     # If "mode" (i.e., the most frequent value) is requested
#     if(requestedStats["mode"]){
# #         results[["Mode"]] <- sapply(X = x, FUN = MFV, outputValue = MFV.outputValue, na.rm = na.rm, silent = silent)
#         results[["Mode"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = MFV, outputValue = MFV.outputValue, na.rm = na.rm, silent = silent)
#     }
#     
#     # If "min" is requested
#     if(requestedStats["minimum"]){
# #         results[["Minimum"]] <- sapply(X = x, FUN = min, na.rm = na.rm)
#         results[["Minimum"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = min, na.rm = na.rm)
#     }
#     
#     # If "quartile1" is requested
#     if(requestedStats["quartile1"]){
# #DEL #         results[["First Quartile"]] <- sapply(X = x, FUN = function(x){unname(quantile.datesOK(x = x, probs = 1/4, na.rm = na.rm, type = quantile.type))})
# #KEEP#         results[["First Quartile"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = function(x){unname(quantile.datesOK(x = x, probs = 1/4, na.rm = na.rm, type = quantile.type))})
#         results[["First Quartile"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = function(x){unname(quantile(x = x, probs = 1/4, na.rm = na.rm, type = quantile.type))})
#     }
#     
#     # If "median" is requested
#     if(requestedStats["median"]){
# #         results[["Median"]] <- sapply(X = x, FUN = median, na.rm = na.rm)
#         results[["Median"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = median, na.rm = na.rm)
#     }
#     
#     # If "quartile3" is requested
#     if(requestedStats["quartile3"]){
# #DEL #         results[["Third Quartile"]] <- sapply(X = x, FUN = function(x){unname(quantile.datesOK(x = x, probs = 3/4, na.rm = na.rm, type = quantile.type))})
# #KEEP#         results[["Third Quartile"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = function(x){unname(quantile.datesOK(x = x, probs = 3/4, na.rm = na.rm, type = quantile.type))})
#         results[["Third Quartile"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = function(x){unname(quantile(x = x, probs = 3/4, na.rm = na.rm, type = quantile.type))})
#     }
#     
#     # If "quartiles" is requested and "fivenum" is not
#     if(requestedStats["quartiles"] && !requestedStats["fivenum"]){
# #DEL #         results[["First Quartile"]] <- sapply(X = x, FUN = function(x){unname(quantile.datesOK(x = x, probs = 1/4, na.rm = na.rm, type = quantile.type))})
# #DEL #         results[["Median"]] <- sapply(X = x, FUN = median, na.rm = na.rm)
# #DEL #         results[["Third Quartile"]] <- sapply(X = x, FUN = function(x){unname(quantile.datesOK(x = x, probs = 3/4, na.rm = na.rm, type = quantile.type))})
# #KEEP#         results[["First Quartile"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = function(x){unname(quantile.datesOK(x = x, probs = 1/4, na.rm = na.rm, type = quantile.type))})
#         results[["First Quartile"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = function(x){unname(quantile(x = x, probs = 1/4, na.rm = na.rm, type = quantile.type))})
#         results[["Median"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = median, na.rm = na.rm)
# #KEEP#         results[["Third Quartile"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = function(x){unname(quantile.datesOK(x = x, probs = 3/4, na.rm = na.rm, type = quantile.type))})
#         results[["Third Quartile"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = function(x){unname(quantile(x = x, probs = 3/4, na.rm = na.rm, type = quantile.type))})
#     }
#     
#     # If "max" is requested
#     if(requestedStats["maximum"]){
# #         results[["Maximum"]] <- sapply(X = x, FUN = max, na.rm = na.rm)
#         results[["Maximum"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = max, na.rm = na.rm)
#     }
#     
#     # If "range" is requested
#     if(requestedStats["range"]){
# #         results[["Range"]] <- sapply(X = x, FUN = function(x){diff(range(x, na.rm = na.rm))})
#         results[["Range"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = function(x){diff(range(x, na.rm = na.rm))})
#     }
#     
#     # If "iqr" is requested
#     if(requestedStats["iqr"]){
# #         results[["IQR"]] <- sapply(X = x, FUN = IQR, na.rm = na.rm, type = quantile.type)
#         results[["IQR"]] <- aggregate(x = x.continuous, by = x.byFactors, FUN = IQR, na.rm = na.rm, type = quantile.type)
#     }
#     
#     # If "fivenum" is requested
#     if(requestedStats["fivenum"]) {
# #DEL #         results.fivenum <- sapply(X = x, FUN = fivenum.datesOK, na.rm = na.rm)
# #KEEP#         results.fivenum <- aggregate(x = x.continuous, by = x.byFactors, FUN = fivenum, na.rm = na.rm)
#         results.fivenum <- aggregate(x = x.continuous, by = x.byFactors, FUN = fivenum.datesOK, na.rm = na.rm)
#         
#         while(length(results.fivenum) > NCOL(x.continuous)) {
#             results.fivenum[[1L]] <- NULL
#         }
#         
#         reslist <- list()
#         for(i in seq_along(results.fivenum)) {
#             reslist[[i]] <- rbind(rboundDF, results.fivenum[[i]])
#         }
#         
#         # If neither "quartiles" nor "summary" are requested
#         if(!(requestedStats["quartiles"] || requestedStats["summary"])) {
#             results[["Minimum"]] <- results.fivenum[1L, ]
#             results[["Lower Hinge"]] <- results.fivenum[2L, ]
#             results[["Median"]] <- results.fivenum[3L, ]
#             results[["Upper Hinge"]] <- results.fivenum[4L, ]
#             results[["Maximum"]] <- results.fivenum[5L, ]
#         } else
#         # If "quartiles" *is* requested
#             if(requestedStats["quartiles"]) {
#             results[["Minimum"]] <- results.fivenum[1L, ]
#             results[["First Quartile"]] <- sapply(X = x, FUN = function(x){unname(quantile.datesOK(x, probs = 1/4, na.rm = na.rm, type = quantile.type))})
#             results[["Lower Hinge"]] <- results.fivenum[2L, ]
#             results[["Median"]] <- results.fivenum[3L, ]
#             results[["Third Quartile"]] <- sapply(X = x, FUN = function(x){unname(quantile.datesOK(x, probs = 3/4, na.rm = na.rm, type = quantile.type))})
#             results[["Upper Hinge"]] <- results.fivenum[4L, ]
#             results[["Maximum"]] <- results.fivenum[5L, ]
#         }
#     }
#     
#     # If "summary" is requested
#     if(requestedStats["summary"]){
#         variableHasMissing <- sapply(X = x, FUN = anyNA)
#         if(any(variableHasMissing) && !all(variableHasMissing)) {
#             missingNo  <- rbind(sapply(X = x[, !variableHasMissing, drop = FALSE], FUN = summary), "NA's" = 0L)
#             missingYes <- sapply(X = x[,  variableHasMissing, drop = FALSE], FUN = summary)
#             results.summary <- cbind(missingNo, missingYes)[, colnames(x), drop = FALSE]
#         } else {
#             results.summary <- sapply(X = x, FUN = summary)
#         }
#         if(requestedStats["fivenum"]) {
#             results[["Minimum"]] <- results.summary[1L, ]
#             results[["First Quartile"]] <- results.summary[2L, ]
#             results[["Lower Hinge"]] <- results.fivenum[2L, ]
#             results[["Median"]] <- results.summary[3L, ]
#             results[["Mean"]] <- results.summary[4L, ]
#             results[["Third Quartile"]] <- results.summary[5L, ]
#             results[["Upper Hinge"]] <- results.fivenum[4L, ]
#             results[["Maximum"]] <- results.summary[6L, ]
#         } else {
#             results[["Minimum"]] <- results.summary[1L, ]
#             results[["First Quartile"]] <- results.summary[2L, ]
#             results[["Median"]] <- results.summary[3L, ]
#             results[["Mean"]] <- results.summary[4L, ]
#             results[["Third Quartile"]] <- results.summary[5L, ]
#             results[["Maximum"]] <- results.summary[6L, ]
#         }
#         if(any(variableHasMissing)) {
#             results[["NA's"]] <- as.integer(results.summary[7L, ])
#         }
#     }
#     
#     # If "quantiles" is requested
#     if(requestedStats["quantiles"]){
#         results[["Quantiles"]] <- rep("", times = ncol(x))
#         results.quantiles <- sapply(X = x, FUN = quantile.datesOK, probs = quantile.probs, na.rm = na.rm, type = quantile.type)
#         results.quantiles.dn <- dimnames(results.quantiles)
#         results.quantiles <- split(results.quantiles, seq_len(nrow(results.quantiles)))
#         names(results.quantiles) <- results.quantiles.dn[[1L]]
#         results <- c(results, lapply(results.quantiles, `names<-`, results.quantiles.dn[[2L]]))
#     }
#     
#     # Pretty up the results if requested
#     if(!is.null(digits)){
#         digits <- as.integer(max(digits[1L], 0L))
#         results <- lapply(X = results, FUN = function(x){
#             if(is.double(x)) {
#                 formatC(x, format = "f", digits = digits)
#             } else if(!is.integer(x)) {
#                 as.character(x)
#             } else {
#                 x
#             }
#         })
#     }
#     
#     if(statsAreRows) {
#         # Convert list to data frame
#         resultsDF <- data.frame(t(data.frame(results, check.names = FALSE, stringsAsFactors = FALSE)), check.names = FALSE, stringsAsFactors = FALSE)
#     } else {
#         results[["Quantiles"]] <- NULL
#         resultsDF <- data.frame(results, check.names = FALSE, stringsAsFactors = FALSE)
#     }
#     
#     # Return the 'resultsDF' data frame
#     return(resultsDF)
# }
    tempres <- vector(mode = "list", length = NCOL(x.continuous))
    names(tempres) <- colnames(x.continuous)
    allCombinations <- levels(interaction(x.byFactor, sep = ","))
    for (i in seq_along())
}


# # Values used for debugging the descriptiveStatsDF() function
# x <- DFFH
# dataObjectName <- "DFFH"
# stats <- "default"
# requestedStats <- checkStats(stats)
# columns <- "all"
# digits <- 2L
# na.rm <- TRUE
# silent <- FALSE
# quantile.probs <- 0:4/4
# quantile.type <- 7L
# keepColumnNames <- TRUE
# categorical.emptyCellSymbol <- ""
# categorical.maxLevels <- 10L
# categorical.na.exclude <- na.rm
# output.showStats <- "all"
# # byFactors <- NULL
# byFactors <- c("sex", "doc")
# # ignore <- NULL
# ignore <- "key"
# output.statsAreRows <- TRUE
# # End debugging values for descriptiveStatsDF()

# Define the descriptiveStatsDF() function
descriptiveStatsDF <- function(x, stats = "default", columns = "all", digits = 2L, na.rm = TRUE, silent = FALSE, quantile.probs = 0:4/4, quantile.type = 7L, keepColumnNames = TRUE, categorical.emptyCellSymbol = "", categorical.maxLevels = 10L, categorical.na.exclude = na.rm, output.showStats = "all", byFactors = NULL, ignore = NULL, output.statsAreRows = TRUE) {
    if(silent) {
        oldWarn <- getOption("warn")
        options("warn" = -1L)
        on.exit(options("warn" = oldWarn), add = TRUE)
    }
    
    dataObjectName <- deparse(substitute(x))
    if(grepl(pattern = "\\[|\\$", x = dataObjectName)) {
        if(grepl(pattern = "-|!", x = dataObjectName)) {
            stop(paste(strwrap(gettextf("Please use the %s argument to specify columns you do not want to include in the results.", sQuote("ignore")), width = 0.95 * getOption("width")), collapse = "\n    "))
        }
        unbracket <- function(x) {
            y <- unlist(strsplit(x = x, split = "\\[|\\]|\\$"))
            object <- y[1L]
            y <- y[-1L]
            y <- unlist(strsplit(x = y, split = ",| |c|\\(|\\)"))
            y <- unlist(strsplit(x = y, split = "\\'"))
            y <- unlist(strsplit(x = y, split = '\\"'))
            return(list("name" = object,
                        "indices" = type.convert(y[nzchar(y)], as.is = TRUE)))
        }
        x <- data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
        bracketfree <- unbracket(x = dataObjectName)
        dataObjectName <- bracketfree[["name"]]
        columns <- bracketfree[["indices"]]
        if(is.integer(bracketfree[["indices"]])) {
            colnames(x) <- columns
            oldColumnNames <- paste("Column ", columns, sep = "")
            on.exit(warning(paste(strwrap(gettext("NOTE: The data set provided to this function was a subset of a larger data set and the columns (i.e., the variables) were specified by numeric index. Because of this, the column names in the resulting output may not accurately correspond to those in the original data set. This can be fixed by using the 'columns' argument and/or named columns (if possible)."), width = 0.95 * getOption("width")), collapse = "\n    "), call. = FALSE), add = TRUE)
        } else {
            oldColumnNames <- colnames(x) <- columns
        }
    }
    
    digits <- min(as.integer(abs(digits[1L])), 22L)
    
    requestedStats <- checkStats(stats = as.character(stats), dataObjectName = dataObjectName)
    
    output.showStats <- unique(match.arg(arg = tolower(output.showStats), choices = c("all", "categorical", "continuous", "byfactors", "bylevels", "none"), several.ok = TRUE))
    
    if(is.data.frame(x)) {
        whatWasIt <- "data.frame"
        oldColumnNames <- dimnames(x)[[2L]]
        oldColumnIndices <- seq_len(ncol(x))
    } else if(is.list(x)) {
        whatWasIt <- "list"
        oldColumnNames <- names(x)[names(x) != ""]
        oldColumnIndices <- seq_along(x)
    } else if(is.matrix(x)) {
        whatWasIt <- "matrix"
        oldColumnNames <- dimnames(x)[[2L]]
        oldColumnIndices <- seq_len(ncol(x))
    } else if(is.array(x)) {
        whatWasIt <- "array"
        oldColumnNames <- dimnames(data.frame(x, check.names = FALSE))[[2L]]
        oldColumnIndices <- seq_len(ncol(data.frame(x)))
    } else {
        whatWasIt <- "vector"
        if(!exists("oldColumnNames")) {
            oldColumnNames <- dataObjectName
        }
        oldColumnIndices <- 1L
    }
    
    check.names <- (length(oldColumnNames) < 1L)
    x <- data.frame(x, check.names = check.names, stringsAsFactors = TRUE)
    if(whatWasIt == "vector") {
        colnames(x) <- dataObjectName
    }
    
    if(length(byFactors) > 0L && mode(columns) != mode(byFactors)) {
        warning(paste(strwrap(gettextf("The %s argument was of mode %s whereas the %s argument was of mode %s. The %s argument was set to NULL.", sQuote("columns"), mode(columns), sQuote("byFactors"), mode(byFactors), sQuote("byFactors")), width = 0.95 * getOption("width")), collapse = "\n    "))
        byFactors <- NULL
    }
    columns <- unique(c(columns, byFactors))
    
    checkedColumns <- checkColumns(x = x, columns = columns, dataObjectName = dataObjectName, keepColumnNames = keepColumnNames, ignore = ignore)
    byFactors <- intersect(checkedColumns[["validColumns"]], byFactors)
    
    x <- x[, checkedColumns[["validColumns"]], drop = FALSE]
    
    
    if(keepColumnNames) {
        colnames(x) <- checkedColumns[["validColumnNames"]]
    } else {
        if(length(byFactors) > 0L) {
            byFactors <- paste("Column ", which(colnames(x) %in% byFactors), sep = "")
        }
        if(all(checkedColumns[["validColumns"]] %in% oldColumnNames)) {
            colnames(x) <- paste("Column ", match(checkedColumns[["validColumns"]], oldColumnNames), sep = "")
        } else {
            colnames(x) <- checkedColumns[["validColumnNames"]]
        }
    }
    
    varTypes <- sapply(X = x, FUN = variableType)
    
    invalidVarTypes <- names(varTypes[which(is.na(varTypes))])
#     invalidVarTypes <- names(varTypes[which(is.na(varTypes) | varTypes == "date")])
    if(length(invalidVarTypes) > 0L) {
        warning(ngettext(n = length(invalidVarTypes),
                         msg1 = paste(strwrap(gettextf("The following variable was left out of the results either because it was not of a recognized variable type or because its type is not currently supported for analysis: %s.", sQuote(invalidVarTypes)), width = 0.95 * getOption("width")), collapse = "\n    "),
                         msg2 = paste(strwrap(gettextf("The following variables were left out of the results either because they were not of recognized variable types or because their types are not currently supported for analysis: %s.", paste(sQuote(invalidVarTypes), collapse = ", ")), width = 0.95 * getOption("width")), collapse = "\n    ")))
    }
    x <- x[, !is.na(varTypes), drop = FALSE]
#     varTypes <- sapply(X = x, FUN = variableType)
#     
#     if(any("categorical", "continuous", "binary", "date") %in% ignore) {
#         if(all("categorical", "continuous", "binary", "date") %in% ignore) {
#             stop("If you want me to ignore everything, then what should I report?")
#         }
#         shouldKeep <- varTypes != ""
#         if("categorical" %in% ignore) {
# #             x <- x[, varTypes != "categorical", drop = FALSE]
#             shouldKeep <- shouldKeep & varTypes != "categorical"
#         }
#         if("continuous" %in% ignore) {
# #             x <- x[, varTypes != "continuous", drop = FALSE]
#             shouldKeep <- shouldKeep & varTypes != "continuous"
#         }
#         if("binary" %in% ignore) {
# #             x <- x[, varTypes != "binary", drop = FALSE]
#             shouldKeep <- shouldKeep & varTypes != "binary"
#         }
#         if("date" %in% ignore) {
# #             x <- x[, varTypes != "date", drop = FALSE]
#             shouldKeep <- shouldKeep & varTypes != "date"
#         }
#         x <- x[, shouldKeep, drop = FALSE]
#     }
    
    x.categorical <- x[, (varTypes %in% c("categorical", "binary")), drop = FALSE]
    x.continuous  <- x[, (varTypes %in% c("continuous", "date")),  drop = FALSE]
    
    byFactors <- intersect(byFactors, colnames(x.categorical))
    
    # Initialize the 'results' list
    results <- list()
    
    # Get categorical results:
    # - Don't need 'columns' argument (already subset from 'x')
    # - Don't need 'stats' argument (always the same for categorical)
    if(all(dim(x.categorical) > 0L) && any(output.showStats %in% c("all", "categorical")) && !("categorical" %in% ignore)) {
        results[["Categorical"]] <- getResults.categorical(x = x.categorical, na.rm = na.rm, emptyCellSymbol = categorical.emptyCellSymbol, maxLevels = categorical.maxLevels, na.exclude = categorical.na.exclude, statsAreRows = output.statsAreRows)
    }
    
    # Get continuous results:
    # - Don't need 'columns' argument (already subset from 'x')
    # - Do need 'stats' argument
    #    * Also need 'quantile.probs' and 'quantile.type' arguments
    if(all(dim(x.continuous) > 0L) && any(output.showStats %in% c("all", "continuous")) && !("continuous" %in% ignore)) {
        results[["Continuous"]] <- getResults.continuous(x = x.continuous, requestedStats = requestedStats, na.rm = na.rm, silent = silent, digits = digits, quantile.probs = quantile.probs, quantile.type = quantile.type, statsAreRows = output.statsAreRows)
    }
    
    # Get subset results
    if(length(byFactors) > 0L && any(output.showStats %in% c("all", "byfactors", "bylevels"))) {
#         results[["By Levels"]] <- by(data = x, INDICES = x[, byFactors, drop = FALSE], FUN = summary)
#         results[["By Factors"]] <- by(data = x, INDICES = x[, byFactors, drop = FALSE], FUN = summary)
        results[["By Factors"]] <- getResults.byFactors(x.continuous = x.continuous, x.byFactors = x.categorical[, c(byFactors), drop = FALSE], requestedStats = requestedStats, na.rm = na.rm, silent = silent, digits = digits, quantile.probs = quantile.probs, quantile.type = quantile.type, statsAreRows = output.statsAreRows)
    }
    
    # Return the list 'results'
    if("none" %in% output.showStats) {
        return(invisible())
    } else {
        return(results)
    }
}









### SANDBOX ###
# # Testing some stuff
# funs <- c("mean", "sd", "median", "quantile.datesOK")
# set.seed(0711)
# dataset <- as.data.frame(matrix(rnorm(4000), nrow = 1000))
# dataset <- DFFH[, c("key", "age", "doa", "htcm", "wtkg")]
# 
# sapply(X = dataset, FUN = get(funs))
# sapply(X = funs, FUN = get)
# 
# results <- list()
# for(i in seq_along(funs)) {
#     results[[i]] <- sapply(X = dataset, FUN = sapply(X = funs, FUN = get)[[i]])
# }
# names(results) <- funs
# 
# temp <- DFFH[, c("key", "age", "doa", "htcm", "wtkg")]
# funs <- c("mean", "sd", "median")
# results <- list()
# for(i in seq_along(funs)) {
# #     results[[i]] <- as.data.frame(sapply(X = temp, FUN = sapply(X = funs, FUN = get)[[i]]))
#     results[[i]] <- data.frame(lapply(X = temp, FUN = sapply(X = funs, FUN = get)[[i]]))
# #     results[[i]] <- lapply(X = temp, FUN = sapply(X = funs, FUN = get)[[i]])
# }
# results

# temp <- descriptiveStatsDF(x = DFFH, by = c("sex", "doc"))
# write.table(x = DFFH,
#           file = "C:/Users/Ted/Desktop/Summer2014/CRTP/RWork/Module02-Merge/DFFH.csv",
#           sep = ",",
#           row.names = FALSE,
#           col.names = TRUE)

# # unquote <- function(x) {
# #     temp <- unlist(strsplit(x = x, split = "\\'"))
# #     temp <- unlist(strsplit(x = temp, split = '\\"'))
# #     temp[nzchar(temp)]
# # }
# # 
# # unindex <- function(x) {
# #     temp <- unlist(strsplit(x = x, split = "\\[|\\]"))
# #     object <- temp[1L]
# #     temp <- temp[-1L]
# #     temp <- unlist(strsplit(x = temp, split = ",| |c|\\(|\\)"))
# #     temp <- unquote(temp)
# #     return(list("name" = object, "indices" = temp))
# # }
# unbracket <- function(x) {
#     y <- unlist(strsplit(x = x, split = "\\[|\\]|\\$"))
#     object <- y[1L]
#     y <- y[-1L]
#     y <- unlist(strsplit(x = y, split = ",| |c|\\(|\\)"))
#     y <- unlist(strsplit(x = y, split = "\\'"))
#     y <- unlist(strsplit(x = y, split = '\\"'))
# #     return(list("name" = object, "indices" = y[nzchar(y)]))
#     return(list("name" = object,
#                 "indices" = type.convert(y[nzchar(y)], as.is = TRUE)))
# }




