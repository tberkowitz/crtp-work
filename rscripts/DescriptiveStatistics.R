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
# source(paste(getwd(), "FakeData.R", sep = "/"))

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
        warning(strwrap("There were no duplicate values in the data. Result is the first data value.", width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""))
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
        warning(strwrap(gettextf("All data values occur with frequency %1d. Only the first value will be returned.", as.integer(unique(freqs))), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""))
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
    lowStats <- gsub(pattern = "[[:punct:]]", replacement = "", x = tolower(stats))
    
    # List out all of the options for "stats" that will be
    # accepted/recognized by the function
    possibleStats <- c("default", "all", "n", "observations", "nobservations", "missing", "nmissing", "na", "mean", "average", "avg", "median", "mode", "sd", "standard deviation", "variance", "iqr", "minimum", "maximum", "fivenum", "interquartile range", "range", "std", "std dev", "mfv", "q1", "quartile1", "q3", "quartile3", "quartiles", "quantiles", "q2", "quartile2", "summary", "hinges", "lower hinge", "lowerhinge", "lhinge", "upper hinge", "upperhinge", "uhinge", "unique", "nunique", "mcv")
    if(!any(pmatch(x = lowStats, table = possibleStats, nomatch = 0L) > 0L)) {
        stop(strwrap(gettextf("I need at least one valid statistic to be specified before I can do anything with %s. Please check for spelling errors.", sQuote(dataObjectName)), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""))
    }
    validStats <- match.arg(arg = lowStats, choices = possibleStats, several.ok = TRUE)
    if(length(lowStats) != length(validStats)) {
        # Use "stats" instead of "lowStats" so the printed
        # values resemble the original user input, not the
        # tolower()ed argument(s)
        invalidStats <- stats[-match(x = possibleStats, table = lowStats, nomatch = 0L)]
        warning(strwrap(gettextf("At least one specified statistic was invalid. Only recognized statistics were used for the results. I did not recognize: %s.", paste(invalidStats, collapse = ", ")), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""))
    }
    
    # Check which statistics options selected
    requestedStatsNames <- c("n", "mean", "median", "mode", "sd", "variance", "minimum", "maximum", "range", "iqr", "lowerhinge", "upperhinge", "quartile1", "quartile3", "quantiles")
    requestedStats <- rep(FALSE, times = length(requestedStatsNames))
    names(requestedStats) <- requestedStatsNames
    
    # Check for "n"
    requestedStats["n"] <- any(c("n", "observations", "nobservations", "missing", "nmissing", "na", "unique", "nunique", "default", "all") %in% validStats)
    
    # Check for "mean"
    requestedStats["mean"] <- any(c("mean", "average", "avg", "summary", "default", "all") %in% validStats)
    
    # Check for "median"
    requestedStats["median"] <- any(c("median", "q2", "quartile2", "quartiles", "fivenum", "summary", "default", "all") %in% validStats)
    
    # Check for "mode"
    requestedStats["mode"] <- any(c("mode", "mfv", "mcv", "all") %in% validStats)
    
    # Check for "sd"
    requestedStats["sd"] <- any(c("sd", "standard deviation", "std", "std dev", "default", "all") %in% validStats)
    
    # Check for "variance"
    requestedStats["variance"] <- any(c("variance", "all") %in% validStats)
    
    # Check for "minimum"
    requestedStats["minimum"] <- any(c("minimum", "fivenum", "summary", "default", "all") %in% validStats)
    
    # Check for "maximum"
    requestedStats["maximum"] <- any(c("maximum", "fivenum", "summary", "default", "all") %in% validStats)
    
    # Check for "range"
    requestedStats["range"] <- any(c("range", "all") %in% validStats)
    
    # Check for "iqr"
    requestedStats["iqr"] <- any(c("iqr", "interquartile range", "all") %in% validStats)
    
    # Check for "lowerhinge"
    requestedStats["lowerhinge"] <- any(c("lower hinge", "lowerhinge", "lhinge", "hinges", "fivenum", "all") %in% validStats)
    
    # Check for "upperhinge"
    requestedStats["upperhinge"] <- any(c("upper hinge", "upperhinge", "uhinge", "hinges", "fivenum", "all") %in% validStats)
    
    # Check for "quartile1"
    requestedStats["quartile1"] <- any(c("q1", "quartile1", "quartiles", "summary", "all") %in% validStats)
    
    # Check for "quartile3"
    requestedStats["quartile3"] <- any(c("q3", "quartile3", "quartiles", "summary", "all") %in% validStats)
    
    # Check for "quantiles"
    requestedStats["quantiles"] <- any(c("quantiles", "all") %in% validStats)
    
    return(requestedStats)
}

# Define the variableType() function - check if continuous/categorical/binary/etc.
variableType <- function(x, NAIsError = FALSE) {
    x <- x[!is.na(x)]
    if(length(x) < 1L) {
        if(NAIsError) {
            stop(strwrap(gettextf("The variable does not have enough non-missing values for me to determine its type."), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""))
        } else {
            warning(strwrap(gettextf("The variable does not have enough non-missing values for me to determine its type."), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""))
            return(NA_character_)
        }
    }
    if((length(unique(x)) == 2L && length(x) > 2L && !(is.factor(x) || is.character(x))) || is.logical(x)) {
        "binary"
    } else if(any(class(x) %in% c("Date", "POSIXt", "POSIXct", "POSIXlt"))) {
        "date"
    } else if(is.character(x) || is.factor(x) || (is.integer(x) && length(unique(x)) <= 10L)) {
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
#     quantile(x, probs = probs, type = type, ...)
    stats::quantile(x, probs = probs, type = type, ...)
}

# Define the fivenum.datesOK() function
fivenum.datesOK <- function(x, na.rm = TRUE) {
    y <- if(isDateVar <- any(class(x) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt"))) {
        unclass(x)
    } else {
        x
    }
#     results.fivenum.datesOK <- fivenum(x = y, na.rm = na.rm)
    results.fivenum.datesOK <- stats::fivenum(x = y, na.rm = na.rm)
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
                    stop(strwrap(gettextf("None of the specified columns exists in the data set %s.", sQuote(dataObjectName)), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""))
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
                                msg1 = strwrap(gettextf("The specified column does not exist in the data set %s.", sQuote(dataObjectName)), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""),
                                msg2 = strwrap(gettextf("None of the specified columns exists in the data set %s.", sQuote(dataObjectName)), width = 0.95 * getOption("width"), prefix = "\n    ", initial = "")))
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
    
    # Guarantee 'x' is a data frame
    x <- data.frame(x, check.names = FALSE, stringsAsFactors = TRUE)
    
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
    
    # Determine the most frequently occurring factor level for each
    # variable
    modeLevels <- sapply(X = x, FUN = function(y){sort(table(y, useNA = table.useNA), decreasing = TRUE)[1L]})
    newNames <- names(modeLevels)
    for(i in seq_along(newNames)) {
        newNames[i] <- gsub(pattern = paste(names(x)[i], ".", sep = ""), replacement = "", x = names(modeLevels)[i])
    }
    results[["MFV"]] <- paste(newNames, " (", modeLevels, ", ", formatC(100*modeLevels/results[["N"]], digits = 1L, format = "f"), "%)", sep = "")
    
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
        warning(strwrap(gettextf("At least one factor has too many levels to display. Only the first %d levels were included in the output. You can change this behavior by increasing the %s argument.", as.integer(maxLevels), sQuote("maxLevels")), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""))
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
    
    # Check if anything is needed from the fivenum() function and act
    # accordingly
    if(any(requestedStats[c("lowerhinge", "upperhinge")])) {
        results.fivenum <- sapply(X = x, FUN = fivenum.datesOK, na.rm = na.rm)
    }
    
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
#         results[["Std. Dev."]] <- sapply(X = x, FUN = function(x){sqrt(var(x, na.rm = na.rm))})
        results[["Std. Dev."]] <- sapply(X = x, FUN = stats::sd, na.rm = na.rm)
    }
    
    # If "var" is requested
    if(requestedStats["variance"]){
#         results[["Variance"]] <- sapply(X = x, FUN = var, na.rm = na.rm)
        results[["Variance"]] <- sapply(X = x, FUN = stats::var, na.rm = na.rm)
    }
    
    # If "mode" (i.e., the most frequent value) is requested
    if(requestedStats["mode"]){
        results[["Mode"]] <- sapply(X = x, FUN = MFV, outputValue = MFV.outputValue, na.rm = na.rm, silent = silent)
    }
    
    # If "min" is requested
    if(requestedStats["minimum"]){
        results[["Minimum"]] <- sapply(X = x, FUN = min, na.rm = na.rm)
    }
    
    # If "lowerhinge" is requested
    if(requestedStats["lowerhinge"]) {
        results[["Lower Hinge"]] <- results.fivenum[2L, ]
    }
    
    # If "quartile1" is requested
    if(requestedStats["quartile1"]){
        results[["First Quartile"]] <- sapply(X = x, FUN = function(x){unname(quantile.datesOK(x = x, probs = 1/4, na.rm = na.rm, type = quantile.type))})
    }
    
    # If "median" is requested
    if(requestedStats["median"]){
#         results[["Median"]] <- sapply(X = x, FUN = median, na.rm = na.rm)
        results[["Median"]] <- sapply(X = x, FUN = stats::median, na.rm = na.rm)
    }
    
    # If "quartile3" is requested
    if(requestedStats["quartile3"]){
        results[["Third Quartile"]] <- sapply(X = x, FUN = function(x){unname(quantile.datesOK(x = x, probs = 3/4, na.rm = na.rm, type = quantile.type))})
    }
    
    # If "upperhinge" is requested
    if(requestedStats["upperhinge"]) {
        results[["Upper Hinge"]] <- results.fivenum[4L, ]
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
#         results[["IQR"]] <- sapply(X = x, FUN = IQR, na.rm = na.rm, type = quantile.type)
        results[["IQR"]] <- sapply(X = x, FUN = stats::IQR, na.rm = na.rm, type = quantile.type)
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
getResults.byFactors <- function(x, byFactors, x.continuous, requestedStats, na.rm = TRUE, silent = FALSE, digits = 2L, quantile.probs = 0:4/4, quantile.type = 7L, MFV.outputValue = "minimum", statsAreRows = TRUE) {
    splitList <- byFactorsList <- split(x = x.continuous, f = levels(interaction(x[, byFactors, drop = FALSE], sep = ", ")))
    for (i in seq_along(splitList)) {
        byFactorsList[[i]] <- getResults.continuous(x = splitList[[i]], requestedStats = requestedStats, na.rm = na.rm, silent = silent, digits = digits, quantile.probs = quantile.probs, quantile.type = quantile.type, MFV.outputValue = MFV.outputValue, statsAreRows = statsAreRows)
    }
    return(byFactorsList)
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
# descriptiveStatsDF <- function(x, stats = "default", columns = "all", digits = 2L, na.rm = TRUE, silent = FALSE, quantile.probs = 0:4/4, quantile.type = 7L, keepColumnNames = TRUE, categorical.emptyCellSymbol = "", categorical.maxLevels = 10L, categorical.na.exclude = na.rm, output.showStats = "all", byFactors = NULL, ignore = NULL, output.statsAreRows = TRUE, export = FALSE, export.file = NULL, export.print = !export) {
descriptiveStatsDF <- function(x, stats = "default", columns = "all", digits = 2L, na.rm = TRUE, silent = FALSE, quantile.probs = 0:4/4, quantile.type = 7L, keepColumnNames = TRUE, categorical.emptyCellSymbol = "", categorical.maxLevels = 10L, categorical.na.exclude = na.rm, output.showStats = "all", byFactors = NULL, ignore = NULL, output.statsAreRows = TRUE, export = FALSE, export.file = NULL, export.printToConsole = !export) {
    if(silent) {
        oldWarn <- getOption("warn")
        options("warn" = -1L)
        on.exit(options("warn" = oldWarn), add = TRUE)
    }
    
    dataObjectName <- deparse(substitute(x))
    if(grepl(pattern = "\\[|\\$", x = dataObjectName)) {
        if(grepl(pattern = "-|!", x = dataObjectName)) {
            stop(strwrap(gettextf("Please use the %s argument to specify columns you do not want to include in the results.", sQuote("ignore")), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""))
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
#         x <- data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
        bracketfree <- unbracket(x = dataObjectName)
        dataObjectName <- bracketfree[["name"]]
        columns <- bracketfree[["indices"]]
#         originalHasColumnNames <- length(dimnames(x)[[2L]]) > 0L
#         if(is.character(columns) && originalHasColumnNames) {
#             columns <- intersect(columns, dimnames(x)[[2L]])
#         } else {
#             
#         }
        x <- data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
        colnames(x) <- columns
        if(is.integer(bracketfree[["indices"]])) {
#             colnames(x) <- columns
            oldColumnNames <- paste("Column ", columns, sep = "")
            on.exit(warning(strwrap(gettext("NOTE: The data set provided to this function was a subset of a larger data set and the columns (i.e., the variables) were specified by numeric index. Because of this, the column names in the resulting output may not accurately correspond to those in the original data set. This can be fixed by using the 'columns' argument and/or named columns (if possible) to specify the variables of interest."), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""), call. = FALSE), add = TRUE)
        } else {
#             oldColumnNames <- colnames(x) <- columns
            oldColumnNames <- columns
        }
    }
    
    digits <- min(as.integer(abs(digits[1L])), 22L)
    
    requestedStats <- checkStats(stats = as.character(stats), dataObjectName = dataObjectName)
    
    output.showStats <- unique(match.arg(arg = tolower(output.showStats), choices = c("all", "categorical", "continuous", "byfactors", "bylevels", "none"), several.ok = TRUE))
    if(!export.printToConsole && export) {
        output.showStats <- c(output.showStats, "none")
    }
    
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
        warning(strwrap(gettextf("The %s argument was of mode %s whereas the %s argument was of mode %s. The %s argument was set to NULL.", sQuote("columns"), mode(columns), sQuote("byFactors"), mode(byFactors), sQuote("byFactors")), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""))
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
    if(length(invalidVarTypes) > 0L) {
        warning(ngettext(n = length(invalidVarTypes),
                         msg1 = strwrap(gettextf("The following variable was left out of the results either because it was not of a recognized variable type or because its type is not currently supported for analysis: %s.", sQuote(invalidVarTypes)), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""),
                         msg2 = strwrap(gettextf("The following variables were left out of the results either because they were not of recognized variable types or because their types are not currently supported for analysis: %s.", paste(sQuote(invalidVarTypes), collapse = ", ")), width = 0.95 * getOption("width"), prefix = "\n    ", initial = "")))
    }
    x <- x[, !is.na(varTypes), drop = FALSE]
    
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
        results[["ByFactors"]] <- getResults.byFactors(x = x, byFactors = byFactors, x.continuous = x.continuous, requestedStats = requestedStats, na.rm = na.rm, silent = silent, digits = digits, quantile.probs = quantile.probs, quantile.type = quantile.type, statsAreRows = output.statsAreRows)
    }
    
    # Export 'results'?
    if(export) {
        require(rtf)
        if(length(export.file) == 0L || !nzchar(export.file) || !is.character(export.file)) {
#             export.file <- paste(getwd(), "dsdf.rtf", sep = "/")
            export.file <- paste(getwd(), "dsdf%03d.rtf", sep = "/")
            warning(strwrap(gettextf("No valid file name was given for exporting the results. Defaulting to current working directory and file name 'dsdf.rtf'. Any existing files with this name will be overwritten."), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""))
        } else {
            if(!grepl(pattern = "\\.(rtf|doc|docx)$", x = export.file)) {
                export.file <- paste(export.file, "rtf", sep = ".")
            }
            if(!grepl(pattern = "/|\\\\", x = export.file)) {
                export.file <- paste(getwd(), export.file, sep = "/")
            }
        }
        rtf <- RTF(file = export.file, width = 8.5, height = 11, font.size = 12)
        for(i in seq_along(results)) {
            if(!is.data.frame(results[i][[1L]]) && is.list(results[i][[1L]])) {
#                 addText(rtf, names(results[i]), bold = TRUE)
#                 addNewLine(rtf)
                rtf::addText(rtf, names(results[i]), bold = TRUE)
                rtf::addNewLine(rtf)
                for(j in seq_along(results[i][[1L]])) {
#                     addText(rtf, names(results[i][[1L]])[j], italic = TRUE)
#                     addNewLine(rtf)
#                     addTable(rtf, results[i][[1L]][[j]])
#                     addNewLine(rtf)
                    rtf::addText(rtf, names(results[i][[1L]])[j], italic = TRUE)
                    rtf::addNewLine(rtf)
                    rtf::addTable(rtf, results[i][[1L]][[j]])
                    rtf::addNewLine(rtf)
                }
            } else {
#                 addText(rtf, names(results)[i], bold = TRUE)
#                 addNewLine(rtf)
#                 addTable(rtf, results[[i]], row.names = TRUE)
#                 addNewLine(rtf)
                rtf::addText(rtf, names(results)[i], bold = TRUE)
                rtf::addNewLine(rtf)
                rtf::addTable(rtf, results[[i]], row.names = TRUE)
                rtf::addNewLine(rtf)
            }
#             addNewLine(rtf)
#             addNewLine(rtf)
            rtf::addNewLine(rtf)
            rtf::addNewLine(rtf)
        }
#         done(rtf)
        rtf::done(rtf)
        on.exit(cat(strwrap(gettextf("NOTE: Your results have been exported and saved in the following location:\n%s", export.file), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""), sep = ""), add = TRUE)
    }
    
    # Return (i.e. print) the list 'results'
    if("none" %in% output.showStats) {
        return(invisible())
    } else {
        return(results)
    }
}




### SANDBOX ###
# temp <- descriptiveStatsDF(DFFH)
# temp.con <- temp$Continuous

# # use naming convention: dsdf.doc
# library(rtf)
# rtf <- RTF(file = paste(getwd(), "dsdf.rtf", sep = "/"), width = 8.5, height = 11, font.size = 12)
# results <- descriptiveStatsDF(iris)
# for(i in seq_along(results)) {
#     addText(rtf, names(results)[i], bold = TRUE)
#     addNewLine(rtf)
#     addTable(rtf, results[[i]], row.names = TRUE)
#     addNewLine(rtf)
#     addNewLine(rtf)
# }
# # addTable(rtf, descriptiveStatsDF(iris), row.names = TRUE, col.justify = c("R", "R", "R", "R", "C"), header.col.justify = "C")
# done(rtf)


# ## PLOTTING ESCAPADES ##
# results <- descriptiveStatsDF(DFFH, output.showStats = "continuous", ignore = "doa")[[1L]]
# temp <- DFFH[, c("key", "age", "htcm", "wtkg")]
# for(i in seq_len(NCOL(temp))) {
#     boxplot(temp[,i,drop=FALSE])
# }
# boxplot(temp, horizontal = TRUE)
# 
# layout(matrix(c(1,2),2,1))
# boxplot(temp[,2],horizontal=TRUE)
# hist(temp[,2])
# 
# # ##-- Create a scatterplot with marginal histograms -----
# # def.par <- par(no.readonly = TRUE)
# # 
# # x <- pmin(3, pmax(-3, stats::rnorm(50)))
# # y <- pmin(3, pmax(-3, stats::rnorm(50)))
# # xhist <- hist(x, breaks = seq(-3,3,0.5), plot = FALSE)
# # yhist <- hist(y, breaks = seq(-3,3,0.5), plot = FALSE)
# # top <- max(c(xhist$counts, yhist$counts))
# # xrange <- c(-3, 3)
# # yrange <- c(-3, 3)
# # nf <- layout(matrix(c(2,0,1,3),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)
# # layout.show(nf)
# # 
# # par(mar = c(3,3,1,1))
# # plot(x, y, xlim = xrange, ylim = yrange, xlab = "", ylab = "")
# # par(mar = c(0,3,1,1))
# # barplot(xhist$counts, axes = FALSE, ylim = c(0, top), space = 0)
# # par(mar = c(3,0,1,1))
# # barplot(yhist$counts, axes = FALSE, xlim = c(0, top), space = 0, horiz = TRUE)
# # 
# # par(def.par)  #- reset to default
# 
# 
# # # http://rgraphgallery.blogspot.com/2013/04/rg-plotting-boxplot-and-histogram.html
# # # data 
# # set.seed(4566)
# # data <- rnorm(100)
# # 
# # # layout where the boxplot is at top  
# # nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
# # par(mar=c(3.1, 3.1, 1.1, 2.1))
# # boxplot(data, horizontal=TRUE,  outline=TRUE,ylim=c(-4,4), frame=FALSE, col = "green1")
# # hist(data,xlim=c(-4,4), col = "pink")
# 
# 
# set.seed(0718)
# x <- rnorm(500, sd = 100)
# x.hist <- hist(x, plot = FALSE)
# # boxplot(x, frame = FALSE, axes = FALSE, horizontal = TRUE)
# # # hist(x, freq = FALSE, xlim = 1.5*c(min(x), max(x)))
# # # hist(x, freq = FALSE, xlim = c(floor(min(x)), ceiling(max(x))), add = TRUE)
# # x.hist <- hist(x, plot = FALSE)
# # hist(x, freq = FALSE, xlim = c(floor(min(x)), ceiling(max(x))), ylim = c(0, min(1, 1.05 * max(x.hist$density))))
# # lines(density(x)$x, density(x)$y, lwd = 2, col = "red")
# # box()
# 
# dev.off()
# nf <- layout(matrix(c(1,2),nrow=2,ncol=1, byrow=TRUE), height=c(1,3))
# # layout.show(2)
# par(mar=c(0.1, 4.1, 0.1, 2.1))
# boxplot(x, frame = FALSE, axes = FALSE, horizontal = TRUE, ylim = c(floor(min(x)), ceiling(max(x))))
# par(mar=c(3.1, 4.1, 0.1, 2.1))
# hist(x, freq = FALSE, xlim = c(floor(min(x)), ceiling(max(x))), ylim = c(0, ceiling(10.5 * max(x.hist$density))/10), main = NULL)
# lines(density(x)$x, density(x)$y, lwd = 2, col = "red")
# ##
# diffx <- diff(par()$usr[1:2])
# # diffy <- diff(par()$usr[3:4])
# xpoint <- (mean(x) + abs(par()$usr[1])) / diffx
# # par(usr = c(0, 1, 0, 1))
# abline(v = xpoint, lwd = 3, col = "blue")
# ##
# # par(mar = c(5.1, 4.1, 4.1, 2.1))
# # par(def.par)
# layout(1)
# par(new=TRUE)
# # abline(v=mean(x),col="blue",lwd=3)
# # par(new=TRUE)
# par(mar = c(0.1, 4.1, 0.1, 2.1))
# plot(mean(x),1,type="h",frame=FALSE,axes=FALSE,lwd=3,col="blue",xaxt="n",yaxt="n",ylim=c(-0.1,1),xlab="",ylab="")
# 
# 
# set.seed(0719)
# x <- rnorm(400, mean = 4)
# x.hist <- hist(x, plot = FALSE)
# par(def.par)
# # par(mfrow=c(1,2))
# par(mfcol=c(2,1))
# par(mar=c(0.1,4.1,0.1,2.1))
# x.limits <- c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
# boxplot(x = x, horizontal = TRUE, axes = FALSE, xlim = x.limits)
# hist(x = x, freq = FALSE, xlim = x.limits, main = NULL)
# 
# x.hist <- hist(x = x, plot = FALSE)
# 
# 
# dev.off()
# # 2014-07-19: http://stackoverflow.com/a/16083416
# set.seed(123)
# data <- rnorm(1000)
# nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
# par(mar=c(5.1, 4.1, 1.1, 2.1))
# boxplot(data, horizontal=TRUE,  outline=FALSE,ylim=c(-4,4),xlab="",ylab="",axes=FALSE)
# hist(data,xlim=c(-4,4))
# 
# 
# # findDecimalPoint <- function(x, decimal.mark = ".") {
# #     if(is.integer(x[1L])) {
# #         return(nchar(x[1L]) + 1L)
# #     }
# #     decimal.mark <- as.character(decimal.mark[1L])
# #     if(length(decimal.mark) == 0L || !(decimal.mark %in% c(".", ","))) {
# #         decimal.mark <- "."
# #     }
# #     x <- unlist(strsplit(x = as.character(as.numeric(x[1L])), split = decimal.mark, fixed = TRUE))
# #     return(nchar(x[1L]) + 1L)
# # }
# # findDecimalPoint <- function(x) {
# #     x <- as.numeric(x)[!is.na(as.numeric(x))]
# # #     iparts <- trunc(x)
# # #     isPositive <- nchar(iparts) == nchar(abs(iparts))
# # #     return(nchar(iparts) - !isPositive + 1L)
# # #     return(nchar(abs(iparts)) + 1L)
# #     return(nchar(abs(trunc(x))) + 1L)
# # }
# # round(head(x), )
# # 
# # set.seed(0719)
# # x <- rnorm(20)
# # findDecimalPoint <- function(x, decimal.mark = ".") {
# #     if(is.integer(x)) {
# #         return(nchar(x) + 1L)
# #     }
# #     decimal.mark <- as.character(decimal.mark)
# #     if(length(decimal.mark) == 0L || !all(decimal.mark %in% c(".", ","))) {
# #         decimal.mark[!(decimal.mark %in% c(".", ","))] <- "."
# #     }
# #     x <- unlist(strsplit(x = as.character(as.numeric(x)), split = decimal.mark, fixed = TRUE))
# #     return(nchar(x[1L]) + 1L)
# # }
# 
# # getRounding <- function(x) {
# #     # The following integer will represent how many places you would
# #     # need to "move" the decimal point *to the left* in order to end
# #     # up with exactly one non-zero digit to the left of the decimal
# #     # point. (Negative values mean you need to move to the right.)
# #     minplaces <- floor(log10(abs(x)))
# #     mins <- floor(x / (10^minplaces)) * 10^minplaces
# #     
# #     maxplaces <- ceiling(log10(abs(x))) - 1L
# #     maxs <- ceiling(x / (10^maxplaces)) * 10^maxplaces
# #     return(list("mins" = mins, "maxs" = maxs))
# # }
# # getRounding <- function(x) {
# #     places <- trunc(log10(abs(range(x))))
# #     places[!is.finite(places)] <- 0L
# #     c(floor(range(x) / (10^places))[1L], ceiling(range(x) / (10^places))[2L]) * 10^places
# # #     c(floor(range(x) / (10^places))[1L], ceiling(range(x) / (10^(places-1L)))[2L]) * 10^places
# # }

# ## USE THIS STUFF ##
# def.par <- par(no.readonly = TRUE)
# getRounding <- function(x) {
#     larx <- log10(abs(range(x)))
#     larx[!is.finite(larx) | is.na(larx)] <- 0L
#     minplaces <- floor(larx[1L])
#     maxplaces <- ceiling(larx[2L])
#     c((floor(range(x)[1L] / (10^minplaces)) * 10^minplaces), (ceiling(range(x)[2L] / (10^(maxplaces - 1L))) * 10^(maxplaces - 1L)))
# }
# 
# 
# set.seed(0718)
# x <- rnorm(500, sd = 100)
# # x <- rexp(500, rate = 0.01)
# x.boxplot.stats <- boxplot(x, plot = FALSE)$stats
# x.hist.density <- hist(x, plot = FALSE)$density
# 
# dev.off()
# nf <- layout(matrix(c(1, 2), nrow = 2, ncol = 1, byrow = TRUE), height = c(1, 3))
# par(mar = c(0, 4.1, 1.1, 2.1))
# boxplot(x, frame = FALSE, axes = FALSE, horizontal = TRUE, ylim = getRounding(x))
# segments(x0 = mean(x), y0 = 0L, x1 = mean(x), y1 = 1L, lwd = 2, col = "blue")
# segments(x0 = median(x), y0 = 0L, x1 = median(x), y1 = 1L, lwd = 2, lty = "dashed", col = "orange")
# segments(x0 = unname(quantile(x, probs = c(1/4, 3/4), type = 7L)), y0 = 0L, x1 = unname(quantile(x, probs = c(1/4, 3/4), type = 7L)), y1 = 1L, lwd = 2, lty = "dashed", col = "dark green")
# segments(x0 = x.boxplot.stats[1L], y0 = 0L, x1 = x.boxplot.stats[1L], y1 = 1L, col = "magenta", lty = 3L, lwd = 2L)
# segments(x0 = x.boxplot.stats[5L], y0 = 0L, x1 = x.boxplot.stats[5L], y1 = 1L, col = "magenta", lty = 3L, lwd = 2L)
# 
# par(mar = c(3.1, 4.1, 0, 2.1))
# hist(x, freq = FALSE, xlim = getRounding(x), ylim = getRounding(x.hist.density), main = NULL)
# lines(density(x)$x, density(x)$y, lwd = 2, col = "red")
# abline(v = mean(x), lwd = 2, col = "blue")
# abline(v = unname(quantile(x, probs = c(1/4, 3/4), type = 7L)), lwd = 2, lty = "dashed", col = "dark green")
# abline(v = median(x), lwd = 2, lty = "dashed", col = "orange")
# abline(v = x.boxplot.stats[1L], lwd = 2, lty = 3, col = "magenta")
# abline(v = x.boxplot.stats[5L], lwd = 2, lty = 3, col = "magenta")
# 
# layout(1)
# par(def.par)  #- reset to default
# ## END USE THIS STUFF ##

# ## EXTRA IDEAS/PLANS ##
# par(new = TRUE)
# # plot(ecdf(x), col = "violet", frame = FALSE, axes = FALSE, xlim = getRounding(x), ylim = getRounding(hist(x, plot = FALSE)$density), xlab = "", ylab = "", main = "")
# plot(x = sort(x), y = seq_along(x) * max(x.hist.density) / length(x), col = "purple", type = "s", frame = FALSE, axes = FALSE, xlim = getRounding(x), ylim = getRounding(hist(x, plot = FALSE)$density), xlab = "", ylab = "", main = "")
# # segments(x0 = mean(x), y0 = max(x.hist.density), x1 = max(x), y1 = max(x.hist.density), col = "violet", lty = 5L)
# 
# # legend("right",
# #        inset = 0.05,
# #        legend = c("Mean", "Median", "Quartiles", "Min. and Max."),
# #        lty = c(1L, 2L, 2L, 3L),
# #        lwd = c(2L, 2L, 2L, 2L),
# #        col = c("blue", "orange", "dark green", "magenta"))
# # legend("right",
# #        inset = 0.01,
# #        legend = c(paste(formatC(mean(x), digits = 1L, format = "f"), " (", formatC(sd(x), digits = 1L, format = "f"), ")", sep = ""), paste(formatC(median(x), digits = 1, format = "f"), " (", formatC(quantile(x, probs = 1/4, type = 7L), digits = 1L, format = "f"), " - ", formatC(quantile(x, probs = 3/4, type = 7L), digits = 1L, format = "f"), ")", sep = "")))
# 
# 
# # # Line types
# # lty <- 1L
# # lty.density <- lty
# # lty.mean <- lty
# # lty.median <- lty + 1L
# # lty.quartile1 <- lty + 1L
# # lty.quartile3 <- lty + 1L
# # lty.minimum <- lty + 2L
# # lty.maximum <- lty + 2L
# # 
# # # Line widths
# # lwd <- 2L
# # lwd.density <- lwd
# # lwd.mean <- lwd
# # lwd.median <- lwd
# # lwd.quartile1 <- lwd
# # lwd.quartile3 <- lwd
# # lwd.minimum <- lwd
# # lwd.maximum <- lwd
# # 
# # # Line colors
# # col <- "black"
# # col.density <- "red"
# # col.mean <- "blue"
# # col.median <- "orange"
# # col.quartile1 <- "dark green"
# # col.quartile3 <- "dark green"
# # col.minimum <- "magenta"
# # col.maximum <- "magenta"
# # 
# # # Plot range limits
# # ylim.boxplot <- xlim.histogram <- getRounding(x)
# # ylim.histogram <- getRounding(hist(x, plot = FALSE)$density)
# ## END EXTRA IDEAS/PLANS ##



# checkStats <- function(stats = "default", dataObjectName = NULL){
checkPlotLines <- function(plotLines = "default", dataObjectName = NULL) {
    # Supply a data object name if none is provided
    if(length(dataObjectName) == 0L){
        dataObjectName <- "the data set"
    }
    
    # Make sure all of the values specified for "plotLines" are
    # lower case (to facilitate string matching)
    lowPlotLines <- gsub(pattern = "[[:punct:]]", replacement = "", x = tolower(plotLines))
    
    # List out all of the options for "plotLines" that will be
    # accepted/recognized by the function
    possiblePlotLines <- c("default", "all", "mean", "average", "avg", "median", "q2", "quartile2", "mode", "mfv", "mcv", "sd", "standard deviation", "stddeviation", "std deviation", "variance", "iqr", "interquartile range", "iq range", "minimum", "maximum", "fivenum", "range", "q1", "quartile1", "q3", "quartile3", "quartiles", "quantiles", "summary", "hinges", "lower hinge", "lowerhinge", "lhinge", "upper hinge", "upperhinge", "uhinge", "ecdf")
    if(!any(pmatch(x = lowPlotLines, table = possiblePlotLines, nomatch = 0L) > 0L)) {
        stop(strwrap(gettextf("I need at least one valid statistic to be specified before I can plot any lines. Please check for spelling errors."), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""))
    }
    validPlotLines <- match.arg(arg = lowPlotLines, choices = possiblePlotLines, several.ok = TRUE)
    if(length(lowPlotLines) != length(validPlotLines)) {
        # Use "plotLines" instead of "lowPlotLines" so the printed
        # values resemble the original user input, not the
        # tolower()ed argument(s)
#         invalidPlotLines <- plotLines[-match(x = possiblePlotLines, table = lowPlotLines, nomatch = 0L)]
        invalidPlotLines <- setdiff(lowPlotLines, possiblePlotLines)
        warning(strwrap(gettextf("At least one specified statistic was invalid. Only recognized statistics were plotted. I did not recognize: %s.", paste(invalidPlotLines, collapse = ", ")), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""))
    }
    
    # Check which statistics options selected
    requestedPlotLinesNames <- c("mean", "median", "mode", "sd", "variance", "minimum", "maximum", "range", "iqr", "lowerhinge", "upperhinge", "quartile1", "quartile3", "quantiles", "ecdf")
    requestedPlotLines <- rep(FALSE, length.out = length(requestedPlotLinesNames))
    names(requestedPlotLines) <- requestedPlotLinesNames
    
    # Check for "mean"
    requestedPlotLines["mean"] <- any(c("mean", "average", "avg", "summary", "default", "all") %in% validPlotLines)
    
    # Check for "median"
    requestedPlotLines["median"] <- any(c("median", "q2", "quartile2", "quartiles", "fivenum", "summary", "default", "all") %in% validPlotLines)
    
    # Check for "mode"
    requestedPlotLines["mode"] <- any(c("mode", "mfv", "mcv", "all") %in% validPlotLines)
    
    # Check for "sd"
    requestedPlotLines["sd"] <- any(c("sd", "standard deviation", "stddeviation", "std deviation", "all") %in% validPlotLines)
    
    # Check for "variance"
    requestedPlotLines["variance"] <- any(c("variance", "all") %in% validPlotLines)
    
    # Check for "minimum"
    requestedPlotLines["minimum"] <- any(c("minimum", "fivenum", "summary", "all") %in% validPlotLines)
    
    # Check for "maximum"
    requestedPlotLines["maximum"] <- any(c("maximum", "fivenum", "summary", "all") %in% validPlotLines)
    
    # Check for "range"
    requestedPlotLines["range"] <- any(c("range", "all") %in% validPlotLines)
    
    # Check for "iqr"
    requestedPlotLines["iqr"] <- any(c("iqr", "interquartile range", "iq range", "all") %in% validPlotLines)
    
    # Check for "lowerhinge"
    requestedPlotLines["lowerhinge"] <- any(c("lower hinge", "lowerhinge", "lhinge", "hinges", "fivenum", "all") %in% validPlotLines)
    
    # Check for "upperhinge"
    requestedPlotLines["upperhinge"] <- any(c("upper hinge", "upperhinge", "uhinge", "hinges", "fivenum", "all") %in% validPlotLines)
    
    # Check for "quartile1"
    requestedPlotLines["quartile1"] <- any(c("q1", "quartile1", "quartiles", "summary", "all") %in% validPlotLines)
    
    # Check for "quartile3"
    requestedPlotLines["quartile3"] <- any(c("q3", "quartile3", "quartiles", "summary", "all") %in% validPlotLines)
    
    # Check for "quantiles"
    requestedPlotLines["quantiles"] <- any(c("quantiles", "all") %in% validPlotLines)
    
    # Check for "ecdf"
    requestedPlotLines["ecdf"] <- any(c("ecdf", "all") %in% validPlotLines)
    
    return(requestedPlotLines)
}


## USE THIS STUFF ##
# getBoxHist.standalone <- function(x, na.rm = TRUE) {
# getBoxHist.standalone <- function(x, na.rm = TRUE, dataObjectName = NULL, title.line = -1L, title.cex = 1.0, title.col = "black", title.font = 2L, quantile.type = 7L) {
# getBoxHist.standalone <- function(x, na.rm = TRUE, dataObjectName = NULL, quantile.type = 7L, title.line = -1L, title.cex = 1.0, title.col = "black", title.font = 2L) {
getBoxHist.standalone <- function(x, na.rm = TRUE, dataObjectName = NULL, quantile.type = 7L, line.main = -1L, cex.main = 1.0, col.main = "black", font.main = 2L, lty = 1L, lty.density = lty, lty.mean = lty, lty.median = lty + 1L, lty.quartile1 = lty + 1L, lty.quartile3 = lty + 1L, lty.minimum = lty + 2L, lty.maximum = lty + 2L, lwd = 2L, lwd.density = lwd, lwd.mean = lwd, lwd.median = lwd, lwd.quartile1 = lwd, lwd.quartile3 = lwd, lwd.minimum = lwd, lwd.maximum = lwd, col = "black", col.density = "red", col.mean = "blue", col.median = "orange", col.quartile1 = "dark green", col.quartile3 = "dark green", col.minimum = "magenta", col.maximum = "magenta") {
    def.par <- par(no.readonly = TRUE)
    on.exit(layout(1), add = TRUE)
    on.exit(par(def.par), add = TRUE)
    
    getRounding <- function(x, na.rm = TRUE) {
        larx <- log10(abs(range(x, na.rm = na.rm)))
        larx[!is.finite(larx) | is.na(larx)] <- 0L
        minplaces <- floor(larx[1L])
        maxplaces <- ceiling(larx[2L])
        c((floor(range(x)[1L] / (10^minplaces)) * 10^minplaces), (ceiling(range(x)[2L] / (10^(maxplaces - 1L))) * 10^(maxplaces - 1L)))
    }
    
    defaultIfNULL <- function(option, default) {
#         textOption <- deparse(substitute(option))
        if(!exists(deparse(substitute(option)))) {
            option <- NULL
        }
        if(length(option) == 0L || !is.finite(option) || is.na(option)) {
            default
        } else {
            option
        }
    }
    
    if(length(dataObjectName) < 1L) {
        dataObjectName <- deparse(substitute(x))
    }
    
    quantile.type <- as.integer(abs(quantile.type[1L]))
    if(!(quantile.type %in% as.integer(1:9))) {
        quantile.type <- 7L
    }
    
    lty <- lty[1L] #<--CHANGE TO ACCEPT A VECTOR AT SOME POINT?
    if(is.character(lty)) {
        validLineTypes <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
        if(!(lty %in% validLineTypes)) {
            warning(strwrap(gettextf("Custom or invalid line type specifications such as %s currently are not supported. The %s option has been set to 1 (%s). Valid character options are: %s.", sQuote(lty), sQuote("lty"), sQuote("solid"), paste(sQuote(validLineTypes), collapse = ", ")), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""))
            lty <- 1L
        } else {
            lty <- match(x = lty, table = validLineTypes, nomatch = 0L) - 1L
        }
    }
    lwd <- abs(lwd[!is.na(lwd)])[1L] #<-- ACCEPT VECTOR AT SOME POINT?
    col <- col[1L] #<-- ACCEPT VECTOR AT SOME POINT?
    
    lty.density <- defaultIfNULL(lty.density, lty)
    lwd.density <- defaultIfNULL(lwd.density, lwd)
    col.density <- defaultIfNULL(col.density, "red")
    
    lty.mean <- defaultIfNULL(lty.mean, lty)
    lwd.mean <- defaultIfNULL(lwd.mean, lwd)
    col.mean <- defaultIfNULL(col.mean, "blue")
    
    lty.median <- defaultIfNULL(lty.median, (lty %% 6L) + 1L)
    lwd.median <- defaultIfNULL(lwd.median, lwd)
    col.median <- defaultIfNULL(col.median, "orange")
    
    lty.quartile1 <- defaultIfNULL(lty.quartile1, (lty %% 6L) + 1L)
    lwd.quartile1 <- defaultIfNULL(lwd.quartile1, lwd)
    col.quartile1 <- defaultIfNULL(col.quartile1, "green3")
    
    lty.quartile3 <- defaultIfNULL(lty.quartile3, (lty %% 6L) + 1L)
    lwd.quartile3 <- defaultIfNULL(lwd.quartile3, lwd)
    col.quartile3 <- defaultIfNULL(col.quartile3, "green3")
    
    lty.minimum <- defaultIfNULL(lty.minimum, (lty %% 6L) + 2L)
    lwd.minimum <- defaultIfNULL(lwd.minimum, lwd)
    col.minimum <- defaultIfNULL(col.minimum, "magenta")
    
    lty.maximum <- defaultIfNULL(lty.maximum, (lty %% 6L) + 2L)
    lwd.maximum <- defaultIfNULL(lwd.maximum, lwd)
    col.maximum <- defaultIfNULL(col.maximum, "magenta")
    
    col.main <- defaultIfNULL(col.main, "black")
    cex.main <- defaultIfNULL(cex.main, 1.0)
    font.main <- defaultIfNULL(font.main, 1L)
    line.main <- defaultIfNULL(line.main, -1L)
    
#     set.seed(0718)
#     x <- rnorm(500, sd = 100)
#     # x <- rexp(500, rate = 0.01)
    x.boxplot.stats <- boxplot(x, plot = FALSE)$stats
    x.hist.density <- hist(x, plot = FALSE)$density
    x.density <- density(x)
    
#     dev.off()
    nf <- layout(matrix(c(1, 2), nrow = 2, ncol = 1, byrow = TRUE), height = c(1, 3))
    par(mar = c(0, 4.1, 1.1, 2.1))
    boxplot(x, frame = FALSE, axes = FALSE, horizontal = TRUE, ylim = getRounding(x, na.rm = na.rm))
#     segments(x0 = mean(x, na.rm = na.rm), y0 = 0L, x1 = mean(x, na.rm = na.rm), y1 = 1L, lwd = 2, col = "blue")
#     segments(x0 = median(x, na.rm = na.rm), y0 = 0L, x1 = median(x, na.rm = na.rm), y1 = 1L, lwd = 2, lty = "dashed", col = "orange")
#     segments(x0 = quantile(x, probs = c(1/4, 3/4), type = quantile.type, names = FALSE, na.rm = na.rm), y0 = 0L, x1 = quantile(x, probs = c(1/4, 3/4), type = quantile.type, names = FALSE, na.rm = na.rm), y1 = 1L, lwd = 2, lty = "dashed", col = "dark green")
#     segments(x0 = x.boxplot.stats[1L], y0 = 0L, x1 = x.boxplot.stats[1L], y1 = 1L, col = "magenta", lty = 3L, lwd = 2L)
#     segments(x0 = x.boxplot.stats[5L], y0 = 0L, x1 = x.boxplot.stats[5L], y1 = 1L, col = "magenta", lty = 3L, lwd = 2L)
    segments(x0 = mean(x, na.rm = na.rm), y0 = 0L, x1 = mean(x, na.rm = na.rm), y1 = 1L, lty = lty.mean, lwd = lwd.mean, col = col.mean)
    segments(x0 = median(x, na.rm = na.rm), y0 = 0L, x1 = median(x, na.rm = na.rm), y1 = 1L, lty = lty.median, lwd = lwd.median, col = col.median)
    segments(x0 = quantile(x, probs = 1/4, type = quantile.type, names = FALSE, na.rm = na.rm), y0 = 0L, x1 = quantile(x, probs = 1/4, type = quantile.type, names = FALSE, na.rm = na.rm), y1 = 1L, lty = lty.quartile1, lwd = lwd.quartile1, col = col.quartile1)
    segments(x0 = quantile(x, probs = 3/4, type = quantile.type, names = FALSE, na.rm = na.rm), y0 = 0L, x1 = quantile(x, probs = 3/4, type = quantile.type, names = FALSE, na.rm = na.rm), y1 = 1L, lty = lty.quartile3, lwd = lwd.quartile3, col = col.quartile3)
    segments(x0 = x.boxplot.stats[1L], y0 = 0L, x1 = x.boxplot.stats[1L], y1 = 1L, lty = lty.minimum, lwd = lwd.minimum, col = col.minimum)
    segments(x0 = x.boxplot.stats[5L], y0 = 0L, x1 = x.boxplot.stats[5L], y1 = 1L, lty = lty.maximum, lwd = lwd.maximum, col = col.maximum)
    # For "font = ": 1 = plain text (default), 2 = bold face, 3 = italic, 4 = bold italic, 5 = symbol font
#     mtext(sprintf("Plots for %s", sQuote(dataObjectName)), line = title.line, cex = title.cex, col = title.col, font = title.font)
    title(main = sprintf("Plots for %s", sQuote(dataObjectName)), col.main = col.main, cex.main = cex.main, font.main = font.main, line = line.main)
    
    par(mar = c(3.1, 4.1, 0, 2.1))
    hist(x, freq = FALSE, xlim = getRounding(x, na.rm = na.rm), ylim = getRounding(x.hist.density, na.rm = na.rm), main = NULL)
# #     lines(density(x)$x, density(x)$y, lwd = 2, col = "red")
#     lines(x.density$x, x.density$y, lwd = 2, col = "red")
#     abline(v = mean(x, na.rm = na.rm), lwd = 2, col = "blue")
#     abline(v = quantile(x, probs = c(1/4, 3/4), type = quantile.type, names = FALSE, na.rm = na.rm), lwd = 2, lty = "dashed", col = "dark green")
#     abline(v = median(x, na.rm = na.rm), lwd = 2, lty = "dashed", col = "orange")
#     abline(v = x.boxplot.stats[1L], lwd = 2, lty = 3, col = "magenta")
#     abline(v = x.boxplot.stats[5L], lwd = 2, lty = 3, col = "magenta")
    lines(x.density$x, x.density$y, lty = lty.density, lwd = lwd.density, col = col.density)
    abline(v = mean(x, na.rm = na.rm), lty = lty.mean, lwd = lwd.mean, col = col.mean)
    abline(v = median(x, na.rm = na.rm), lty = lty.median, lwd = lwd.median, col = col.median)
    abline(v = quantile(x, probs = 1/4, type = quantile.type, names = FALSE, na.rm = na.rm), lty = lty.quartile1, lwd = lwd.quartile1, col = col.quartile1)
    abline(v = quantile(x, probs = 3/4, type = quantile.type, names = FALSE, na.rm = na.rm), lty = lty.quartile3, lwd = lwd.quartile3, col = col.quartile3)
    abline(v = x.boxplot.stats[1L], lty = lty.minimum, lwd = lwd.minimum, col = col.minimum)
    abline(v = x.boxplot.stats[5L], lty = lty.maximum, lwd = lwd.maximum, col = col.maximum)
}
## END USE THIS STUFF ##




# # Line types
# lty = 1L, lty.density = lty, lty.mean = lty, lty.median = lty + 1L, lty.quartile1 = lty + 1L, lty.quartile3 = lty + 1L, lty.minimum = lty + 2L, lty.maximum = lty + 2L,
# # Line widths
# lwd = 2L, lwd.density = lwd, lwd.mean = lwd, lwd.median = lwd, lwd.quartile1 = lwd, lwd.quartile3 = lwd, lwd.minimum = lwd, lwd.maximum = lwd,
# # Line colors
# col = "black", col.density = "red", col.mean = "blue", col.median = "orange", col.quartile1 = "dark green", col.quartile3 = "dark green", col.minimum = "magenta", col.maximum = "magenta",
#     # Line types
# #     lty <- 1L
# #     lty.density <- lty
# #     lty.mean <- lty
# #     lty.median <- lty + 1L
# #     lty.quartile1 <- lty + 1L
# #     lty.quartile3 <- lty + 1L
# #     lty.minimum <- lty + 2L
# #     lty.maximum <- lty + 2L
#     lty <- lty[1L] #<--CHANGE TO ACCEPT A VECTOR (AT SOME POINT)
#     if(is.character(lty)) {
#         validLineTypes <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
#         if(!(lty %in% validLineTypes)) {
#             warning(strwrap(gettextf("Custom or invalid line type specifications such as %s currently are not supported. The %s option has been set to 1 (%s). Valid character options are: %s.", sQuote(lty), sQuote("lty"), sQuote("solid"), paste(sQuote(validLineTypes), collapse = ", ")), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""))
#             lty <- 1L
#         } else {
#             lty <- match(x = lty, table = validLineTypes, nomatch = 0L) - 1L
#         }
#     }
#     lty.density <- defaultIfNULL(lty.density, lty)
#     lty.mean <- defaultIfNULL(lty.mean, lty)
#     lty.median <- defaultIfNULL(lty.median, (lty %% 6L) + 1L)
#     lty.quartile1 <- defaultIfNULL(lty.quartile1, (lty %% 6L) + 1L)
#     lty.quartile3 <- defaultIfNULL(lty.quartile3, (lty %% 6L) + 1L)
#     lty.minimum <- defaultIfNULL(lty.minimum, (lty %% 6L) + 2L)
#     lty.maximum <- defaultIfNULL(lty.maximum, (lty %% 6L) + 2L)
#     
#     # Line widths
# #     lwd <- 2L
# #     lwd.density <- lwd
# #     lwd.mean <- lwd
# #     lwd.median <- lwd
# #     lwd.quartile1 <- lwd
# #     lwd.quartile3 <- lwd
# #     lwd.minimum <- lwd
# #     lwd.maximum <- lwd
#     lwd <- abs(lwd[!is.na(lwd)])[1L] #<-- ACCEPT VECTOR AT SOME POINT?
#     lwd.density <- defaultIfNULL(lwd.density, lwd)
#     lwd.mean <- defaultIfNULL(lwd.mean, lwd)
#     lwd.median <- defaultIfNULL(lwd.median, lwd)
#     lwd.quartile1 <- defaultIfNULL(lwd.quartile1, lwd)
#     lwd.quartile3 <- defaultIfNULL(lwd.quartile3, lwd)
#     lwd.minimum <- defaultIfNULL(lwd.minimum, lwd)
#     lwd.maximum <- defaultIfNULL(lwd.maximum, lwd)
#     
#     # Line colors
# #     col <- "black"
# #     col.density <- "red"
# #     col.mean <- "blue"
# #     col.median <- "orange"
# #     col.quartile1 <- "dark green"
# #     col.quartile3 <- "dark green"
# #     col.minimum <- "magenta"
# #     col.maximum <- "magenta"
#     col <- col[1L]
#     col.density <- defaultIfNULL(col.density, "red")
#     col.mean <- defaultIfNULL(col.mean, "blue")
#     col.median <- defaultIfNULL(col.median, "orange")
#     col.quartile1 <- defaultIfNULL(col.quartile1, "green3")
#     col.quartile3 <- defaultIfNULL(col.quartile3, "green3")
#     col.minimum <- defaultIfNULL(col.minimum, "magenta")
#     col.maximum <- defaultIfNULL(col.maximum, "magenta")


