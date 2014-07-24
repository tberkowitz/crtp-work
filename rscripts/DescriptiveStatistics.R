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
    stats::quantile(x, probs = probs, type = type, ...)
}

# Define the fivenum.datesOK() function
fivenum.datesOK <- function(x, na.rm = TRUE) {
    y <- if(isDateVar <- any(class(x) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt"))) {
        unclass(x)
    } else {
        x
    }
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
        results[["Std. Dev."]] <- sapply(X = x, FUN = stats::sd, na.rm = na.rm)
    }
    
    # If "var" is requested
    if(requestedStats["variance"]){
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


# Define the getBoxHist() function
getBoxHist <- function(x, na.rm = TRUE, dataObjectName = NULL, plotDensityCurve = TRUE, plotVerticalLines = TRUE, plotStatsValues = TRUE, lty = par("lty"), lwd = par("lwd"), col = par("col"), digits = 2L, textLineWidthFactor = 0.15 * par("cex"), textLineHeightFactor = 1.015 * par("cex"), boxplotBuffer = 0.3) {
    def.par <- par(no.readonly = TRUE)
    on.exit(layout(1), add = TRUE)
    on.exit(par(def.par), add = TRUE)
    
# #     fPart <- function(x) abs(x - trunc(x))
# #     
# #     getRounding <- function(x, na.rm = TRUE, adj = TRUE) {
# #         arx <- abs(range(x, na.rm = na.rm))
# #         larx <- log10(arx)
# #         larx[!is.finite(larx) | is.na(larx)] <- 0L
# # #         powers <- 10^(floor(larx))
# #         powers <- 10^(floor(larx) - 1L)
# #         denoms <- fPart(arx / powers) < 0.5 & fPart(arx / powers) > 0
# #         adj <- adj * sign(range(x)) * powers * denoms / 2
# #         c(floor(range(x)[1L] / powers[1L]), ceiling(range(x)[2L] / powers[2L])) * powers - adj
# #     }
#     getRounding <- function(x, na.rm = TRUE, adj = TRUE) {
#         fPart <- function(x) abs(x - trunc(x))
#     #     arx <- abs(range(x, na.rm = na.rm))
#     #     larx <- log10(arx)
#     #     larx[!is.finite(larx) | is.na(larx)] <- 0L
#     #     powers <- 10^(floor(larx) - 1L)
#     #     denoms <- fPart(arx / powers) < 0.5 & fPart(arx / powers) > 0
#     #     adj <- adj * sign(range(x)) * powers * denoms / 2
#     #     c(floor(range(x)[1L] / powers[1L]), ceiling(range(x)[2L] / powers[2L])) * powers - adj
#         minx <- abs(range(x, na.rm = na.rm)[1L])
#         lminx <- log10(minx)
#         lminx[!is.finite(lminx) | is.na(lminx)] <- 0L
#         minpower <- 10^(floor(lminx) - 1L)
#     #     mindenom <- fPart(minx / minpower) < 0.5 & fPart(minx / minpower) > 0
# #         mindenom <- fPart(minx / minpower) > 0.5 & fPart(minx / minpower) < 0
#         mindenom <- fPart(minx / minpower) > 0.5
#         minadj <- adj * sign(minx) * minpower * mindenom / 2
#         
#         maxx <- abs(range(x, na.rm = na.rm)[2L])
#         lmaxx <- log10(maxx)
#         lmaxx[!is.finite(lmaxx) | is.na(lmaxx)] <- 0L
#         maxpower <- 10^(floor(lmaxx) - 1L)
#         maxdenom <- fPart(maxx / maxpower) < 0.5 & fPart(maxx / maxpower) > 0
#         maxadj <- adj * sign(maxx) * maxpower * maxdenom / 2
#         
#         c(floor(minx / minpower) * minpower - minadj, ceiling(maxx / maxpower) * maxpower - maxadj)
#     }
    
    if(length(dataObjectName) < 1L) {
        dataObjectName <- deparse(substitute(x))
    }
    
    if(missing(lty) || length(lty) == 0L) {
        lty <- par("lty")
    }
    if(length(lty) == 1L) {
        lty.density <- lty.vertical <- lty
    } else {
        lty.density <- lty[1L]
        lty.vertical <- lty[-1L]
    }
    
    if(missing(lwd) || length(lwd) == 0L) {
        lwd <- par("lwd")
    }
    if(length(lwd) == 1L) {
        lwd.density <- lwd.vertical <- lwd
    } else {
        lwd.density <- lwd[1L]
        lwd.vertical <- lwd[-1L]
    }
    
    if(missing(col) || length(col) == 0L) {
        col <- par("col")
    }
    if(length(col) == 1L) {
        col.density <- col.vertical <- col
    } else {
        col.density <- col[1L]
        col.vertical <- col[-1L]
    }
    
    x.boxplot.stats <- boxplot(x, plot = FALSE)$stats
    # Retrieved 2014-07-22: http://stackoverflow.com/a/9122859
    x.hist <- hist(x, plot = FALSE)
    x.hist$density <- x.hist$counts / sum(x.hist$counts)
    if(plotDensityCurve) {
        x.density <- density(x)
        x.density$y <- x.density$y * diff(x.hist$breaks)[1L]
    }
    if(plotVerticalLines || plotStatsValues) {
        x.mean <- mean(x, na.rm = na.rm)
        x.median <- stats::median(x, na.rm = na.rm)
        x.lowerhinge <- stats::fivenum(x, na.rm = na.rm)[2L]
        x.upperhinge <- stats::fivenum(x, na.rm = na.rm)[4L]
    }
    
    nf <- layout(matrix(c(1, 2), nrow = 2, ncol = 1, byrow = TRUE), height = c(1, 3))
    par(mar = c(0, 4.1, 1.1, 2.1))
#     boxplot(x, frame = FALSE, axes = FALSE, horizontal = TRUE, ylim = getRounding(x, na.rm = na.rm, adj = TRUE))
    boxplot(x, frame = FALSE, axes = FALSE, horizontal = TRUE, ylim = range(x))
    if(plotVerticalLines) {
        segments(x0 = c(x.mean, x.median, x.lowerhinge, x.upperhinge),
                 y0 = rep(0L, length.out = 4L),
                 x1 = c(x.mean, x.median, x.lowerhinge, x.upperhinge),
                 y1 = rep(1L, length.out = 4L),
                 lty = lty.vertical,
                 lwd = lwd.vertical,
                 col = col.vertical)
        if(plotStatsValues) {
            oldXPD <- par()$xpd
            par(xpd = TRUE)
            text(x = c(x.median, x.lowerhinge, x.upperhinge), y = boxplotBuffer + textLineHeightFactor, labels = formatC(c(x.median, x.lowerhinge, x.upperhinge), format = "f", digits = 1L), col = col.vertical)
            text(x = c(x.median, x.lowerhinge, x.upperhinge), y = boxplotBuffer + textLineHeightFactor + textLineHeightFactor * strheight(x.median), labels = c("Q2", "Q1", "Q3"), col = col.vertical)
            par(xpd = oldXPD)
        }
    }
    
    # title(main = sprintf("Plots for %s", sQuote(dataObjectName)), col.main = col.main, cex.main = cex.main, font.main = font.main, line = line.main)
    
    par(mar = c(5.1, 4.1, 0, 2.1))
#     plot(x.hist, freq = FALSE, main = NULL, xlim = getRounding(x, na.rm = na.rm, adj = TRUE), ylim = getRounding(x.hist$density, na.rm = na.rm, adj = TRUE), xlab = sprintf("Values of %s", sQuote(dataObjectName)), ylab = "Relative Frequency")
    plot(x.hist, freq = FALSE, main = NULL, xlab = sprintf("Values of %s", sQuote(dataObjectName)), ylab = "Relative Frequency", xlim = range(x))
    if(plotDensityCurve) {
        lines(x.density$x, x.density$y, lty = lty.density, lwd = lwd.density, col = col.density)
    }
    if(plotVerticalLines) {
        abline(v = c(x.mean, x.median, x.lowerhinge, x.upperhinge),
               lty = lty.vertical,
               lwd = lwd.vertical,
               col = col.vertical)
    }
    if(plotStatsValues) {
        text(x = max(x, na.rm = na.rm) - abs(textLineWidthFactor) * strwidth(paste(formatC(x.mean, format = "f", digits = digits), " (", formatC(sd(x, na.rm = na.rm), format = "f", digits = digits), ")", sep = "")), y = max(x.hist$density, na.rm = na.rm) + c(0, -abs(textLineHeightFactor) * strheight("Mean (SD)")), labels = c("Mean (SD)", paste(formatC(x.mean, format = "f", digits = digits), " (", formatC(sd(x, na.rm = na.rm), format = "f", digits = digits), ")", sep = "")), col = col.vertical)
    }
    layout(1)
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
descriptiveStatsDF <- function(x, stats = "default", columns = "all", digits = 2L, na.rm = TRUE, silent = FALSE, quantile.probs = 0:4/4, quantile.type = 7L, keepColumnNames = TRUE, categorical.emptyCellSymbol = "", categorical.maxLevels = 10L, categorical.na.exclude = na.rm, output.showStats = "all", byFactors = NULL, ignore = NULL, output.statsAreRows = TRUE, export = FALSE, export.file = NULL, export.printToConsole = !export, plots = FALSE, plots.plotDensityCurve = TRUE, plots.plotVerticalLines = TRUE, plots.plotStatsValues = TRUE, plots.lty = c(1, 1, 2, 2, 2), plots.lwd = 2, plots.col = c("red", gray(0.7))) {
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
        bracketfree <- unbracket(x = dataObjectName)
        dataObjectName <- bracketfree[["name"]]
        columns <- bracketfree[["indices"]]
        x <- data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
        colnames(x) <- columns
        if(is.integer(bracketfree[["indices"]])) {
            oldColumnNames <- paste("Column ", columns, sep = "")
            on.exit(warning(strwrap(gettext("NOTE: The data set provided to this function was a subset of a larger data set and the columns (i.e., the variables) were specified by numeric index. Because of this, the column names in the resulting output may not accurately correspond to those in the original data set. This can be fixed by using the 'columns' argument and/or named columns (if possible) to specify the variables of interest."), width = 0.95 * getOption("width"), prefix = "\n    ", initial = ""), call. = FALSE), add = TRUE)
        } else {
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
    
    # Get plot results for continuous variables
    if(plots) {
        for(i in seq_len(NCOL(x.continuous))) {
#             getBoxHist(x = x.continuous[, i], na.rm = na.rm, dataObjectName = colnames(x.continuous)[i], plotDensityCurve = plots.plotDensityCurve, plotVerticalLines = plots.plotVerticalLines, plotStatsValues = plots.plotStatsValues, lty = plots.lty, lwd = plots.lwd, col = plots.col, digits = digits)
            getBoxHist(x = if(class(x.continuous[, i]) %in% c("Date", "POSIXt", "POSIXct", "POSIXlt")) {unclass(x.continuous[, i])} else {x.continuous[, i]}, na.rm = na.rm, dataObjectName = colnames(x.continuous)[i], plotDensityCurve = plots.plotDensityCurve, plotVerticalLines = plots.plotVerticalLines, plotStatsValues = plots.plotStatsValues, lty = plots.lty, lwd = plots.lwd, col = plots.col, digits = digits)
        }
    }
    
    # Export 'results'?
    if(export) {
        require(rtf)
        if(length(export.file) == 0L || !nzchar(export.file) || !is.character(export.file)) {
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
                rtf::addText(rtf, names(results[i]), bold = TRUE)
                rtf::addNewLine(rtf)
                for(j in seq_along(results[i][[1L]])) {
                    rtf::addText(rtf, names(results[i][[1L]])[j], italic = TRUE)
                    rtf::addNewLine(rtf)
                    rtf::addTable(rtf, results[i][[1L]][[j]])
                    rtf::addNewLine(rtf)
                }
            } else {
                rtf::addText(rtf, names(results)[i], bold = TRUE)
                rtf::addNewLine(rtf)
                rtf::addTable(rtf, results[[i]], row.names = TRUE)
                rtf::addNewLine(rtf)
            }
            rtf::addNewLine(rtf)
            rtf::addNewLine(rtf)
        }
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


# # getBoxHist <- function(x, na.rm = TRUE, dataObjectName = NULL, plotDensityCurve = TRUE, plotVerticalLines = TRUE, plotStatsValues = TRUE, lty = par("lty"), lwd = par("lwd"), col = par("col")) {
# getBoxHist <- function(x, na.rm = TRUE, dataObjectName = NULL, plotDensityCurve = TRUE, plotVerticalLines = TRUE, plotStatsValues = TRUE, lty = par("lty"), lwd = par("lwd"), col = par("col"), digits = 2L, textLineWidthFactor = 0.15 * par("cex"), textLineHeightFactor = 1.015 * par("cex"), boxplotBuffer = 0.3) {
#     def.par <- par(no.readonly = TRUE)
#     on.exit(layout(1), add = TRUE)
#     on.exit(par(def.par), add = TRUE)
#     
#     fPart <- function(x) abs(x - trunc(x))
#     
#     getRounding <- function(x, na.rm = TRUE, adj = TRUE) {
#         arx <- abs(range(x, na.rm = na.rm))
# #         larx <- log10(abs(range(x, na.rm = na.rm)))
#         larx <- log10(arx)
#         larx[!is.finite(larx) | is.na(larx)] <- 0L
# #         minplaces <- floor(larx[1L])
# # #         maxplaces <- ceiling(larx[2L])
# #         maxplaces <- floor(larx[2L])
# #         minpower <- 10^(floor(larx[1L]))
# #         maxpower <- 10^(floor(larx[2L]))
# #         mindenom <- 2^(fPart(arx[1L]/minpower)<0.5)
# #         maxdenom <- 2^(fPart(arx[2L]/maxpower)<0.5)
#         powers <- 10^(floor(larx))
# #         denoms <- 2^(fPart(arx / powers) < 0.5)
# #         denoms <- 2^(fPart(arx / powers) < log10(5))
#         denoms <- fPart(arx / powers) < 0.5 & fPart(arx / powers) > 0
# #         adj <- adj * (sign(range(x)) * powers * (denoms - 1L) / 2^(denoms - 1L))
#         adj <- adj * sign(range(x)) * powers * denoms / 2
# #         c((floor(range(x)[1L] / (10^minplaces)) * 10^minplaces), (ceiling(range(x)[2L] / (10^(maxplaces - 1L))) * 10^(maxplaces - 1L)))
# #         tentativeLimits <- c((floor(range(x)[1L] / (10^minplaces)) * 10^minplaces), (ceiling(range(x)[2L] / (10^(maxplaces - 1L))) * 10^(maxplaces - 1L)))
# #         tentativeLimits <- c((floor(range(x)[1L] / (10^minplaces)) * 10^minplaces), (ceiling(range(x)[2L] / (10^(maxplaces))) * 10^(maxplaces)))
# #         tentativeLimits <- c((floor(range(x)[1L] / minpower) * minpower), (ceiling(range(x)[2L] / maxpower) * maxpower))
# #         c((floor(range(x)[1L] / minpower) * minpower/mindenom), (ceiling(range(x)[2L] / maxpower) * maxpower/maxdenom))
#         c(floor(range(x)[1L] / powers[1L]), ceiling(range(x)[2L] / powers[2L])) * powers - adj
#     }
#     
#     if(length(dataObjectName) < 1L) {
#         dataObjectName <- deparse(substitute(x))
#     }
#     
#     if(missing(lty) || length(lty) == 0L) {
#         lty <- par("lty")
#     }
#     if(length(lty) == 1L) {
#         lty.density <- lty.vertical <- lty
#     } else {
#         lty.density <- lty[1L]
#         lty.vertical <- lty[-1L]
#     }
#     
#     if(missing(lwd) || length(lwd) == 0L) {
#         lwd <- par("lwd")
#     }
#     if(length(lwd) == 1L) {
#         lwd.density <- lwd.vertical <- lwd
#     } else {
#         lwd.density <- lwd[1L]
#         lwd.vertical <- lwd[-1L]
#     }
#     
#     if(missing(col) || length(col) == 0L) {
#         col <- par("col")
#     }
#     if(length(col) == 1L) {
#         col.density <- col.vertical <- col
#     } else {
#         col.density <- col[1L]
#         col.vertical <- col[-1L]
#     }
#     
#     x.boxplot.stats <- boxplot(x, plot = FALSE)$stats
#     # Retrieved 2014-07-22: http://stackoverflow.com/a/9122859
#     x.hist <- hist(x, plot = FALSE)
#     x.hist$density <- x.hist$counts / sum(x.hist$counts)
#     if(plotDensityCurve) {
#         x.density <- density(x)
#         x.density$y <- x.density$y * diff(x.hist$breaks)[1L]
#     }
#     if(plotVerticalLines || plotStatsValues) {
#         x.mean <- mean(x, na.rm = na.rm)
#         x.median <- stats::median(x, na.rm = na.rm)
#         x.lowerhinge <- stats::fivenum(x, na.rm = na.rm)[2L]
#         x.upperhinge <- stats::fivenum(x, na.rm = na.rm)[4L]
#     }
#     
#     nf <- layout(matrix(c(1, 2), nrow = 2, ncol = 1, byrow = TRUE), height = c(1, 3))
#     par(mar = c(0, 4.1, 1.1, 2.1))
#     boxplot(x, frame = FALSE, axes = FALSE, horizontal = TRUE, ylim = getRounding(x, na.rm = na.rm, adj = FALSE))
#     if(plotVerticalLines) {
#         segments(x0 = c(x.mean, x.median, x.lowerhinge, x.upperhinge),
#                  y0 = rep(0L, length.out = 4L),
#                  x1 = c(x.mean, x.median, x.lowerhinge, x.upperhinge),
#                  y1 = rep(1L, length.out = 4L),
#                  lty = lty.vertical,
#                  lwd = lwd.vertical,
#                  col = col.vertical)
#         if(plotStatsValues) {
#             oldXPD <- par()$xpd
#             par(xpd = TRUE)
# #             text(x = c(x.median, x.lowerhinge, x.upperhinge), y = 4/3, labels = formatC(c(x.median, x.lowerhinge, x.upperhinge), format = "f", digits = 1L), col = col.vertical)
# #             text(x = c(x.median, x.lowerhinge, x.upperhinge), y = 4.5/3, labels = c("Q2", "Q1", "Q3"), col = col.vertical)
#             text(x = c(x.median, x.lowerhinge, x.upperhinge), y = boxplotBuffer + textLineHeightFactor, labels = formatC(c(x.median, x.lowerhinge, x.upperhinge), format = "f", digits = 1L), col = col.vertical)
# #             text(x = c(x.median, x.lowerhinge, x.upperhinge), y = textLineHeightFactor * (boxplotBuffer + textLineHeightFactor), labels = c("Q2", "Q1", "Q3"), col = col.vertical)
#             text(x = c(x.median, x.lowerhinge, x.upperhinge), y = boxplotBuffer + textLineHeightFactor + textLineHeightFactor * strheight(x.median), labels = c("Q2", "Q1", "Q3"), col = col.vertical)
#             par(xpd = oldXPD)
#         }
#     }
#     
# #     title(main = sprintf("Plots for %s", sQuote(dataObjectName)), col.main = col.main, cex.main = cex.main, font.main = font.main, line = line.main)
#     
#     par(mar = c(5.1, 4.1, 0, 2.1))
#     plot(x.hist, freq = FALSE, main = NULL, xlim = getRounding(x, na.rm = na.rm, adj = FALSE), ylim = getRounding(x.hist$density, na.rm = na.rm, adj = TRUE), xlab = sprintf("Values of %s", sQuote(dataObjectName)), ylab = "Relative Frequency")
#     if(plotDensityCurve) {
#         lines(x.density$x, x.density$y, lty = lty.density, lwd = lwd.density, col = col.density)
#     }
#     if(plotVerticalLines) {
#         abline(v = c(x.mean, x.median, x.lowerhinge, x.upperhinge),
#                lty = lty.vertical,
#                lwd = lwd.vertical,
#                col = col.vertical)
#     }
#     if(plotStatsValues) {
# #         text(x = max(x, na.rm = na.rm), y = max(x.hist$density, na.rm = na.rm), labels = c("Mean (SD)", paste(formatC(x.mean, format = "f", digits = digits), " (", formatC(sd(x, na.rm = na.rm), format = "f", digits = digits), ")", sep = "")), col = col.vertical)
# #         text(x = max(x, na.rm = na.rm), y = max(x.hist$density, na.rm = na.rm), labels = expression(Mean ~ (SD) ~ \\n .(x.mean) ~ (.(sd(x, na.rm = na.rm)))), col = col.vertical)
# #         text(x = max(x, na.rm = na.rm), y = max(x.hist$density, na.rm = na.rm), labels = bquote(paste("Mean (SD)", "\n", .(formatC(x.mean, format = "f", digits = digits)), " (", .(formatC(sd(x, na.rm = na.rm), format = "f", digits = digits)), ")")), col = col.vertical)
# #         text(x = max(x, na.rm = na.rm) - 1.015 * par("cex") * strwidth(paste(formatC(x.mean, format = "f", digits = digits), " (", formatC(sd(x, na.rm = na.rm), format = "f", digits = digits), ")", sep = "")), y = max(x.hist$density, na.rm = na.rm) + c(0, -1.015 * par("cex") * strheight("Mean (SD)")), labels = c("Mean (SD)", paste(formatC(x.mean, format = "f", digits = digits), " (", formatC(sd(x, na.rm = na.rm), format = "f", digits = digits), ")", sep = "")), col = col.vertical)
# #         text(x = max(x, na.rm = na.rm) - 0.1 * par("cex") * strwidth("Mean (SD)"), y = max(x.hist$density, na.rm = na.rm) + c(0, -1.015 * par("cex") * strheight("Mean (SD)")), labels = c("Mean (SD)", paste(formatC(x.mean, format = "f", digits = digits), " (", formatC(sd(x, na.rm = na.rm), format = "f", digits = digits), ")", sep = "")), col = col.vertical)
# #         text(x = max(x, na.rm = na.rm) - 0.15 * par("cex") * strwidth(paste(formatC(x.mean, format = "f", digits = digits), " (", formatC(sd(x, na.rm = na.rm), format = "f", digits = digits), ")", sep = "")), y = max(x.hist$density, na.rm = na.rm) + c(0, -1.015 * par("cex") * strheight("Mean (SD)")), labels = c("Mean (SD)", paste(formatC(x.mean, format = "f", digits = digits), " (", formatC(sd(x, na.rm = na.rm), format = "f", digits = digits), ")", sep = "")), col = col.vertical)
#         text(x = max(x, na.rm = na.rm) - abs(textLineWidthFactor) * strwidth(paste(formatC(x.mean, format = "f", digits = digits), " (", formatC(sd(x, na.rm = na.rm), format = "f", digits = digits), ")", sep = "")), y = max(x.hist$density, na.rm = na.rm) + c(0, -abs(textLineHeightFactor) * strheight("Mean (SD)")), labels = c("Mean (SD)", paste(formatC(x.mean, format = "f", digits = digits), " (", formatC(sd(x, na.rm = na.rm), format = "f", digits = digits), ")", sep = "")), col = col.vertical)
#     }
#     layout(1)
# }
# 
# set.seed(0722)
# x <- rnorm(500, sd = 100)
# getBoxHist(x)
# getBoxHist(x, col = c("red", gray(0.2)), lty = c(1, 1, 2, 2, 2), lwd = 2)
# getBoxHist(x, col = c("red", gray(0.5)), lty = c(1, 1, 2, 2, 2), lwd = 2)
# getBoxHist(x, col = c("red", gray(0.7)), lty = c(1, 1, 2, 2, 2), lwd = 2)
# 
# par(xpd = NA)
# convfrom <- "ndc"
# convto <- "user"
# xleft <- 0
# ybottom <- 0
# xright <- 1
# ytop <- 1
# rect(xleft = grconvertX(xleft, from = convfrom, to = convto), ybottom = grconvertY(ybottom, from = convfrom, to = convto),
#      xright = grconvertX(xright, from = convfrom, to = convto), ytop = grconvertY(ytop, from = convfrom, to = convto))


    
#     getRounding <- function(x, na.rm = TRUE, adj = TRUE) {
#         fPart <- function(x) abs(x - trunc(x))
#         arx <- abs(range(x, na.rm = na.rm))
#         larx <- log10(arx)
#         larx[!is.finite(larx) | is.na(larx)] <- 0L
# #         powers <- 10^(floor(larx))
#         powers <- 10^(floor(larx) - 1L)
#         denoms <- fPart(arx / powers) < 0.5 & fPart(arx / powers) > 0
#         adj <- adj * sign(range(x)) * powers * denoms / 2
#         c(floor(range(x)[1L] / powers[1L]), ceiling(range(x)[2L] / powers[2L])) * powers - adj
#     }


# x <- DFFH$wtkg
# 
# getRounding <- function(x, na.rm = TRUE, adj = TRUE) {
#     fPart <- function(x) abs(x - trunc(x))
# #     arx <- abs(range(x, na.rm = na.rm))
# #     larx <- log10(arx)
# #     larx[!is.finite(larx) | is.na(larx)] <- 0L
# #     powers <- 10^(floor(larx) - 1L)
# #     denoms <- fPart(arx / powers) < 0.5 & fPart(arx / powers) > 0
# #     adj <- adj * sign(range(x)) * powers * denoms / 2
# #     c(floor(range(x)[1L] / powers[1L]), ceiling(range(x)[2L] / powers[2L])) * powers - adj
#     minx <- abs(range(x, na.rm = na.rm)[1L])
#     lminx <- log10(minx)
#     lminx[!is.finite(lminx) | is.na(lminx)] <- 0L
#     minpower <- 10^(floor(lminx) - 1L)
# #     mindenom <- fPart(minx / minpower) < 0.5 & fPart(minx / minpower) > 0
#     mindenom <- fPart(minx / minpower) > 0.5# & fPart(minx / minpower) < 0
#     minadj <- adj * sign(minx) * minpower * mindenom / 2
#     
#     maxx <- abs(range(x, na.rm = na.rm)[2L])
#     lmaxx <- log10(maxx)
#     lmaxx[!is.finite(lmaxx) | is.na(lmaxx)] <- 0L
#     maxpower <- 10^(floor(lmaxx) - 1L)
#     maxdenom <- fPart(maxx / maxpower) < 0.5 & fPart(maxx / maxpower) > 0
#     maxadj <- adj * sign(maxx) * maxpower * maxdenom / 2
#     
#     c(floor(minx / minpower) * minpower - minadj, ceiling(maxx / maxpower) * maxpower - maxadj)
# }





# set.seed(0724)
# x <- rnorm(500, sd = 100)
# y <- rnorm(500, sd = 0.01)
# 
# # getRounding <- function(x, na.rm = TRUE, adj = TRUE) {
# #     fPart <- function(x) abs(x - trunc(x))
# # #     arx <- abs(range(x, na.rm = na.rm))
# # #     larx <- log10(arx)
# # #     larx[!is.finite(larx) | is.na(larx)] <- 0L
# # #     powers <- 10^(floor(larx) - 1L)
# # #     denoms <- fPart(arx / powers) < 0.5 & fPart(arx / powers) > 0
# # #     adj <- adj * sign(range(x)) * powers * denoms / 2
# # #     c(floor(range(x)[1L] / powers[1L]), ceiling(range(x)[2L] / powers[2L])) * powers - adj
# #     minx <- min(x[is.finite(x)], na.rm = na.rm)
# #     aminx <- abs(minx)
# #     laminx <- log10(aminx)
# #     minpower <- 10^(floor(laminx) - 1L)
# #     mindenom <- fPart(aminx / minpower) > 0.5# & fPart(aminx / minpower) < 0
# #     minadj <- adj * sign(minx) * minpower * mindenom / 2
# #     
# #     maxx <- max(x[is.finite(x)], na.rm = na.rm)
# #     amaxx <- abs(maxx)
# #     lamaxx <- log10(amaxx)
# #     maxpower <- 10^(floor(lamaxx) - 1L)
# #     maxdenom <- fPart(amaxx / maxpower) < 0.5 & fPart(amaxx / maxpower) > 0
# #     maxadj <- adj * sign(maxx) * maxpower * maxdenom / 2
# #     
# #     c(floor(minx / minpower) * minpower - minadj, ceiling(maxx / maxpower) * maxpower - maxadj)
# # }
# 
# x.hist <- hist(x, plot = FALSE)
# hist(y, axes = FALSE)
# axis(side = 1, labels = TRUE)
# 
# 
# set.seed(0724)
# x<-rnorm(30, sd = 100)
# x<-rnorm(30, sd = 0.01)
# # x <- unclass(DFFH$doa)
# 
# 
# roundNearestDigit <- function(x, ndigits = 1L, special = NULL) {
#     fPart <- function(x) abs(x - trunc(x))
# #     special <- match.arg(arg = special, choices = c("range", "minimum", "maximum", "none"), several.ok = TRUE)
#     special <- match.arg(arg = special, choices = c("range", "minimum", "maximum", "all"), several.ok = TRUE)
#     if(length(special) > 1L && "range" %in% special) {
#         special <- "range"
#     } else {
#         special <- special[1L]
#     }
# #     x[!is.finite(x) | is.na(x)] <- 1L
#     x <- switch(special,
#                 "minimum" = min(x[is.finite(x) & !is.na(x)]),
#                 "maximum" = max(x[is.finite(x) & !is.na(x)]),
#                 "range" = range(x[is.finite(x) & !is.na(x)]),
#                 "all" = x[is.finite(x) & !is.na(x)],
#                 stop("invalid 'special' selection"))
#     absx <- abs(x)
#     logabsx <- log10(absx)
#     logabsx[!is.finite(logabsx) | is.na(logabsx)] <- 0L
#     powerOfTen <- floor(logabsx)
#     adjustedPowerOfTen <- powerOfTen + 1L - abs(as.integer(ndigits))
#     scaledx <- x / (10^adjustedPowerOfTen)
# #     return(scaledx)
# #     roundedx <- switch(special,
# #                        "minimum" = floor(scaledx) * 10^adjustedPowerOfTen,
# #                        "maximum" = ceiling(scaledx) * 10^adjustedPowerOfTen,
# #                        "range" = list("Minimum" = floor(scaledx) * 10^adjustedPowerOfTen, "Maximum" = ceiling(scaledx) * 10^adjustedPowerOfTen),
# #                        "none" = round(scaledx, digits = 0L) * 10^adjustedPowerOfTen,
# #                        stop("invalid 'special' selection"))
# #     roundedx <- switch(special,
# #                        "minimum" = floor(min(scaledx)) * 10^adjustedPowerOfTen,
# #                        "maximum" = ceiling(max(scaledx)) * 10^adjustedPowerOfTen,
# #                        "range" = c(floor(min(scaledx)) * 10^adjustedPowerOfTen, ceiling(max(scaledx)) * 10^adjustedPowerOfTen),
# #                        "all" = round(scaledx, digits = 0L) * 10^adjustedPowerOfTen,
# #                        stop("invalid 'special' selection"))
#     roundedx <- switch(special,
#                        "minimum" = floor(scaledx) * 10^adjustedPowerOfTen,
#                        "maximum" = ceiling(scaledx) * 10^adjustedPowerOfTen,
#                        "range" = c(floor(scaledx[1L]) * 10^adjustedPowerOfTen[1L], ceiling(scaledx[2L]) * 10^adjustedPowerOfTen[2L]),
#                        "all" = round(scaledx, digits = 0L) * 10^adjustedPowerOfTen,
#                        stop("invalid 'special' selection"))
#     return(roundedx)
# }





