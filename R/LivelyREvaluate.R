require(evaluate)
require(rjson)
require(parallel)
require(tools)

# helper for interactive package development
reload <- function( path ) {
    name = "LivelyREvaluate"
    packageName = paste("package", name, sep=":")
    detach(packageName, unload = TRUE,character.only=TRUE)
    #library.dynam.unload(name, system.file(package = name))
    path <- paste( "--vanilla  CMD INSTALL ", path )
    system2( 'R', path  )
    require(name, character.only=TRUE)
}

# inserted as last statement into evaluation
endMarker <- '"!@DONE@!"'

# evalResults is a hashmap of evalStates that represent finished evaluations
evalResults <- new.env(hash=T, parent=emptyenv()) # hashmap

# evalProcs is a hashmap of running eval process. when getEvalResult() is
# called we collect the finished processes, remove the from evalProcs, and put
# their results in evalResults
evalProcs <- new.env(hash=T, parent=emptyenv())

# the evalState represents a running or finished evaluation. It is a pairlist
# with the following fields:
#   id - identifier for evaluation
#   source - code that is evaluated
#   statementIndex - internal state used by our evaluate::output_handler to
#                    track the progress of eval results
#   result - data.frame that holds on to the different types of evaluation
#            output. see evaluate::new_output_handler and .evaluateString() for
#            more details
createEvalState <- function(id="", source="") {
    list(id=id, source=source, statementIndex=0,
         result=data.frame(
            source=character(),
            value=character(),
            text=character(),
            graphics=character(),
            message=character(),
            warning=character(),
            error=character(),
            stringsAsFactors=FALSE))
}

clearEvalResults = function() {
    remove(list=ls(evalResults), envir=evalResults)
}

# returns every eval result we have gathered so far
getEvalResults = function() {
    lapply(ls(evalResults), getEvalResult)
}

# helper for collecting a eval process
collectEvalProc = function(id) {
    proc = evalProcs[[id]]
    if (is.null(proc)) return(NULL)
    result = parallel::mccollect(proc, wait=FALSE)
    message(paste("Collecting:", toString(result)))
    if (!is.null(result)) remove(list=c(id), envir=evalProcs)
    if (inherits(result,"try-error")) result
    else result[[1]]
}

killEvalProc = function(id) {
    proc = evalProcs[[id]]
    if (!is.null(proc)) tools::pskill(proc$pid)
}

# return a specific eval result
getEvalResult = function(id, format="R", file=NULL) {
    result = evalResults[[id]]
    if (is.null(result)) {
        result = collectEvalProc(id)
        if (!is.null(result)) evalResults[[id]] = result
    }
    if (inherits(result,"try-error")) result = toString(result)
    else result = result$result
    if (format == "JSON") result = rjson::toJSON(result)
    if (!is.null(file)) {
        write(result, file = file)
    }
    result
}

isDone = function(id) {
    result = getEvalResult(id)
    !is.null(result) && (inherits(result, "try-error") || endMarker %in% result$result$source)
}

.whenEvalFinished = function(evalResult) {
    parallel:::sendMaster(evalResult)
    quit(save = "no", status = 0, runLast = FALSE)
}

# here we start the evaluation and define the output_handler used by
# evaluate::evaluate. Returns an evalState 
.evaluateString <- function(evalId, string, exit=FALSE) {
    evalState = createEvalState(id=evalId, source=string)
    if (exit) {
        # when we are evaluating in a separate process make sure that we quit
        # the process when eval is done
        on.exit(do.call(".whenEvalFinished", list(evalState)))
    }
    recordResult = function(type, value) {
        if (type == "source") {
            evalState$statementIndex <<- evalState$statementIndex + 1
        }
        evalState$result[evalState$statementIndex, type] <<- toString(value)
    }
    evalHandler <- evaluate::new_output_handler(
        source = function(x){ recordResult('source', x); },
        text = function(x){ recordResult('text', x); },
    #    graphics = function(x){ recordResult('graphics', x); },
        message = function(x){ recordResult('message', x); },
        warning = function(x){ recordResult('warning', x); },
        error = function(x){ recordResult('error', x); },
        value = function(val){ recordResult('value', val); return(val) })
    evaluate:::evaluate(
        paste(string, endMarker, sep="\n"),
        stop_on_error=1,
        output_handler=evalHandler)
    return(evalState)
}


# synchronous eval
evaluateString <- function(evalId, string) {
    evalResults[[evalId]] = .evaluateString(evalId, string)
}

# asynchronous eval
evaluateStringParallel <- function(id, string) {
    evalProcs[[id]] = parallel::mcparallel(
        .evaluateString(id, string, exit=TRUE),
        name=id)
}

evaluateJSON <- function(json,fork=TRUE) {
    # json = {id: STRING, source: STRING}
    jso = as.environment(rjson::fromJSON(json)) # make it a hash
    if (fork) {
        evaluateStringParallel(jso$id, jso$source)
    } else {
        evaluateString(jso$id, jso$source)
    }
}
