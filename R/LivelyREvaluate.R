require(evaluate)
require(rjson)
require(parallel)
require(tools)

# helper for interactive package development
.reload <- function(path) {
    name = "LivelyREvaluate"
    packageName = paste("package", name, sep=":")
    detach(packageName, unload = TRUE,character.only=TRUE)
    #library.dynam.unload(name, system.file(package = name))
    path <- paste( "--vanilla  CMD INSTALL ", path )
    system2( 'R', path  )
    require(name, character.only=TRUE)
}

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# state
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# inserted as last statement into evaluation
.endMarker <- '"!@DONE@!"'

# .evalResults is a hashmap of evalStates that represent finished evaluations
.evalResults <- new.env(hash=T, parent=emptyenv()) # hashmap

# .evalProcs is a hashmap of running eval process. when getEvalResult() is
# called we collect the finished processes, remove the from .evalProcs, and put
# their results in .evalResults
.evalProcs <- new.env(hash=T, parent=emptyenv())

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# private helpers
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# the evalState represents a running or finished evaluation. It is a pairlist
# with the following fields:
#   id - identifier for evaluation
#   source - code that is evaluated
#   statementIndex - internal state used by our evaluate::output_handler to
#                    track the progress of eval results
#   result - data.frame that holds on to the different types of evaluation
#            output. see evaluate::new_output_handler and .evaluateString() for
#            more details
.createEvalState <- function(id="", source="", env=parent.frame()) {
    list(id=id,
         source=source,
         statementIndex=0,
         stopCause=NULL,
         env=env,
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

.clearEvalResults = function() {
    remove(list=ls(.evalResults), envir=.evalResults)
}

# helper for collecting results from an eval process
# if there is no process for the given id, return NULL.
# try to fetch a result from the process.  if none available return NULL.
# results are sent using sendMaster(evalState), so the value returned by
#   mccollect() is a list of evalState objects (or an error)
# if the result indicates an error, remove the process and return the error.
# 
.collectEvalProc = function(id) {
    forgetProc = function() { remove(list=c(id), envir=.evalProcs) }

    proc = .evalProcs[[id]]
    if (is.null(proc)) return(NULL) # no eval process with this id running
    results <- parallel::mccollect(proc, wait=FALSE)
    if (inherits(results,"try-error")) {
      warning(paste0("error result from parallel eval process: ", results))
      forgetProc()
      return(results)
    }
    evalState <- results[[1]]
    if (is.null(evalState)) return(NULL)   # no result available (yet)
    .mergeEnvironments(globalenv(), evalState$env)
    lastResultRow <- nrow(evalState$result)
    if (!is.na(evalState$result[lastResultRow, 'error'])) {
      evalState$stopCause = "ERROR"
      forgetProc()
    } else if (evalState$result[lastResultRow,'source'] == .endMarker) {
      evalState$result = evalState$result[-lastResultRow,]
      evalState$stopCause = "COMPLETE"
      forgetProc()
    }
    return(evalState)
}

# rk's version
# .collectEvalProc = function(id) {
#   proc = .evalProcs[[id]]
#   if (is.null(proc)) return(NULL) # no eval process with this id running
#   result <- parallel::mccollect(proc, wait=FALSE)
#   if (is.null(result)) return(NULL) # process not yet done
#   remove(list=c(id), envir=.evalProcs)
#   if (inherits(result,"try-error")) {
#     warning(paste("result of parallel eval process is an error", result))
#     return(result)
#   } else {
#     evalState = result[[1]]
#     .mergeEnvironments(globalenv(), evalState$env)
#     evalState$interrupted = evalState$result[nrow(evalState$result),'source'] != .endMarker
#     if (!evalState$interrupted) evalState$result = evalState$result[-nrow(evalState$result),]
#     return(evalState)
#   }
# }

.mergeEnvironments = function(baseEnv, envToMerge) {
    for (name in ls(envToMerge, all.names=TRUE)) {
        if (exists(name, envir=envToMerge, inherits=FALSE)) {
            value = get(name, envir=envToMerge)
            if (!exists(name, envir=baseEnv) || !identical(value, get(name, envir=baseEnv))) {
                message(paste0('assigning ', name))
                assign(name, value, envir=baseEnv)
            }
        }
    }
}

.whenEvalFinished = function(evalState) {
    # this is triggered by
    #  a. orderly exit from a parallel procedure
    #  b. forced exit (using stopEvaluation)
    # we are not responsible for clearing up the stored procedure and
    # any results; this will be done when .collectEvalProc is next called
    parallel:::sendMaster(evalState)
    message('Eval is done')
    quit(save = "no", status = 0, runLast = FALSE)
}

# here we start the evaluation and define the output_handler used by
# evaluate::evaluate. Returns an evalState 
.evaluateString <- function(evalId, string, exit=FALSE, envir=parent.frame()) {
    evalState = .createEvalState(id=evalId, source=string, env=envir)
    .evalResults[[evalId]] <- evalState
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
        # ael: each value- or message-bearing result causes immediate sending
        # of all results obtained since the last time we sent.
        # it assumes that all other fields of the latest result are ready (ok?)
        if (type == "value" || type == "message") {
            parallel:::sendMaster(evalState)
            evalState$result <<- evalState$result[FALSE,]   # wipe clean
            evalState$statementIndex <<- 0
        }
    }
    evalHandler <- evaluate::new_output_handler(
        source = function(x){ recordResult('source', x); },
        text = function(x){ recordResult('text', x); },
    #    graphics = function(x){ recordResult('graphics', x); },
        message = function(x){ recordResult('message', x); },
        warning = function(x){ recordResult('warning', x); },
        error = function(x){ recordResult('error', x); },
        value = function(val){ recordResult('value', val); return(val) })
    evaluate::evaluate(
        paste(string, .endMarker, sep="\n"),
        envir=envir,
        stop_on_error=1,
        output_handler=evalHandler)
    return(evalState)
}

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# accessors
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

.getEvalProcessState <- function(id) {
    #   - no proc => UNKNOWN
    #   - eval stopped => COMPLETE/INTERRUPT/ERROR
    #   - eval running => RUNNING
    proc <- .evalProcs[[id]]
    if (is.null(proc)) return('UNKNOWN')
    state <- .evalResults[[id]]
    if (!is.null(state$stopCause)) return(state$stopCause)
    return('RUNNING')
}
# .getEvalProcessState <- function(id,result) {
#   if (!is.null(result)) {
#     if (result$interrupted) return('INTERRUPTED')
#         else return('DONE')
#   }
#   if (!is.null(.evalProcs[[id]])) return('PENDING')
#   return('NOTEXISTING')
# }

getEvalResults <- function() {
    # returns every eval result we have gathered so far
    lapply(ls(.evalResults), getEvalResult)
}

# return a specific eval result
getEvalResult <- function(id,         # id under which the eval was started
                         format="R", # c("R","JSON")
                         file=NULL   # optionally write result into file
                         ) {
    # possible states to be handled:
    #   - no proc => UNKNOWN
    #   - mcollect gives error => <error>
    #   - mccollect returns NULL, eval was running => { PENDING, NULL }
    #   - mccollect returns NULL, eval was stopped => { stopCause, NULL }
    #   - mccollect returns empty non-NULL result => { stopCause, NULL }
    #   - mccollect returns non-empty result => { PARTIAL, res }
    runningState <- .getEvalProcessState(id)     # before calling mccollect
    if (runningState == 'UNKNOWN') {
      result = list(processState='UNKNOWN', result=NULL)
    } else {
      newEvalState <- .collectEvalProc(id)
      if (inherits(newEvalState,"try-error")) {
        res <- toString(newEvalState)
      } else if (is.null(newEvalState)) {
        res <- NULL
        if (runningState == 'RUNNING') { processState <- 'PENDING'
          } else { processState <- runningState }
      } else {  # found a new result (though perhaps just an empty one)
        # if we already had a stopCause, don't overwrite it
        oldStop <- .evalResults[[id]]$stopCause
        if (!is.null(oldStop)) newEvalState$stopCause <- oldStop
        .evalResults[[id]] <- newEvalState
        res <- newEvalState$result
        if (nrow(res) == 0) {
          res <- NULL
          processState <- newEvalState$stopCause
        } else { processState <- 'PARTIAL' }
      }
      result <- list(
            processState=processState,
            result=res)
    }
    if (format == "JSON") result = rjson::toJSON(result)
    if (!is.null(file)) write(result, file = file) # optional file output
    result
}

# getEvalResult <- function(id,         # id under which the eval was started
#                           format="R", # c("R","JSON")
#                           file=NULL   # optionally write result into file
# ) {
#   evalState = .evalResults[[id]] # do we already have a result?
#   if (is.null(evalState)) { # eval still running, try collect result
#     evalState = .collectEvalProc(id)
#     if (!is.null(evalState)) .evalResults[[id]] = evalState
#   }
#   result = NULL
#   if (inherits(evalState,"try-error")) {
#     result = toString(evalState)
#   } else {
#     result = list(
#       processState=.getEvalProcessState(id,evalState),
#       result=evalState$result)
#   }
#   if (format == "JSON") result = rjson::toJSON(result)
#   if (!is.null(file)) write(result, file = file) # optional file output
#   result
# }

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# evaluation control
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

evaluate <- function(id, string, envir=environment()) {
    # evaluate function, main entry point
    .evalProcs[[id]] = parallel::mcparallel(
        .evaluateString(id, string, exit=TRUE, envir=envir),
        name=id)
}

evaluateToJSON <- function(json) {
    # json = {id: STRING, source: STRING}
    jso = as.environment(rjson::fromJSON(json)) # make it a hash
    evaluate(jso$id, jso$source)
}

stopEvaluation <- function(id) {
    proc = .evalProcs[[id]]
    if (!is.null(proc)) {
      .evalResults[[id]]$stopCause <- 'INTERRUPT'
      tools::pskill(proc$pid, signal=SIGINT)
    }
}
