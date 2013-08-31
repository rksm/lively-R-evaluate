# LivelyREvaluate

LivelyREvaluate provides support for R code evaluation for [Lively Kernel / Lively Web](https://github.com/LivelyKernel/LivelyKernel).
It defines the R API that is called by the [Lively RServer](https://github.com/LivelyKernel/RServer).

# API

## evaluate(id, sourceString)

Start a new evaluation under the given id.

### Arguments

- id - String to identify the evaluation process
- sourceString - source code to evaluate

### Example

    require(LivelyREvaluate)
    evaluate('test', '1+4')
    # evaluation happens in asynchronously in subprocess, so wait a bit
    Sys.sleep(0.2)
    getEvalResult('test')

The evaluation result will the be a pairlist with the fields `interrupted=FALSE` and
`result=`

| |            source|value|text|graphics|                                       message|warning|error|
|-|------------------|-----|----|--------|----------------------------------------------|-------|-----|
|1| message("hello!")| <NA>|<NA>|    <NA>|simpleMessage in message("hello!"): hello!\n\n|   <NA>| <NA>|
|2|               1+4|    5|<NA>|    <NA>|                                          <NA>|   <NA>| <NA>|



## evaluateToJSON(json)

`json = {id: STRING, source: STRING}`

## stopEvaluation(id)

## getEvalResult(id)

## getEvalResults()

## getEvalProcessState(id)

returns `"DONE"`, `"PENDING"`, or `"NONEXISTING"`


# License

[MIT license](LICENSE)
