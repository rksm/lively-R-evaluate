# lively-R-evaluate

lively-R-evaluate provides support for R code evaluation for [Lively Kernel / Lively Web](https://github.com/LivelyKernel/LivelyKernel).
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

expr | source            | value| text      | graphics   | message                                    | warning | error
---  | ----------------- | -----   | ----   | --------   | ------------------------------------------ | ------- | -----
1    | message("hello!") |  \<NA\> | \<NA\> |     \<NA\> | simpleMessage in message("hello!"): hello! |  \<NA\> |  \<NA\>
2    |               1+4 |     5   | \<NA\> |     \<NA\> |                                     \<NA\> |  \<NA\> |  \<NA\>


## evaluateToJSON(json)

Call evaluate by specifying id and source with JSON and return the result as JSON

### Arguments

- json - `{id: STRING, source: STRING}`

## stopEvaluation(id)

Interrupts an evaluation process.

### Arguments

- id - The process name/identifier (id given by user, not pid!)

## getEvalResult(id)

Returns the evaluation result.

### Arguments

- id - The eval identifier

### Example

See `evaluate()`

## getEvalResults()

Returns all evaluation results.

## getEvalProcessState(id)

returns `"DONE"`, `"PENDING"`, or `"NONEXISTING"`

### Arguments

- id - The eval identifier


# License

[MIT license](LICENSE)
