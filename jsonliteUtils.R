## Creates a list and post processess it to remove nulls

js = function(x)jsonlite::toJSON(x,auto_unbox=TRUE, pretty=FALSE)


vector.q = function(arg){
    arg.vec = unlist(arg)
    if(length(arg.vec)>1) list("$in" = arg.vec) else arg.vec
}

list.rn = function(...){
    res = do.call(list, list(...))
    nulls = sapply(res, function(x)is.null(x))
    null.pos = which(nulls)
    sapply(rev(null.pos), function(p) res[[p]] <<- NULL)
    res
}



## res = list.rn(1, 2, NULL, 3, list(x = 4), NULL)

## vector.query places "$in$ in query if the argment is not a single item

## list.rn removes any NULL elements, which allows optional components in the query

## Example:

## con$aggregate(
##         js(
##             list(list(
##                 "$match" = list.rn(
##                     "target" = if(is.null(target)) NULL else vector.q(target),
##                     "year" = vector.q(year),
##                     "systemName" = vector.q(systemName))))))
##

## This query can accommodate the the case of the target being NULL, a singleton or a veector or targets
