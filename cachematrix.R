## these functions are for special "matrix" type.
## this special matrix cache its inverse matrix,
## so can save time for repetitive inverse matrix calculation.
## usage:
##  x <- makeCacheMatrix(matrix(1:4, 2, 2))
##  inverseX <- cacheSolve()


## make special matrix type that cache its inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    i = NULL
    
    set <- function(newMatrix) {
        x <<- newMatrix
        i <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inverseMatrix) i <<- inverseMatrix
    getInverse <- function() i
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## calculate inverse matrix for special matrix type.
## if x has cached inverse matrix, return it.
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}