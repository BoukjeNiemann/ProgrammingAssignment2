## The functions makeCacheMatrix and cacheSolve are used to calculate and cache the inverse of a matrix.
## If the inverse is allready calculated, the solution is obtained from the cache.
## It is assumed that the input matrix is invertible.
##
## The function cacheSolve makes use of the outcome of the function makeCacheMatrix to either
## calculate the inverse of the matrix or obtain the calculated result from the environment of makeCacheMatrix.
## Therefore you must use it like this:
## x <- <matrixdefinition>
## p <- makeCacheMatrix(x)
## cacheSolve(p) will calculate it for the first time and 
## store the result in the invironment of makeCacheMatrix(x).
## If you ask cacheSolve(p) again it will obtain the result from the cache, 
## which will be proved by the output comment 'getting cached data'.

## makeCacheMatrix returns a list of 4 functions and a memory environment for two objects inv and x
## which are bound to the environment of makeCacheMatrix and are used to store the inputmatrix x and it's
## inverse matrix inv
## the inv is only reset to NULL in this function, the actual calculation and storing of the inverse is done 
## by the function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inversematrix) inv <<- inversematrix
    getinv <- function() inv
    list(set=set,get=get,
         setinv=setinv,
         getinv=getinv)
}


## cacheSolve is a function which expects input of the type makeCacheMatrix(x)
## where x is a matrix, the extra parameters ... can be used as extra parameters for the function solve
## cacheSolve will determine if it has allready calculated the inverse for this (unchanged) matrix,
## if so it will just return the stored result,
## if not the inverse is calculated by the function solve and the result is stored in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
