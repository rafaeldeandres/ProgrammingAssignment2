## The following functions aim to cache the inverse of a given square invertible matrix to avoid
## repeatedly calculating it

## This first function is used to create a 'special' matrix object that can cache its inverse
## It builds a set of functions and returns the functions within a list to the parent environment

makeCacheMatrix <- function(x = matrix()) {
	## x has to be a square invertible matrix
	i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function is used to obtain the inverse of the 'special' matrix
## It requires an argument that is returned by makeCacheMatrix() to retrieve the inverse 
## from the cached value that is stored in the makeCacheMatrix() environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
        if(!is.null(i)){
                message("getting cached data") 
                return(i)
        }
        mx <- x$get()
        i <- solve(mx, ...)
        x$setinv(i)
        i
}

## In order to make it work, store makeCacheMatrix(x) into a variable a and then do cacheSolve(a)