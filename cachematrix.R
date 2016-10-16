## Put comments here that give an overall description of what your
## functions do:

## for this assignment we're assuminng that the matrix supplied is always invertible.


## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    nvrse <- NULL
    set <- function(y) {
        x<<-y
        nvrse<<-NULL
    }
    get <- function() x
    setInverse <- function(solve) nvrse <<- solve
    getInverse <- function() nvrse
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then `cacheSolve` should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    nvrse <- x$getInverse()
  if(!is.null(nvrse)){
        message("getting cached data")
        return(nvrse)
    }
    data <- x$get()
    nvrse <- (solve(data, ...))
    x$setInverse(nvrse)
    nvrse
}

