## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Creates getters and setters for the matrix and the matrix inverse
# if no matrix is provided, an empty matrix is created and the cached inverse
# is null
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# Checks for the cached inverse in the provided cacheMatrix and returns it,
# else it calculates the inverse using solve and caches it in x, the cacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

