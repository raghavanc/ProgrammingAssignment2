## function Caches the Inverse of a Matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    x <- solve(x)
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    list(set = set, get = get)
}

## Return a matrix that is the inverse of 'x'
## Function used to validate the result
cacheSolve <- function(x, ...) {
    m <- x$get() 
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    x <- solve(x)
    x$set(m)
    m
}
