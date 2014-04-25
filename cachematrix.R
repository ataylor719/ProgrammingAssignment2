## makeCacheMatrix gets the inverse of a matrix and stores
## or caches it outside its own environment so it can be
## used in cacheSolve instead of recalculating.

## makeCacheMatrix takes a matrix and caches it in a list

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve uses the list created by makeCacheMatrix
## return a cached inverse matrix or calculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message ("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
