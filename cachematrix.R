## Functions makeCacheMatrix and cacheSolve work together to
## preserve state inside of an R object. This allows us to 
## cache the time-consuming computation of inverting a matix.

## The function makeCacheMatrix creates a special "matrix"
## which is really a list containing functions to:
## 1. set the value of the matrix (set)
## 2. get the value of the matrix (get)
## 3. set the inverse of the matrix (setinverse)
## 4. get the inverse of the matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL
        ## create the list of functions
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        ## return the list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## The function cacheSolve calculates the inverse of the "matrix"
## created by makeCacheMatrix. It first checks if the inverse has
## already been calculated. If so, it get the inverse from the
## cache and skips the computation. Otherwise, it calculates the
## inverse and sets the inverse in the cache via setinverse.
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## cache was null so compute and return the inverse 
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
