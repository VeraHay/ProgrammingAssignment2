##  Above two functions cache the inverse of a matrix
##  to use the cached value when available.


makeCacheMatrix <- function(x = matrix()) {

##  This function provides options to 
##         set the value of the matrix;
##         get the value of the matrix;
##         set the value of the inverse of the matrix;
##         get the value of the inverse of the matrix;

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}




cacheSolve <- function(x, ...) {

##  This function computes the inverse of the matrix returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()

        i <- solve(data, ...)
        x$setmean(i)
        i
}