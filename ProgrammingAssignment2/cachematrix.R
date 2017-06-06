## The following two functions will allow for the inverse of a matrix to be 
## calculated and stored in cache.


## The following function will create a list
# First element will set the matrix
# Second will get the matrix
# Third will set the inverse
# Fourth will get the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## The following function will find the inverse of the matrix that was set 
## above.  First it will check to see if the inverse was found.  If not, it 
## will find the inverse and the set it in the cache.

cacheSolve <- function(x, ...) {
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
