###################
# makeCacheMatrix # This function creates a special "matrix" object that can cache its inverse.
###################
makeCacheMatrix <- function(x = matrix()) {

# x must be a square invertible matrix
# return: a list containing functions to
#1 set the matrix
#2 get the matrix
#3 set the inverse
#4 get the inverse
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##############
# cacheSolve # This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##############
cacheSolve <- function(x, ...) {

# Return a matrix that is the inverse of 'x'
# x: output of makeCacheMatrix()
# return: inverse of the original matrix input to makeCacheMatrix()
        
        m <- x$getinv()
        if(!is.null(m)) { #If the inverse has already been calculated, then the cachesolve retrieves the inverse from the cache.
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
























