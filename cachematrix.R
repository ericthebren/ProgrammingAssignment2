##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix
##get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(inverse) m <<- inverse
        getinvmatrix <- function() m
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}
##The following function calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse of matrix has already been calculated. If so, it gets the inverse of matrix from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the inverse of matrix in the cache via the setinvmatrix function.

cacheSolve <- function(x, ...) {
        m <- x$getinvmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        z <- x$get()
        ##Solve fun     ction instead of inverse.  That took me forever to figure out.
        m <- solve(z, ...)
        x$setinvmatrix(m)
        m
}

##How To Use This
## x<- rbind(c(1, 2), c(3, 2))
## m = makeCacheMatrix(x)
## cacheSolve(m)