## makeCacheMatrix(): This function creates a special "matrix" object that 
## can cache its inverse. 
## INPUT: invertible matrix
## OUTPUT: list of four elements containing each one function to 
## (a) set the value of the matrix
## (b) get the value of the matrix
## (c) calculate the inverse of the matrix
## (d) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## define set() function
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## define get() function: returns input
        get <- function() x
        ## define setinv() function: calculates inverse of matrix using solve()
        ## and stores result in cache
        setinv <- function(solve) i <<- solve
        ## define getin() function: returns inverse if calculated before 
        ## using cacheSolve() 
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve(): This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix() above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve retrieves the inverse from 
## the cache.
## INPUT: special "matrix" created by makeCacheMatrix()
## OUTPUT: inverted matrix of matrix input in makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## check if matrix is already in the cache
        i  <- x$getinv()
        if(!is.null(i)) {
                ## if yes, print message and return cached inverted matrix 
                message("getting cached data")
                return(i)
        }
        ## if not, get matrix, calculate its inverse, set value in cache and 
        ## return cached inverted matrix
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
