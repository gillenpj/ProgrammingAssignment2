# Sample usage:
# 
# > if(exists("x")) rm(x)
# > x <- matrix(c(1, 2, 2, 1), byrow = 2, nrow = 2)
# > x <- makeCacheMatrix(x)
# > cacheSolve(x)
#            [,1]       [,2]
# [1,] -0.3333333  0.6666667
# [2,]  0.6666667 -0.3333333
# > cacheSolve(x)
# getting cached data
#            [,1]       [,2]
# [1,] -0.3333333  0.6666667
# [2,]  0.6666667 -0.3333333
# 

# Creates a special "matrix", which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) xinv <<- inv
        getinv <- function() xinv
        
        # Returns a special "matrix"
        
        retval <- list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        retval$set(x)
        retval
}


# Calculates the inverse of the special "matrix" created with the above 
# function. However, it first checks to see if the iverse has already been 
# calculated. If so, it gets the inverse from the cache and skips the 
# computation. Otherwise, it calculates the inverse of the matrix and sets the 
# value of the inverse in the cache via the setinv function

cacheSolve <- function(x, ...) {
        xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        xinv <- solve(x$get())
        x$setinv(xinv)
        
        ## Return a matrix that is the inverse of 'x'

        xinv
}