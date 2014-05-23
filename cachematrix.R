## Put comments here that give an overall description of what your
## functions do

# This fuction explains the use of lexical scoping rules of the R Language.
# With the help of scoping rules we can cache the time consuming computations.
# In this function we had created a matrix and calculated its inverse and cached the value of inverse .
# So when we need that same inverse value we can directly look up to the cache instead of computing it again .

## Write a short comment describing this function

# The function makeCacheMatrix() creates a matrix, which is really a list containg a function to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse matrix
# 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m<<- NULL
        }
        
        get <- function() x
        setinv <- function(inv) m<<- inv
        getinv <- function()m
        list(set = set, get = get, setinv = setinv, getinv = getinv)

       

}


## Write a short comment describing this function

# The function cacheSolve() calculates the inverse of the matrix created by the above function.
# First it checks that if the inverse of the matrix has already been calculated.
# If so, it gets the inverse value of the matrix from the cache .
# And if not, then it calculates the inverse with solve() function and sets the value of inverse in cache with setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cache data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m

  
}
