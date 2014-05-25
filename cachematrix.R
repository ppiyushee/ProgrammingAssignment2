## overall description of Functions

# These fucntions explains the use of lexical scoping rules of R Language.
# With the help of the scoping rules we can cache the time consuming computations to be used later.

# In the following functions, we create a matrix ,calculate its inverse and cache the value of inverse .
# So when the same inverse is required, its taken from the cache instead of computing it again .


## Short comments -  makeCacheMatrix function

# The function makeCacheMatrix() creates a matrix, which is really a list containg functions to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse of the matrix
# 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        #used to set the value of the inverse of the matrix
        m <- NULL
        
        #set the matrix
        set <- function(y) {
                x <<- y
                m<<- NULL
        }
        
        #get the matrix
        get <- function() x
        
        #set m as the inverse of the matrix 
        setinv <- function(inv) m<<- inv
        
        #get the inverse of the matrix 
        getinv <- function()m
        
        #return the matrix with functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


##Short comment describing cacheSolve function

# The function cacheSolve() calculates the inverse of the matrix created by the above function.
# First it checks that if the inverse of the matrix has already been calculated.
# If so, it gets the inverse value of the matrix from the cache .
# And if not, then it calculates the inverse with solve() function and sets the value of inverse in cache with setinv function.

cacheSolve <- function(x, ...) {

        #gets the value of m set by makeCacheMatrix of matrix x        
        m <- x$getinv()
        
        #if inverse of the matrix already calculated and cached get the inverse from the cached value 
        if(!is.null(m)) {
                message("getting cache data")
                return(m)
        }
        
        #if inverse not calculated earlier then calculate the inverse
        data <- x$get()
        m <- solve(data,...)
        
        #set/cache the inverse of matrix 
        x$setinv(m)
        m
}