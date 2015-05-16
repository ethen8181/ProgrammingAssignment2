# -----------------------------------------------------------------------------------
# Matrix inversion is usually a costly computation, so there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly.
# The assignment is to write a pair of functions that cache the inverse of a matrix.
# -----------------------------------------------------------------------------------

# ----------------------------------------------------------------------
# the solve function with matrix, which returns the inverse of a matrix
# can also be used to solve equations, examples below
A  <- matrix( 1:4, nrow = 2 )
solve(A) 

b  <- c( 2, 2 )
solve( A, b )
# -----------------------------------------------------------------------


# ----------------------------------------------------------------------------
# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# ----------------------------------------------------------------------------
makeCacheMatrix <- function( x = matrix() ) 
{
    m <- NULL
    # set : assign a new matrix
    set <- function(y)
    {
        x <<- y 
        m <<- NULL
    }    
    
    # get : returns the original matrix
    get <- function() x
    
    # setinverse : calculate the inverse of the matrix and pass it to m
    setinverse <- function(inverse)
    {
        m <<- inverse   
    } 
    
    # getinverse : returns the inversed matrix m
    getinverse <- function() m  
    
    # form the four functions into a list
    # can be called using makeCacheMatrix(x)$
    list( set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}


# -----------------------------------------------------------------------------------
# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated and the matrix has not changed, 
# then the cachesolve should retrieve the inverse from the cache.
# -----------------------------------------------------------------------------------
cacheSolve <- function( x, ... ) 
{
    # see if the inverse has already been calculated. 
    # If so, it gets the inverse from the cache and skips the computation
    m <- x$getinverse()
    
    if( !is.null(m) ) 
    {
        message("getting cached inverse matrix")  # print it as a message 
        return(m)
    }
    
    # Otherwise, it calculates and sets the inverse using setinverse function
    matrix1 <- x$get()
    
    m <- solve( matrix1, ... )
    
    x$setinverse(m)
    m
}

# trying the function out
test <- makeCacheMatrix(A)
cacheSolve(test)


