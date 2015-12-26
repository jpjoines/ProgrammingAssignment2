# Creates functions to store a matrix and it's inverse in and retrieve them fromthe calling environment.
# Returns a list of these functions.

makeCacheMatrix <- function( x = matrix() )
{
    m <- NULL
    
    set <- function( y )
    {
        x <<- y
        m <<- NULL
    }
    
    get <- function()
    {
        x
    }
    
    setminverse <- function( minverse )
    {
        m <<- minverse
    }
    
    getminverse <- function()
    {
        m
    }
    
    list( set = set, get = get, setminverse = setminverse, getminverse = getminverse )
}


## Write a short comment describing this function
# Checks the parent environment if the inverse of a matrix has been stored there as a variable.
# If so, returns that value, if not computes the inverse and then stores it in the parent evironment.
# Both are accomplished by using the functions from the list created by makeCacheMatrix() above.
cacheSolve <- function( x, ... )
{
    ## Return a matrix that is the inverse of 'x'
    m <- x$getminverse()
    if( !is.null( m ) )
    {
        message( "getting cached matrix" )
        return( m )
    }
    
    data <- x$get()
    m <- solve( data, ... )
    x$setminverse( m )
    m
}