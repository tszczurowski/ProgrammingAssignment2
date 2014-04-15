# Before we invert matrix we have to use function makeCacheMatrix() on our matrix.
# Function makeCacheMatrix() assigns to dx the list of functions on matrix which we want to invert in list dx.
# dx is assigned globally and than it is later used by function cacheSolve(), which exactly inverts our matrix.

# Function makeCacheMatrix() 
# It assigns globally to variable df the list of functions on a matrix to invert.
# df consist of three functions: get() return the matrix to invert, getsolve() stores in "m" the inverted matrix.
# "m" is stored in makecacheMatrix() function,
#setsolve() is a function which calculates the inverted matrix and stores it in "m"

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        dx <<- list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


#Function CacheSolve()
#This function firstly checks if matrix was inverted (!is.null(m)).
# Then returns the "m", where inverted matrix is stored or it calculates it.
# If CacheSolve() computes inverted matrix, firstly, it gets matrix with get(), then it inverts it,
#and finally it stores it in m variable (remember, that m is stord in makeCacheMatrix()).


cacheSolve <- function(x, ...) {
        m <- dx$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- dx$get()
        m <- solve(data, ...)
        dx$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
