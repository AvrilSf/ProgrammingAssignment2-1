# Both of the functions below are strongly based in the example provided in the
# assignment page, as I am really new in programming, I am trying to learn by
# applying all the examples. I'm sure that as I keep learning, I will be able to 
# be creative in the ways of writing code. If this is not what is expected, 
# please give suggestions to improve. Please help me keep learning.

# The first function, makeCacheMatrix creates a square "matrix".
# The output is a list containing a function to set and get the value of the
# square matrix and also, set and get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        mtr <- NULL
        set <- function(y) {
                x <<- y
                mtr <<- NULL
        }
        get <- function() x
        set_inve <- function(solve) mtr <<- solve
        get_inve <- function() mtr
        list(set = set, get = get,
             set_inve = set_inve,
             get_inve = get_inve)
}


# This cacheSolve function gives the inverse of the square matrix created in the
# previous function. It firstly verifies if the inverse is already calculated.
# If it is prior to this function, it gets the inverse from the cache and avoids 
# the calculation. Else, it finds the inverse of the matrix and sets the value
# in the cache through the set_inve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mtr <- x[get_inve()]
        if(!is.null(mtr)) {
                message("getting cached data")
                return(mtr)
        }
        data2 <- x$get()
        mtr <- solve(data2, 1)
        x$set_inve(mtr)
        mtr
        
}
