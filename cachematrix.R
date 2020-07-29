# Both of the functions below are strongly based in the example provided in the
# assignment page.

# The first function, makeCacheMatrix creates a square "matrix".
# The output is a list containing a function to set and get the value of the
# square matrix and also, set and get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        mtr <- NULL               ## Initialize mtr as NULL; will hold value of
                                  ## matrix inverse.
        set <- function(y) {      ## Define the set function to assign new.
                x <<- y           ## Value of matrix in parent environment.
                mtr <<- NULL      ## If there is a new matrix,reset mtr to NULL.
        }
        
        get <- function() x       ## Define the get function - returns value of
                                  ## the matrix argument.        
        set_inve <- function(solve) mtr <<- solve    ## Assigns value of mtr in
                                  ## parent environment.
        get_inve <- function() mtr ## Gets the value of mtr where called.
        list(set = set, get = get,
             set_inve = set_inve,
             get_inve = get_inve)  ## You need this in order to refer to the 
                                   ## functions with the $ operator.
}


# This cacheSolve function gives the inverse of the square matrix created in the
# previous function. It firstly verifies if the inverse is already calculated.
# If it is prior to this function, it gets the inverse from the cache and avoids 
# the calculation. Else, it finds the inverse of the matrix and sets the value
# in the cache through the set_inve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        mtr <- x$get_inve
        if (!is.null(mtr)) {                     # In inverse matrix is not NULL
                message("getting cached data")  # type message:"getting cached..
                return(mtr)                     # return the invertible matrix
        }                       # if value of invertible matrix is null then
        
        data2 <- x$get()        # get the original matrix data 
        mtr <- solve(data2, 1)  # use solve function to inverse matrix
        x$set_inve(mtr)         # set the invertible matrix
        mtr                     # return the invertible matrix
        
}
