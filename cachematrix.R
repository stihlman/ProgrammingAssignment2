##-----------------------------------------------------------------
##  F u n c t i o n    O v e r v i e w
##    makeCacheMatrixlist
##       is a 2 distinct pairs of get/set functions, 1 get/set pair 
##       to operate on a matrix and another get/set pair to operate
##       on its inverse.  Both set functions post their contents
##       in another (non-local) environment
##             
##    cacheSolve
##       is a function that uses the output matrix from 
##       makeCacheMatrixlist to either (a) get the previously-cached
##       inverse from a containing environment or (b) calculate its
##       inverse and cache it 
##-----------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
        ##-----------------------------------------------------------------
        ## Function:   makeCacheMatrix
        ## Desc:       a list of functions to set/get a square (invertible) 
        ##             matrix and set/get the inverse of the matrix 
        ## Arguments:  x is a matrix to be inverted (thus it must be square)
        ## Returns:    list with these functions 
        ##             set (set the matrix)
        ##             get (get the matrix)
        ##             setinv (set the inverse of the matrix)
        ##             getinv (get the inverse of the matrix)
        ##-----------------------------------------------------------------

        inv = NULL
        
        set = function(y) {
                x <<- y  ## super assignment operator to place 
                         ## in a containing environment (not local)
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ##-----------------------------------------------------------------
        ## Function:   cacheSolve
        ## Desc:       derives the inverse of the matrix provided
        ## Arguments:  x is output (matrix) from makeCacheMatrix() function
        ## Returns:    inverse of matrix
        ##-----------------------------------------------------------------
        
        inv = x$getinv()
        
        # check to see if the inverse has already been derived
        if (!is.null(inv)){
                # get & return the cached value since it was previously derived 
                message("getting cached data")
                return(inv)
        }
        
        # calculate the inverse of the matrix 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # use setinv function of the makeCacheMatrix to set the inverse
        x$setinv(inv)
        
        return(inv)
}

############################################
##   T E S T   H A R N E S S
##      set.seed(314)
##      r = rnorm(100)
##      mtrx_1 = matrix(r, nrow=10, ncol=10)
##      mtrx_1a = makeCacheMatrix(mtrx_1)
##      cacheSolve(mtrx_1a)
############################################
