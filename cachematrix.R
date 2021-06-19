#LEGEND
#some_matrix - the input matrix to be processed
#inv_matrix - the inverted matrix
#set_inv - the holding variable when the matrix inversion is calculated
#get_inv - the holding variable for calling the inverted matrix


# First function does 2 things: 
#1) it creates a list of functions to be called and used later
#2) it creates a function that holds the input matrix, to compare against
#   the next time we run the full code
makeCacheMatrix <- function(some_matrix = matrix()){    
        
        inv_matrix <- NULL                              
        set <- function(y){
                some_matrix <<- y
                inv_matrix <<- NULL
        }
        
        get <- function() some_matrix 
        set_inv <- function(solve) inv_matrix <<- solve 
        get_inv <- function() inv_matrix
        list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


#This function:
#1) calculates the inverted matrix
#2) compares the matrix it's about to invert with the initial matrix pulled
#   into the code earlier.
#   This is the crux of the procedure - to see if the initial matrix is the same
#   as the one about to be calculated. This is the decision that saves all the 
#   time!
#   If it's a new matrix: performs and reports the calculation
#   If it's dentical matrix: pulls the already calculated inversion from a cache
cacheSolve <- function(some_matrix, ...){
        inv_matrix <- some_matrix$get_inv()
        if(!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        }
        message("making calculations")
        data <- some_matrix$get()
        inv_matrix <- solve(data, ...)
        some_matrix$set_inv(inv_matrix)
        inv_matrix
}
#These lines are my test code to toggle between 2 different matrices

#some_matrix <- matrix(1:4, 2, 2)
some_matrix <- matrix(c(1,0,0,0,1,0,0,0,2),ncol=3,nrow=3)
z <- makeCacheMatrix(some_matrix)
cacheSolve(z)