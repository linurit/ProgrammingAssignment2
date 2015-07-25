# Defines functions to manipulate calculation, caching and retreival 
# of a matrix inversion and makes them available to other functions

makeCacheMatrix <- function(x = matrix()) {

     # solved initiated to NULL 
     solved <- NULL

     # take input to set and assign to y, then assign this to x 
     # assign NULL to solved
     set <- function(y) {
          x <<- y
          solved <<- NULL
     }

     # function which defines getting the passed-in matrix
     get <- function() x
     
     # function which defines solving the matrix inverse and assigning to 
     # solved     
     setsolved <- function(solve) solved <<- solve

     # function which defines retrieving the matrix inverse from cache
     getsolved <- function() solved
     
     # make functions available by name
     list(set = set, get = get, 
          setsolved = setsolved,
          getsolved = getsolved)
     
}


# Takes an input of a matrix, checks for a cached result, or calculates
# the result if needed.

cacheSolve <- function(x, ...) {

     # solved is the cache vector, set to NULL by default     
     solved <- x$getsolved()

     # if solved is not null, return the value of solved from cache
     if(!is.null(solved)) {    
          message("getting matrix inverse")
          return(solved)
     }

     # execute the get() function on x and put it in matrix
     matrix <- x$get()
 
     # solve the passed matrix to the solve function
     solved <- solve(matrix)

     # return the solved matrix inversion     
     solved
}
