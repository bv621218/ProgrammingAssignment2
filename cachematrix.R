# This script contains two functions.
#
#
# "makeCacheMatrix" is a function that creates a object that contains a matrix and can store the results of its inverse.
#
# "cacheSolve" is a function that will look for and print the cached inverse matrix or calculate the inverse if not cached.
#
#
### makeCacheMatrix

makeCacheMatrix<- function(mat = matrix()){
  inv<- NULL # clear cache
  setMat<- function(y){
    mat<<- y # set matrix
    inv<<- NULL # ensure that inverse is cleared.
  }
  #function to retreive the matrix
  getMat<- function(){mat}
  #function to set the inverse of the matric is cache is cleared
  setInv<- function(inverse){ inv <<- inverse}
  #function to retrieve the cached inverse matrix
  getInv<- function(){inv}
  #set returned object
  list(setMat = setMat, getMat = getMat,
       setInv = setInv, getInv = getInv)
}


### cacheSolve function
cacheSolve<- function(mat){
  inv<- mat$getInv()
  # if there is no cached inverse matrix, this will solve for the matrix and cache the result.
  if(is.null(inv)){
    message("calculating inverse matrix")
    inv<- solve(mat$getMat())
    mat$setInv(inv)
    inv
  }
  else{
    message("Cached inverse matrix")
    return(inv)
  }
}

#Code example
# mat<- makeCacheMatrix(matrix(rnorm(25), nrow = 5))
# cacheSolve(mat)
