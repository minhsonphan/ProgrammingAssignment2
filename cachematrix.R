## Description of makeCacheMatrix()
## This function returns a list of functions that allow to calculate the inversed matrix and retrieve its value
## Input: the matrix that should be inversed
## Ouput: a list of functions
##      setinversedmatrix()     : Calculates the inversed matrix and assigns it to the initial_matrix variable
##      getinitalmatrix()       : Retrieves the initial_matrix variable
##      getinversedmatrix()     : Retrieves the inversed_matrix variable

makeCacheMatrix <- function(x=matrix()) {
        ## initial_matrix contains the matrix that will be inversed
        if(!is.null(x)){
                initial_matrix <<- x        
        }else{
                initial_matrix <<- NULL
        }
        ## inversed_matrix contains the inversed matrix
        inversed_matrix <- NULL
        

        ## calculates the inversed marix and assign it to inversed_matrix
        setinversedmatrix <- function(){
                if(!is.null(initial_matrix)){
                        inversed_matrix<<-solve(initial_matrix)
                }else{
                        print("cannot calculate inverse matrix because initial matrix is NULL")
                }
        } 

        ## retrieves the value of initial_matrix
        getinitialmatrix <- function() initial_matrix

        ## retrieves the value of inversed_matrix
        getinversedmatrix <- function() inversed_matrix
        
        ## return a list of functions to handle the initial and inversed matrix
        list(setinversedmatrix = setinversedmatrix,
             getinitialmatrix = getinitialmatrix, getinversedmatrix = getinversedmatrix)
}


## Description of cacheSolve()
## This function checks whether the inversed matrix is cached.
## If the inversed matrix is cached, the function returns it
## If the inversed matrix is not cached, the function calculates the inversed matrix and returns it
## Input: the makeCacheMatrix function
## Ouput: the inversed matrix

cacheSolve <- function(x) {
        ## retrieve the inversed_matrix value from the cache
        inversed_matrix <- x$getinversedmatrix()
        
        ## checks whether the cached inversed_matrix value is NULL or not
        ## if not NULL then retrieves the inversed_matrix value in cache
        ## otherwise calculates the inversed matrix and sets the value in cache
        if(!is.null(inversed_matrix)) {
                message("getting cached inversed matrix")
                
                ## retrieve the initial_matrix value from the cache
                initial_matrix <- x$getinitialmatrix()
                
                ## checks whether the inversed matrix in cache matches the initial matrix
                ## in this case initial matrix * inversed matrix should be equal to the unity matrix
                if(all(inversed_matrix%*%initial_matrix==diag(nrow(inversed_matrix)))){
                        return(inversed_matrix)        
                }else{
                ## if the test ik ko then the inversed matrix in cache does not match the initial matrix
                ## the inversed matrix needs to be calculated
                        return(x$setinversedmatrix())       
                }
        }else{
                return(x$setinversedmatrix())
        }

}

## Description of TestCachedMatrix()
## This function tests whether the cached matrix is retrieved correctly by cacheSolve()
## in the case where the cached matrix is not NULL

TestCachedMatrix<-function(){
        ## initialize a matrix
        mymatrix<-matrix(1:4,nrow=2,ncol=2)
        ## sets the list of functions to handle inversed matrix
        mycacheMatrix<-makeCacheMatrix(mymatrix)
        ## test1 : check the value of the initial matrix in cache
        print(mycacheMatrix$getinitialmatrix())
        ## calculate the inversed matrix
        mycacheMatrix$setinversedmatrix()
        ## test2 : check the value of the inversed matrix in cache
        inversed_matrix<-mycacheMatrix$getinversedmatrix()
        print(inversed_matrix)
        ## test3 : check the whether the value of the inversed matrix in cache is correct
        inversed_matrix<-cacheSolve(mycacheMatrix)
        print(inversed_matrix)
        
        if(all(inversed_matrix%*%mymatrix==diag(nrow(mymatrix)))){
                print("succesful test")
        }else{
                print("unsuccesful test")
                print(inversed_matrix%*%mymatrix)
        }
        
}

## Description of TestNotCachedMatrix()
## This function tests whether the cached matrix is retrieved correctly by cacheSolve()
## in the case the cached inversed matrix is NULL

TestNotCachedMatrix<-function(){
        
        ## initialize a matrix
        mymatrix<-matrix(1:4,nrow=2,ncol=2)
        ## sets the list of functions to handle inversed matrix
        mycacheMatrix<-makeCacheMatrix(mymatrix)
        ## test1 : check the value of the initial matrix in cache
        print(mycacheMatrix$getinitialmatrix())
        ## test2 : check the value of the inversed matrix in cache. It should be NULL.
        inversed_matrix<-mycacheMatrix$getinversedmatrix()
        print(inversed_matrix)
        ## test3 : check the whether the value of the inversed matrix in cache is correct
        inversed_matrix<-cacheSolve(mycacheMatrix)
        print(inversed_matrix)
        if(all(inversed_matrix%*%mymatrix==diag(nrow(mymatrix)))){
                print("succesful test")
        }else{
                print("unsuccesful test")
        }
        
}
