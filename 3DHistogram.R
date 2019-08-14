Histogram3D<- function(x,y, breaks){
    
    # x and y have the same length
    
    len<- length(x) # = length(y)
    
    rangeX <- max(x) - min(x)
    
    rangeY <- max(y) - min(y)
    
    intervalx <- rangeX/breaks
    
    intervaly <- rangeY/breaks
    
    midsxx<- matrix(ncol = breaks, nrow = breaks)
    
    midsyy<- matrix(ncol = breaks, nrow = breaks)
    
    
    breaksx<- seq( min(x) ,  max(x) , intervalx ) # length BREAKS +1
  
    breaksy<- seq( min(y) ,  max(y) , intervaly ) # length BREAKS +1
    
   
    weights<- matrix(ncol = breaks, nrow = breaks)
    weights <- is.na(weights) - 0L
    weights <- weights - 1

    
    for( i in 1: len)
    {

        
        for(j in 1:(breaks))
        {
            
            if(breaksx[j] <= x[i] &&  x[i] < breaksx[j+1] )
            {
                indicex<- j

                break
            }
            if(x[i]==max(x))
            {
                indicex<- breaks

                break
            }
        }
        
        for(j in 1:(breaks))
        {
            if(breaksy[j] <= y[i] &&  y[i]  < breaksy[j+1] )
            {
                indicey<- j

                break
            }
            if(y[i]==max(y))
            {
                indicey<- breaks

                break
            }
            
        }
        
        weights[indicex,indicey] <- weights[indicex,indicey] + 1
        
    }
    
    weights <- weights /len
    
    weights <- weights / (( breaksx[2] - breaksx[1] ) * ( breaksy[2] - breaksy[1] ))
        
    final<-list("density"= weights, "breaksx"=breaksx, "breaksy"=breaksy )
    
    return(final)
    
}