#Pairwise Sequence alignment function called "my2seq_alignment_viz"
#This code creates a function that aligns two sequences and outputs a visual representation.
#It uses the needleman algorithm however the penalizations are different 
# Match +1, Mismatch -1, Gap -1 
#This was meant as an exercise, however I intend to change the penalizations to the original +5, -4, -1
#Too much talking 

my2seq_alignment_viz  <- function(sequence_2, sequence_1){
  our_algo <- matrix(nrow = length(sequence_2)+1, ncol = length(sequence_1)+1)

  #assigning objects to represent rownames and columns (sequences to be aligned)
  c <- colnames(our_algo) <-  c("ini", sequence_1)
  r <- rownames(our_algo) <-   c("ini", sequence_2)

  #filling the ini row and column
  our_algo[,1] <- c(0:-length(sequence_2))
  our_algo[1,] <- c(0:-length(sequence_1)) 
  
  for (j in 2:ncol(our_algo)) {
    for (i in 2:nrow(our_algo)) {
      if(c[j]==r[i]){ #Match
        d <- our_algo[i-1,j-1]+1 #diagonal vector 
        
      }
      
      else{ #Mismatch
        d <- our_algo[i-1,j-1]-1 #diagonal vector (next row, next column movement)
      }
      s <- our_algo[i-1,j]-1   #sideways vector(x axis movement)
      t <- our_algo[i,j-1]-1   #verticle vector(y axis movement)
      
      our_algo[i,j] <- max(d,s,t)
      
    }
   
  }
  
  ##Visualization function (requires ggplot2 package)
  library(ggplot2) #loading ggplot2
  
  max_row_mtrx <- t(apply(our_algo[2:nrow(our_algo),2:ncol(our_algo)], 1, function(row) {
    row[row != max(row)] <- 0 #replace with zero if not max
    return(row)
  }))  ##t() returns the matrix with the same dimensions.
  
  max_row_mtrx <- heatmap(max_row_mtrx, Rowv = NA, Colv = NA)  #clustering or reodering rows and columns inhibited.
  
  return(max_row_mtrx)
}

#TESTING 1,2

#Example sequences for testing
sequence_1 <- c("A","T","G","G","C","G","T")
sequence_2 <- c("A","T","G","A","G","T")

my2seq_alignment_viz(sequence_2, sequence_1) #success
