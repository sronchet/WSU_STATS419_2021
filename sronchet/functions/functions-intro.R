# is it a whole number
is.wholenumber = function(x, tol = .Machine$double.eps^0.5)
{
  abs(x - round(x)) < tol; 
}
is.wholenumber(1)

library(FRACTION); 
library(plotrix); 
library(pracma); 

#handShake function
handShake = function(n = 1, plotMe = FALSE)
{
  if(n < 1)
  {
    stop("Error"); 
  }
  
  if(!is.wholenumber(n))
  {
    stop("n must be an integer"); 
  }
  
  h = n*(n-1)/2;
  
  if(plotMe)
  {
    plot(0,0, type = "n", main = paste0(n, "People", h, "handShakes"));
      draw.circle(0,0,1, border = "blue", lwd = 3); 
    saved = matrix(0, n ,2); 
    x = 0;
    y = 0;
    for (i in 1:n)
    {
      x = round(cos(2 * pi * i/n), digits = 4); 
      y = round(sin(2 * pi * i/n), digits = 4);
      points(x = x, y = y, cex = 3); 
      saved[i, 1] = x; 
      saved[i, 2] = y; 
    }
    for(source in 1:n)
    {
      for(target in 1:n)
      {
        segments(saved[source, 1], saved[source, 2], saved[target, 1], saved[target, 2])
      }
    }
    #can you draw a circle
    #can you loop through "n" points ... and connect "n-1" elements
    #for (i  in 1:n)... for (j in 1(n-1))
    #plot(n,h); 
  }
  
  h; 
}