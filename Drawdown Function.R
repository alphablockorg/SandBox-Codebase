
drawdown.analysis<-function(x,maxdrop11){
  
  drawdown.days=NULL
  
  x2=as.numeric(x[,2])
  max=x[1,2]
  i=1
  drawdown.start=NULL
  drawdown.end=NULL
  #maxdrop11=5
  
  j=i
  
  while(i<length(x2)){#Thus loop gives us when drawdowns begin and end.
    
    while((((x2[i+1]/max)-1)*100) >= (-maxdrop11) && i<length(x2) ){
      if(x2[i+1]>max) max=x2[i+1]
      i=i+1
      
    }
    
    
    if(i!=length(x2)) drawdown.start=c(drawdown.start,which.max(x2[j:i])+j-1)
    drawdown.start
    if(i!=length(x2)) {
      while(x2[i+1]< x2[drawdown.start[length(drawdown.start)]] && i<length(x2) ) i=i+1
      drawdown.end=c(drawdown.end,i+1)
      drawdown.end
      max=x2[i+1]
      j=i=i+1
      
    }
  }
  
  
  if(!is.null(drawdown.start)){
    
    n=max(length(drawdown.end),length(drawdown.start))
    drawdown=rep(0,length(x2))
    maxdrop1=NULL
    for(i in seq(1,n,1)){
      if(length(drawdown.end)==length(drawdown.start)){
        j=seq(drawdown.start[i],(drawdown.end[i]-1),1)
        drawdown[j]=((x2[j]/x2[drawdown.start[i]])-1)*100
        maxdrop1=c(maxdrop1,min(((x2[j]/x2[drawdown.start[i]])-1)*100))
      }
      
      if((length(drawdown.end)!=length(drawdown.start))&& i!=n){
        j=seq(drawdown.start[i],(drawdown.end[i]-1),1)
        drawdown[j]=((x2[j]/x2[drawdown.start[i]])-1)*100
        maxdrop1=c(maxdrop1,min(((x2[j]/x2[drawdown.start[i]])-1)*100))
        
      }
      
      if((length(drawdown.end)!=length(drawdown.start))&& i==n){
        j=seq(drawdown.start[i],length(x2),1)
        drawdown[j]=((x2[j]/x2[drawdown.start[i]])-1)*100
        maxdrop1=c(maxdrop1,min(((x2[j]/x2[drawdown.start[i]])-1)*100))
        
      }
      
      
    }
  }
  if (is.null(drawdown.start)){n=1} else {
    mat=data.frame(matrix(NA,nrow=n,ncol=4))
    mat[,1]=x[drawdown.start,1]
    mat[,2]=x[(drawdown.end-1),1]
    mat[,1]=as.Date(as.character(mat[,1]),"%Y-%m-%d")
    mat[,2]=as.Date(as.character(mat[,2]),"%Y-%m-%d")
    mat[,3]=mat[,2]-mat[,1]
    mat[,4]=maxdrop1
  }
  if(!is.null(drawdown.start)) 
  {return(list(mat,drawdown,drawdown.start,drawdown.end))} else { return(list(NULL,NULL,NULL,NULL)) }
  
}
