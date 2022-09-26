
library(raster)
search()
library("rstudioapi")     
setwd(dirname(getActiveDocumentContext()$path))
getwd()
createFeautres <-function(label,number,features){
  for (i in 1:(number)){
   # print(i)
    feat<-c(label)
    if (i<=9){
    path<-paste('./Images/40293751_',label,'_0',i,'.csv',sep = "")
   #needs relative path
   # print(path)
     csvF<-read.csv(file =path ,header = FALSE)
    csvF
    num<-paste(0,i,sep = "")
    matTemp<-as.matrix(csvF)
 feat<-append(feat,num)
    }else
    {
      path<-paste('./Images/40293751_',label,'_',i,'.csv',sep = "")
    #Needs relative path
          #print(path)
      csvF<-read.csv(file = path,header = FALSE)
      csvF
      matTemp<-as.matrix(csvF)
      feat<-append(feat,i)
    }
    #Number of black pixels
  nrBlackPixels<-0
  for (i in 1:nrow(matTemp)) {
    for ( j in 1:ncol(matTemp)){
      if (matTemp[i,j]==1)
      {
        nrBlackPixels<-nrBlackPixels+1
      }
    }
  }
 # print(nrBlackPixels)
  feat<-append(feat,nrBlackPixels)
  #Rows with 1 black Pixel
  pixelCount<-0
  rowWith1BlcPx<-0
  for (i in 1:nrow(matTemp)) {
    
    
    for (j in 1:ncol(matTemp))
    {
      if (matTemp[i,j]==1)
      {
        pixelCount<-pixelCount+1   
      }
    }
    #print(pixelCount)
    if (pixelCount==1){
      rowWith1BlcPx<-rowWith1BlcPx+1
    }
    pixelCount<-0
  }
  
 # print(rowWith1BlcPx)
  feat<-append(feat,rowWith1BlcPx)
  
  #col with 1 black pixel
  pixelCount<-0
  colWith1BlcPx<-0
  for (i in 1:ncol(matTemp)) {
    
    
    for (j in 1:nrow(matTemp))
    {
      if (matTemp[j,i]==1)
      {
        pixelCount<-pixelCount+1   
      }
      
    }
    if (pixelCount==1){
      colWith1BlcPx<-colWith1BlcPx+1
    }
    #print(pixelCount)
    pixelCount<-0
  }
  #print(colWith1BlcPx)
  feat<-append(feat,colWith1BlcPx)
  #row with 3 or more black pixels
  pixelCount<-0
  rowWith3BlcPx<-0
  for (i in 1:nrow(matTemp)) {
    
    
    for (j in 1:ncol(matTemp))
    {
      if (matTemp[i,j]==1)
      {
        pixelCount<-pixelCount+1   
      }
    }
    #print(pixelCount)
    if (pixelCount>=3){
      rowWith3BlcPx<-rowWith3BlcPx+1
    }
    pixelCount<-0
  }
  
 # print(rowWith3BlcPx)
  feat<-append(feat,rowWith3BlcPx)
  #Col with 3 or more black pixels
  pixelCount<-0
  colWith3BlcPx<-0
  for (i in 1:ncol(matTemp)) {
    
    
    for (j in 1:nrow(matTemp))
    {
      if (matTemp[j,i]==1)
      {
        pixelCount<-pixelCount+1   
      }
    }
   # print(pixelCount)
    if (pixelCount>=3){
      colWith3BlcPx<-colWith3BlcPx+1
    }
    pixelCount<-0
  }
  #print(colWith3BlcPx)
  feat<-append(feat,colWith3BlcPx)
  
  #aspect ratio
  topmost<-0
  bottommost<-0
  leftmost<-0
 rightmost<-0
  for (i in 1:nrow(matTemp)) {
    for ( j in 1:ncol(matTemp)){
      if (matTemp[i,j]==1)
      {
        if (i>rightmost){
          rightmost<-i
        }
        if (j>bottommost){
          bottommost<-j
        }
        if (topmost==0){
       topmost<-j
        }
        if (leftmost==0){
          leftmost<-i
        }
        if (topmost!=0 && topmost> j)
        {
          topmost<-j
        }
        if (leftmost !=0 && leftmost>i ){
          leftmost<-i
        }
      }
    }
  }
 #print(bottommost)
 #print(topmost)
 #print(leftmost)
# print(rightmost)
 #print(topmost)
 #print(bottommost)
  height<-bottommost-topmost
  width<-rightmost-leftmost
  if (height==0)
  {
    height=1
  }
  if (width==0)
  {
    width=1
  }
  
  aspectRatio<- height / width
# print(height)
# print(width)
#   print(as.double(aspectRatio))
  feat<-append(feat,aspectRatio)
  #neighbour with 1 pixel
  nrBlackPixels<-0
  nghPixelswth1<-0
  for (i in 1:nrow(matTemp)) {
    for ( j in 1:ncol(matTemp)){
      if (matTemp[i,j]==1){
      if(i-1>=1 & j-1>=1){
      if (matTemp[i-1,j-1]==1)
      {
       
        nrBlackPixels<-nrBlackPixels+1
      }
      }
      if (i-1>=1){
        if (matTemp[i-1,j]==1){
  nrBlackPixels<-nrBlackPixels+1
        }
        
      }
      if (i-1>=1 && j+1 <=18){
        if (matTemp[i-1,j+1]==1){
          nrBlackPixels<-nrBlackPixels+1
        }
      }
      if (j-1 >=1){
        if (matTemp[i,j-1]==1){
          nrBlackPixels<-nrBlackPixels+1
        }
      }
      if (j+1 <=18){
        if (matTemp[i,j+1]==1){
          nrBlackPixels<-nrBlackPixels+1
        }
      
      }
      if (i+1<=18 && j-1 >=1){
        if (matTemp[i+1,j-1]==1){
          nrBlackPixels<-nrBlackPixels+1
        }
        
      }
      if (i+1 <=18){
        if (matTemp[i+1,j]==1){
          nrBlackPixels<-nrBlackPixels+1
        }
        
      }
      if (i+1 <=18 && j+1<=18){
        if (matTemp[i+1,j+1]==1){
          nrBlackPixels<-nrBlackPixels+1
        }
        
      }
     
      if (nrBlackPixels==1){
        nghPixelswth1<-nghPixelswth1+1
      }
      nrBlackPixels=0
    }
    }
  }
 # print("jsjsjs")
 # print(nghPixelswth1)
  feat<-append(feat,nghPixelswth1)
  #No black pixels above
  whitePixel<-0
  noBlackPixsAbove<-0
  for (i in 1:nrow(matTemp)) {
    for ( j in 1:ncol(matTemp)){
      if(matTemp[i,j]==1)
      {
      if(i-1>=1 & j-1>=1){
        if (matTemp[i-1,j-1]==0)
        {
          
          whitePixel<-whitePixel+1
        }
      }
      if (i-1>=1){
        if (matTemp[i-1,j]==0){
          whitePixel<-whitePixel+1
        }
        
      }
      if ( i-1>=1 && j+1 <=18){
        if (matTemp[i-1,j+1]==0){
          whitePixel<-whitePixel+1
        }
      }
     # print(whitePixel)
      if (whitePixel==3)
      {
        noBlackPixsAbove<-noBlackPixsAbove+1
      }
      whitePixel<-0
      }
    }
  }
  #print(noBlackPixsAbove)
  feat<-append(feat,noBlackPixsAbove)
  #no black pixels below
  whitePixel<-0
  noBlackPixsBelow<-0
  for (i in 1:nrow(matTemp)) {
    for ( j in 1:ncol(matTemp)){
      if(matTemp[i,j]==1)
      {
        if(i+1<=18 & j-1>=1){
          if (matTemp[i+1,j-1]==0)
          {
            
            whitePixel<-whitePixel+1
          }
        }
        if (i+1<=18){
          if (matTemp[i+1,j]==0){
            whitePixel<-whitePixel+1
          }
          
        }
        if ( i+1<=18 && j+1 <=18){
          if (matTemp[i+1,j+1]==0){
            whitePixel<-whitePixel+1
          }
        }
        #print(whitePixel)
        if (whitePixel==3)
        {
          noBlackPixsBelow<-noBlackPixsBelow+1
        }
        whitePixel<-0
      }
    }
  }
 # print(noBlackPixsBelow)
  feat<-append(feat,noBlackPixsBelow)
  #No black pixels to left
  whitePixel<-0
  noBlackPixsLeft<-0
  for (i in 1:nrow(matTemp)) {
    for ( j in 1:ncol(matTemp)){
      if(matTemp[i,j]==1)
      {
        if(i-1>=1 & j-1>=1){
          if (matTemp[i-1,j-1]==0)
          {
            
            whitePixel<-whitePixel+1
          }
        }
        if (j-1>=1){
          if (matTemp[i,j-1]==0){
            whitePixel<-whitePixel+1
          }
          
        }
        if ( i+1<=18 && j-1>=1){
          if (matTemp[i+1,j-1]==0){
            whitePixel<-whitePixel+1
          }
        }
        #print(whitePixel)
        if (whitePixel==3)
        {
          noBlackPixsLeft<-noBlackPixsLeft+1
        }
        whitePixel<-0
      }
    }
  }
  #print(noBlackPixsLeft)
  feat<-append(feat,noBlackPixsLeft)
  #No black pixels to the right
  whitePixel<-0
  noBlackPixsRight<-0
  for (i in 1:nrow(matTemp)) {
    for ( j in 1:ncol(matTemp)){
      if(matTemp[i,j]==1)
      {
        if(i-1>=1 & j+1<=18){
          if (matTemp[i-1,j+1]==0)
          {
            
            whitePixel<-whitePixel+1
          }
        }
        if (j+1<=18){
          if (matTemp[i,j+1]==0){
            whitePixel<-whitePixel+1
          }
          
        }
        if ( i+1<=18 && j+1<=18){
          if (matTemp[i+1,j+1]==0){
            whitePixel<-whitePixel+1
          }
        }
      #  print(whitePixel)
        if (whitePixel==3)
        {
          noBlackPixsRight<-noBlackPixsRight+1
        }
        whitePixel<-0
      }
    }
  }
  #print(noBlackPixsRight)
  feat<-append(feat,noBlackPixsRight)
  #no black pixels neighbouring horiz
  whitePixel<-0
  noBlackPixsHoriz<-0
  for (i in 1:nrow(matTemp)) {
    for ( j in 1:ncol(matTemp)){
      if(matTemp[i,j]==1)
      {
        if (j+1<=18){
          if (matTemp[i,j+1]==0){
            whitePixel<-whitePixel+1
          }
          
        }
        if (j-1>=1){
          if (matTemp[i,j-1]==0){
            whitePixel<-whitePixel+1
          }
        }
        #print(whitePixel)
        if (whitePixel==2)
        {
          noBlackPixsHoriz<-noBlackPixsHoriz+1
        }
        whitePixel<-0
      }
    }
  }
  #print(noBlackPixsHoriz)
  feat<-append(feat,noBlackPixsHoriz)
  #No black pixels vertical
  whitePixel<-0
  noBlackPixsVert<-0
  for (i in 1:nrow(matTemp)) {
    for ( j in 1:ncol(matTemp)){
      if(matTemp[i,j]==1)
      {
        if (i+1<=18){
          if (matTemp[i+1,j]==0){
            whitePixel<-whitePixel+1
          }
          
        }
        if (i-1>=1){
          if (matTemp[i-1,j]==0){
            whitePixel<-whitePixel+1
          }
        }
        #print(whitePixel)
        if (whitePixel==2)
        {
          noBlackPixsVert<-noBlackPixsVert+1
        }
        whitePixel<-0
      }
    }
  }
 # print(noBlackPixsVert)
  feat<-append(feat,noBlackPixsVert)
  #find connected areas
ras<-raster(matTemp)
#print(ras)
rc<-clump(ras,directions=8,gaps=FALSE)


clumpTab<-(freq(rc))
#print(nrow(clumpTab)-1)
connectedAreas<-nrow(clumpTab)-1
feat<-append(feat,connectedAreas)
#find eyes
#ras<-raster(matTemp)
#print(ras)
nrBlackPixels<-0
for (i in 1:nrow(matTemp)) {
  for ( j in 1:ncol(matTemp)){
    if (matTemp[i,j]==1)
    {
      matTemp[i,j]=0
    }
    else {
      matTemp[i,j]=1
    }
  }
}
ras<-raster(matTemp)
rc<-(clump(ras,directions=4,gaps=FALSE,values=0))
#print(length(rc))
clumpTab<-(freq(rc))

#print(freq(rc))
#print(nrow(clumpTab)-2)
eyes<-nrow(clumpTab)-2
feat<-append(feat,eyes)

#customFeature number of pixels in widest row-to tell if it is a wide symbol such as a smiley and sad face??
nrBlackPixels<-0
for (i in 1:nrow(matTemp)) {
  for ( j in 1:ncol(matTemp)){
    if (matTemp[i,j]==1)
    {
      matTemp[i,j]=0
    }
    else {
      matTemp[i,j]=1
    }
  }
}
pixelCount<-0
mostBlackPixels<-0
for (i in 1:nrow(matTemp)) {
  
  
  for (j in 1:ncol(matTemp))
  {
   # print(matTemp[i,j])
    if (matTemp[i,j]==1)
    {
      pixelCount<-pixelCount+1   
    }
  }
 
  #print(pixelCount)
  if (pixelCount>mostBlackPixels){
   mostBlackPixels<-pixelCount
  }
  pixelCount<-0
  
}
#print(mostBlackPixels)

feat<-append(feat,mostBlackPixels)
#print(feat)
features<-rbind(features,feat)
#print(values(freq(rc)))

  }
  #print(features)
  return(features)
}
#features<-matrix(ncol = 18)
#features
#smiley01<-read.csv(file = 'C:/Users/eunan/Documents/CSC2062 Assignment 2/Images/40293751_smiley_09.csv',header = FALSE)
#smiley01
#smilMat<-as.matrix(smiley01)
features<-matrix(,ncol=18)
features<-createFeautres("a",8,features)
features<-createFeautres("b",8,features)
features<-createFeautres("c",8,features)
features<-createFeautres("d",8,features)
features<-createFeautres("e",8,features)
features<-createFeautres("f",8,features)
features<-createFeautres("g",8,features)
features<-createFeautres("h",8,features)
features<-createFeautres("i",8,features)
features<-createFeautres("j",8,features)
features<-createFeautres("sad",20,features)
features<-createFeautres("smiley",20,features)
features<-createFeautres("xclaim",20,features)
colnames(features)<-c('label','index','nr_pix','rows_with_1','cols_with_1','rows_with_3p','cols_with_3p','aspect_ratio','neigh_1 ','no_neigh_above','no_neigh_below','no_neigh_left','no_neigh_right','no_neigh_horiz','no_neigh_vert','connected_areas','eyes','custom')
write.table(na.omit(features),file="40293751_features.csv",row.names = F,quote = F,sep=",")
csvF<-read.csv(file ="40293751_features.csv" ,header = TRUE)

