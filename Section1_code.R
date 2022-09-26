library("rstudioapi")     
setwd(dirname(getActiveDocumentContext()$path))
getwd()

createMatrix<-function(symbol,label,number){
  setwd("./Images")
  #symbol==0
  symbol[symbol==0]<- 1
  symbol==255
  symbol[symbol==255]<-0

  symbol[symbol==255]<-0

  
  symbolMat<-matrix(symbol[3:326,1],nrow = 18,ncol = 18,byrow = TRUE)
  str1<-label
  str2<-number
  student_No<-"40293751_"
  fileName<-paste(student_No,label,"_",number,".csv",sep = "")
  write.table(symbolMat,file=fileName,quote = FALSE,row.names = FALSE, col.names = FALSE,sep=",")
  }
createImage<-function(name,symbol,maxNo){
for (i in 1:(maxNo))
{
  setwd(dirname(getActiveDocumentContext()$path))
  if (i<=9){
    str1<-paste("./Images/",name,"0",i,".PGM",sep = "")
  #needs relative path
    h<-read.delim(str1,header=TRUE,quote = " ")
  createMatrix(h,symbol,paste("0",i,sep=""))
  }
  else
  {
    str1<-paste("./Images/",name,i,".PGM",sep = "")
   #needs relative path
    h<-read.delim(str1,header=TRUE,quote = " ")
    createMatrix(h,symbol,paste(i,sep=""))
  }

}
  print("All csv files created")
}
createImage("a","a",8)
createImage("b","b",8)
createImage("c","c",8)
createImage("d","d",8)
createImage("e","e",8)
createImage("f","f",8)
createImage("f","g",8)
createImage("h","h",8)
createImage("i","i",8)
createImage("j","j",8)
createImage("smileyFace","smiley",20)
createImage("sadFace","sad",20)
createImage("xclaim","xclaim",20)



