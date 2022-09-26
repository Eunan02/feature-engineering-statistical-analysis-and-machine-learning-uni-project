## @knitr Pr4.1
library(knitr)
library("rstudioapi")
library(MASS)
library(ggplot2)
search()
setwd(dirname(getActiveDocumentContext()$path))
getwd()
set.seed(42)
predictAsceptRatio<-function()
{
  multipe.regression<-lm(aspect_ratio~nr_pix + rows_with_1+cols_with_1+rows_with_3p+cols_with_3p+neigh_1+no_neigh_above+no_neigh_below+no_neigh_left+no_neigh_right+no_neigh_horiz+no_neigh_vert+connected_areas+eyes+custom,data=csvF)
print(summary(multipe.regression))

sa<-stepAIC(multipe.regression,direction="backward")
multRegModel<-multipe.regression<-lm(aspect_ratio~nr_pix + rows_with_1+cols_with_1+cols_with_3p+no_neigh_above+no_neigh_right+no_neigh_horiz+connected_areas+custom,data=csvF)
print(summary(multRegModel))
}
logisticRegression<-function()
{
  #Add dummy value 1 for letters, so it discriminates between letters and non letters
  csvF$dummy.letters<-0
  print(csvF$dummy.letters)
  csvF[1:80,19]<-1

  #Shuffle the rows
  features_shuffled<-csvF[sample(nrow(csvF)),]
  
  #first 80% will be used as training data
  trainingdata=features_shuffled[1:112,]
  testData=features_shuffled[113:140,]
  
  plt <- ggplot(trainingdata, aes(x=no_neigh_above, fill=as.factor(dummy.letters))) +
    geom_histogram(binwidth=.2, alpha=.5, position='identity')
  plot(plt)

  glmFit<-glm(dummy.letters ~ no_neigh_above, 
              data = trainingdata, 
              family = 'binomial') 
 
  print(summary(glmFit))
  
  newData = as.data.frame(c(5,10,20)) 
  colnames(newData) = 'no_neigh_above'
  
  head(newData)
  predicted = predict(glmFit, newData, type="response")
  print(predicted)
  x.range = range(trainingdata[["no_neigh_above"]])
 x.range
  x.values = seq(x.range[1],x.range[2],length.out=1000)
  x.values
  
 fitted.curve <- data.frame(no_neigh_above = x.values)
   summary(fitted.curve)
   fitted.curve[["dummy.letters"]] = predict(glmFit, fitted.curve, type="response")
   
  # Plot the training data and the fitted curve:
  plt <-ggplot(trainingdata, aes(x=no_neigh_above, y=dummy.letters)) + 
     geom_point(aes(colour = factor(dummy.letters)), 
                show.legend = T)+
     geom_line(data=fitted.curve, colour="orange", size=1)
  
  plot(plt)
  
  trainingdata[["predicted_val"]] = predict(glmFit, trainingdata, type="response")
  trainingdata[["predicted_class"]] = 0
  trainingdata[["predicted_class"]][trainingdata[["predicted_val"]] > 0.5] = 1
  
  
  
  correct_items = trainingdata[["predicted_class"]] == trainingdata[["dummy.letters"]] 
  correct_items
  
  # proportion correct:
  print("Prop correct for training data")
  print(nrow(trainingdata[correct_items,])/nrow(trainingdata))
  
  # proportion incorrect:
  print("Prop incorrect for training data")
  print(nrow(trainingdata[!correct_items,])/nrow(trainingdata))
  
  
  

  
  testData[["predicted_val"]] = predict(glmFit, testData, type="response")
  testData[["predicted_class"]] = 0
  testData[["predicted_class"]][testData[["predicted_val"]] > 0.5] = 1
  
  correct_items = testData[["predicted_class"]] == testData[["dummy.letters"]] 
  
  # proportion correct:
  print("Prop correct for test data")
  print(nrow(testData[correct_items,])/nrow(testData))
  
  # proportion incorrect:
  print("Prop incorrect for test data")
  print(nrow(testData[!correct_items,])/nrow(testData))
  
  
}
medianSplit<-function()
{
 #4.3
  split<-matrix(,nrow=3,ncol=3,byrow=TRUE)
  rownames(split)<-c("Letters","Faces","Exclamation Marks")
  colnames(split)<-c("Split1","Split2","Split3")

median_nr_pix<-median(csvF$nr_pix)
median_aspect_ratio<-median(csvF$aspect_ratio)
median_neigh_1<-median(csvF$neigh_1)
csvF$split1=as.factor(ifelse(csvF$nr_pix>median_nr_pix,1,0 ))
csvF$split2=as.factor(ifelse(csvF$aspect_ratio>median_aspect_ratio,1,0 ))
csvF$split3=as.factor(ifelse(csvF$neigh_1>median_neigh_1,1,0 ))



prop_Nr_Pix_letters<-0
nr_above_median_nr_pix<-0
prop_Aspect_Ratio_letters<-0
nr_above_median_aspect_ratio<-0
prop_neigh_1_Letters_letters<-0
nr_above_median_neigh_1<-0

for (i in 1:(80))
{
  if (csvF[i,19]==1)
  {
    nr_above_median_nr_pix<-nr_above_median_nr_pix+1
  }
  if (csvF[i,20]==1)
  {
    nr_above_median_aspect_ratio<-nr_above_median_aspect_ratio+1
    
  }
  if (csvF[i,21]==1)
  {
    nr_above_median_neigh_1<-nr_above_median_neigh_1+1
  }
}
prop_Nr_Pix_letters<- (nr_above_median_nr_pix / 80)
prop_Aspect_Ratio_letters<-(nr_above_median_aspect_ratio / 80)
prop_neigh_1_letters<-(nr_above_median_neigh_1 / 80)
split[1,1]=prop_Nr_Pix_letters
split[1,2]=prop_Aspect_Ratio_letters
split[1,3]=prop_neigh_1_letters


prop_Nr_Pix_faces<-0
nr_above_median_nr_pix<-0
prop_Aspect_Ratio_faces<-0
nr_above_median_aspect_ratio<-0
prop_neigh_1_Letters_faces<-0
nr_above_median_neigh_1<-0

  for (i in 81:(120))
  {
    if (csvF[i,19]==1)
    {
      nr_above_median_nr_pix<-nr_above_median_nr_pix+1
    }
    if (csvF[i,20]==1)
    {
      nr_above_median_aspect_ratio<-nr_above_median_aspect_ratio+1
      
    }
    if (csvF[i,21]==1)
    {
      nr_above_median_neigh_1<-nr_above_median_neigh_1+1
    }
  
}
prop_Nr_Pix_faces<- (nr_above_median_nr_pix / 40)
prop_Aspect_Ratio_faces<-(nr_above_median_aspect_ratio / 40)
prop_neigh_1_faces<-(nr_above_median_neigh_1 / 40)
split[2,1]=prop_Nr_Pix_faces
split[2,2]=prop_Aspect_Ratio_faces
split[2,3]=prop_neigh_1_faces


prop_Nr_Pix_xclaim<-0
nr_above_median_nr_pix<-0
prop_Aspect_Ratio_xclaim<-0
nr_above_median_aspect_ratio<-0
prop_neigh_1_Letters_xclaim<-0
nr_above_median_neigh_1<-0

for (i in 121:(140))
{
  if (csvF[i,19]==1)
  {
    nr_above_median_nr_pix<-nr_above_median_nr_pix+1
  }
  if (csvF[i,20]==1)
  {
    nr_above_median_aspect_ratio<-nr_above_median_aspect_ratio+1
    
  }
  if (csvF[i,21]==1)
  {
    nr_above_median_neigh_1<-nr_above_median_neigh_1+1
  }
  }
prop_Nr_Pix_xclaim<- (nr_above_median_nr_pix / 20)
prop_Aspect_Ratio_xclaim<-(nr_above_median_aspect_ratio / 20)
prop_neigh_1_xclaim<-(nr_above_median_neigh_1 / 20)
split[3,1]=prop_Nr_Pix_xclaim
split[3,2]=prop_Aspect_Ratio_xclaim
split[3,3]=prop_neigh_1_xclaim
print(split)


#4.4
for (i in 81:(120))
{
  if (csvF[i,19]==0)
  {
   
    csvF[i,19]=1
 
    break
  }
}
for (i in 81:(120))
{
  if (csvF[i,20]==0)
  {
  
    csvF[i,20]=1
  
    break
  }
}
for (i in 81:(120))
{
  if (csvF[i,21]==0)
  {
  
    csvF[i,21]=1
   
    break
  }
}

for (i in 1:(80))
{
  if (csvF[i,19]==0)
  {
   
    csvF[i,19]=1
   
    break
  }
}

for (i in 121:(140))
{
  if (csvF[i,20]==0)
  {
   
    csvF[i,20]=1
   
    break
  }
}
for (i in 121:(140))
{
  if (csvF[i,21]==0)
  {
   
    csvF[i,21]=1
   
    break
  }
}

letters1<-(prop_Nr_Pix_letters * prop_Aspect_Ratio_letters * prop_neigh_1_letters * (80 / 140))
letters2<-((1-prop_Nr_Pix_letters) * (1-prop_Aspect_Ratio_letters) * (1-prop_neigh_1_letters) * (80 / 140))

faces1<-(prop_Nr_Pix_faces * prop_Aspect_Ratio_faces * prop_neigh_1_faces * (40 / 140))
faces2<-((1-prop_Nr_Pix_faces) * (1-prop_Aspect_Ratio_faces) * (1-prop_neigh_1_faces) * (40 / 140))

xclaim1<-(prop_Nr_Pix_xclaim * prop_Aspect_Ratio_xclaim * prop_neigh_1_xclaim * (20 / 140))
xclaim2<-((1-prop_Nr_Pix_xclaim) * (1-prop_Aspect_Ratio_xclaim) * (1-prop_neigh_1_xclaim) * (20 / 140))

print("Naive bayes classifer for all 1s in letters")
print(letters1)
print("Naive bayes classifer for all 0s in letters")
print(letters2)
print("Naive bayes classifer for all 1s in faces")
print(faces1)
print("Naive bayes classifer for all 0s in faces")
print(faces2)
print("Naive bayes classifer for all 1s in xclaim")
print(xclaim1)
print("Naive bayes classifer for all 0s in xclaim")
print(xclaim2)
}
csvF<-read.csv(file ="40293751_features.csv" ,header = TRUE)


predictAsceptRatio() #4.1
## @knitr Pr4.2
logisticRegression()#4.2
## @knitr Pr4.3&4
csvF<-read.csv(file ="40293751_features.csv" ,header = TRUE)
medianSplit()#4.3 and 4.4


