chooseCRANmirror(81)

### The following lines only have to be executed onces. afterward, you can comment them out by adding '#' in front of the lines
install.packages("caret"); # Need to install a package calld caret for machine learning
install.packages("e1071"); # Need to install a package called e1071 for Support Vector Machine

##############################################################
### Assessment task: developing a machine-learning classifier.
##############################################################

### Read data files (default example: diagnosis of systemic lupus erthymatosus by an antibody microarray)

### Load the feature annotation file
annot<-read.table("annot.txt",sep="\t", stringsAsFactors=F,header=T)

### Load the diagnosis labels from a text file called 'diagnosis.txt'
diagnosis<-strsplit(readLines("diagnosis.txt"),"\t")[[1]]

### Load the data matrix, downlaoded with modification from https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE27293
D<-read.table("GSE108687_series_matrix.txt",sep="\t", stringsAsFactors=F,header=T,fill=TRUE);

# D<-read.table("GSE.txt",sep="\t", stringsAsFactors=F,header=T,fill=TRUE);



### Match up the feature ID with the actual antibody annotation
featureID<-D[,1];   ## Extract the feature - in this case, the feature ID is located in the first column
dd<-as.matrix(D[,-1]);  ## Extract the expression matrix - in this case, the data matrix is everything except the first column
rownames(dd)<-annot[match(featureID,annot[,1]),"ID"]   ## Assign the antibody names as rownames

dd[1:3,1:4]  # Display the first 3 rows and the first four columns

dim(dd)   ### Check the size (n x m) of this two dimensional matrix

table(diagnosis)  ### Look at the distribution of cases based on disease groups


diagnosis.list<-list();  ### This is a list that contains the indices of all samples that have the same diagnosis
all.diagnosis<-unique(diagnosis);
names(all.diagnosis)<-all.diagnosis;
list.diagnosis<-lapply(all.diagnosis,function(label){
  which(diagnosis==label)
});

list.diagnosis   # shows the content of the list

# This helps create a vector of colour names for all the samples based on their diagnosis
colours <- rep("grey", length(diagnosis));  # create a vector 'colours' by repeating the word 'grey' length(diagnosis) times
colours[which(diagnosis=="tissue: Normal Mesenteric Tissues")]<-"blue";
colours[which(diagnosis=="tissue: DSRCT tumor")]<-"yellow";
colours[which(diagnosis=="tissue: ES Tumor")]<-"orange";
colours[which(diagnosis=="SLE_active")]<-"red";
colours[which(diagnosis=="Rheumatoid arthritis")]<-"green";
colours[which(diagnosis=="Other_Autoimmune")]<-"grey";

control<-"Normal Mesenteric Tissues";    # specifying which label is the control gorup

res<-list();   # This list saves all the t.test results

### Perform t-test on each feature (antibody) to identify those that have significant differential expression between a disease group and healthy controls

for( protein in rownames(dd) ){
  expression<-dd[protein,];
  t.results<-lapply(list.diagnosis, function(this.ind){
    t.in<-t.test(expression[this.ind],
                 expression[list.diagnosis[[2]]], alternative="two.sided", paired=F);
  })
  diagnosis.colours<-sapply(t.results,function(t.result){
    # The code here creates a rule that colour a statistically significant test as red.
    # The p-value cut-off here is 0.001
    if(t.result$p.value<0.001){
      return("red");
    }
    else{
      return("grey")
    }
  })
  diagnosis.colours<-diagnosis.colours[order(names(diagnosis.colours))]
  ### One boxplot will be generated for each antibody (feature), with significant differential expression highlighted in red.
  png(file=paste("plots/boxplot_",antibody,".png",sep=""),width=400, height=450);
  par(mar=c(10,4,4,2))
  boxplot(exp~diagnosis, data.frame("exp"=dd[antibody,], diagnosis),outline=T, las=2,col=diagnosis.colours,frame=F, ylab="Expression", main=antibody)
  dev.off();
  res[[antibody]]<-t.results;
}


### Principal component analysis - for dimensionality reductio
pca <- prcomp(t(dd),scale=F);
summary(pca)    # Idenifying the variance explained by each principal component (PC)

png(file="plots/pca.png",width=600,height=600)
plot(pca$x[,1], pca$x[,2], col=colours, pch=16, main="Principal Component Analysis", xlab="PC1", ylab="PC2",  cex=2, bty="n");
legend("bottomright",pch=16,legend=c("Normal", "SLE_inactive","SLE_semiactive", "SLE_active", "Rheumatoid arthritis", "Other_Autoimmune"),col=c("blue","yellow","orange","red","green","grey"))
dev.off();


##### Develop and evaluate a supervised classifer for normal vs. SLE-semiactive

# This loads the 'caret' package
library(caret)
library(e1071)

set.seed(12);    # Set the seed for the random number generator. This way, our results will be identical

#ind.subset<-which(diagnosis=="Normal" | diagnosis=="SLE_inactive" | diagnosis=="SLE_active" );
ind.subset<-which(diagnosis=="Normal" | diagnosis=="SLE_inactive" );
dd.subset<-dd[,ind.subset]
diagnosis.subset<-diagnosis[ind.subset];

trainIndex <- createDataPartition(diagnosis.subset, p = 0.6, list = FALSE,   times = 1)    # Use 60% of the samples for training


dd.frame<-data.frame(t(dd.subset)[trainIndex,],"diagnosis"=diagnosis.subset[trainIndex])   # Create a laballed training data set
dd.frame[1:3,]   # Showing the first three samples in the labelled data set

## Training and evaluation
control <- trainControl(method="cv", number=3, classProbs=F, savePrediction=T  );   # This is 5-fold cross-validation  #classProbs=F, savePrediction=T

fit.svm <- train(diagnosis~., data=dd.frame, method="svmRadialCost", trControl=control)  # This operation trains an SVM classifier

dd.test.frame<-data.frame(t(dd.subset)[-trainIndex,],"diagnosis"=diagnosis.subset[-trainIndex])

### Making prediction on unseen samples
predictions.svm<-predict.train(object=fit.svm, dd.test.frame); # Perform prdiction using the SVM classifier

table(predictions.svm,dd.test.frame[,"diagnosis"])  #Compute the confusion matrix
