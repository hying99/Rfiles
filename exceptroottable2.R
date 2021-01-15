#20201216
#����except.root.tabel2
except.root.labels2 <-  allClasses[allClasses != "root"]
except.root.tabel2 <- as.data.frame(matrix(data=0,nrow=length(dataset2[,1]),ncol=length(except.root.labels2)))
colnames(except.root.tabel2) <- except.root.labels2
rownames(except.root.tabel2) <- dataset2[,1]
nrowDataset2 <- nrow(dataset2)
ncolDataset2 <- ncol(dataset2)
#indexClassesLevels��ȥ�����ڵ��·��
exrootindexClassesLevels <- vector("list",length(except.root.labels2))
exrootClassesPaths <- allClassesPaths[-grep("root",allClasses)] 

for (i in 1:length(except.root.labels2)) {
  exrootindexClassesLevels[[i]] <- grep(paste(exrootClassesPaths[[i]],collapse = "|"),except.root.labels2)
}
for(j in 1:nrowDataset2){
  classes <- as.character(dataset2[j,ncolDataset2])
  if(length(grep("@",classes) > 0)){
    classes <- unlist(strsplit(classes,"@"))
  }
  
  #remove the illegal classes
  posIllegal <- grep(paste(illegalClasses,collapse="|"),classes)
  if(length(posIllegal) > 0){
    classes <- classes[-posIllegal]
  }
  #�ҵ����ݼ���GO��ǩ��allclasses�е�λ�ã���dataframeclasses�ж�Ӧλ�ý�ȫ��������Ϊ1��ͨ��indexClassesLevels�����ݣ�
  #There are instances that are assigned only to the illegal classes.
  #In this case they will not be classified in any class 	
  if(length(classes) > 0){
    #Set to 1 the class position and the position of all superclasses of the class
    #allPositions�������������λ�ã�indexClassesLevels[allPositions]��������и��������λ�á�
    allPositions2 <- grep(paste(classes,collapse="|"),except.root.labels2)
    #zhangjp
    #zhangjp
    except.root.tabel2[j,unique(unlist(exrootindexClassesLevels[allPositions2]))] <- 1
  }
}
invalidindex <- vector()
for (k in 1:ncol(except.root.tabel2)) {
  if (sum(except.root.tabel2[,k]) <  100)
  {
    invalidindex <- c(invalidindex,k)
  }
    
}
invalidlabels <- except.root.labels2[invalidindex]
except.root.labels2 <- except.root.labels2[-invalidindex]
except.root.tabel2 <- except.root.tabel2[,-invalidindex]
exrootClassesPaths <- exrootClassesPaths[-invalidindex]