#20201216
#建立except.root.tabel2
except.root.labels2 <-  allClasses[allClasses != "root"]
except.root.tabel2 <- as.data.frame(matrix(data=0,nrow=length(dataset2[,1]),ncol=length(except.root.labels2)))
colnames(except.root.tabel2) <- except.root.labels2
rownames(except.root.tabel2) <- dataset2[,1]
nrowDataset2 <- nrow(dataset2)
ncolDataset2 <- ncol(dataset2)
#indexClassesLevels里去掉根节点的路径
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
  #找到数据集的GO标签在allclasses中的位置，在dataframeclasses中对应位置将全部父类标记为1（通过indexClassesLevels的数据）
  #There are instances that are assigned only to the illegal classes.
  #In this case they will not be classified in any class 	
  if(length(classes) > 0){
    #Set to 1 the class position and the position of all superclasses of the class
    #allPositions是样本所属类的位置，indexClassesLevels[allPositions]是类的所有父类的索引位置。
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