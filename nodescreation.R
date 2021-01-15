#20201216���������ݼ���GO����
#��ȡclassesPerLevel��go.for.level2
listexrootClassesLevels <- listAllClassesLevels[-grep("root",allClasses)]
listexrootClassesLevels <- listexrootClassesLevels[-invalidindex]
#ȥ��listexrootClassesLevels�ĵ�һ��Ԫ��
listexrootClassesPaths <- vector("list")
#����eachnodeslevels��һȥ��list��ĵ�һ��Ԫ�أ��ڵ�������������·����һ����string��a/b/c/d�������˳��Ķ��Ԫ�ء�a����b����c����d��
eachnodeslevels <- vector("list",length = length(listexrootClassesPaths))
for (t in 1:length(listexrootClassesLevels)) {
  listexrootClassesPaths[[t]] <- listexrootClassesLevels[[t]][-1]
  eachnodeslevels[[t]] <- strsplit(listexrootClassesPaths[[t]],"/")
  
}
eachnodeslevels2 <- eachnodeslevels

levelClasses <- vector("numeric",length(listexrootClassesLevels))

for(j in 1:length(levelClasses)){
  numEdges <- vector()
  allPaths <- listexrootClassesLevels[[j]][2:length(listexrootClassesLevels[[j]])]
  for(k in 1:length(allPaths)){
    numEdges <- c(numEdges,length(unlist(strsplit(allPaths[k],"/"))))
  }
  levelClasses[j] <- max(numEdges)
}

#cat(levelClasses,"\n")

#Number of levels
numLevels <- max(unique(levelClasses))
namesClasses <- vector("list",numLevels)
for(i in 2:numLevels){
  regExpClass <- paste("^",i,"$",sep="")
  posClassesLevel <- grep(regExpClass,levelClasses)
  for(j in 1:length(posClassesLevel)){
    namesClasses[[i]] <- c(namesClasses[[i]],listexrootClassesLevels[[posClassesLevel[j]]][1])
  }
}	
go.for.level2 <- namesClasses
go.for.level2[[1]] <- NULL


#��������nodes.to.index2

nodes.to.index2 <- as.list(1:length(except.root.labels2))
names(nodes.to.index2) <- except.root.labels2



#�����ӽڵ�,����nodes.to.children,nodes.to.parents,
children.nodes2 <- vector("list")
parents.nodes2 <- vector("list")
index2 <- vector("list")
children.index2 <- vector("list")
nextindex2 <- vector("list")

#names(index) <- except.root.labels2
nodes.to.children2 <- vector("list")
nodes.to.parents2 <- vector("list")
nodes.to.ancestors2 <- vector("list")
nodes.to.descendants2 <- vector("list")
#names(index) <- except.root.labels2
go.leaf.nodes2 <- vector()
for (i in 1:length(except.root.labels2)) 
{
  cat(i,"\n")
  #����class[i]��ȫ��listexrootclassesPaths��λ������
  parentclasses2 <- except.root.labels2[i]
  #index2��except.root.labels2[i]��listexrootClassesPaths���ֵ�λ��
  index2[[parentclasses2]] <- grep(parentclasses2,listexrootClassesPaths)
  #class[i]��ֻ����һ�Σ�index2[i]���ж��Ԫ��
  for (k in 1:length(index2[[i]])) {
    #�����ں�ĳclass��listexrootclassesPaths���ڲ�λ��
    aa <- vector()
    
    for (j in 1:length(eachnodeslevels[[index2[[i]][k]]])) {
      #ĳ���ڵ��paths����
      #cat(j,"\n")
      bb <- vector()
      
      if(sum(grepl(parentclasses2,eachnodeslevels[[index2[[i]][k]]][[j]])) != 0)
      {
      #bb��except.root.labels2[i]�ھ���ĳ���ڵ��j��paths������ֵ�λ��
      bb <- grep(parentclasses2,eachnodeslevels[[index2[[i]][k]]][[j]])
      #��eachnodeslevels�����ӽڵ㸸�ڵ�
      children.nodes2[[parentclasses2]] <- c(children.nodes2[[parentclasses2]],eachnodeslevels[[index2[[i]][k]]][[j]][bb+1])
      parents.nodes2[[parentclasses2]] <- c(parents.nodes2[[parentclasses2]],eachnodeslevels[[index2[[i]][k]]][[j]][bb-1])
      aa <- c(aa,bb)
       }
    }
    
    nextindex2[[k]] <- list(aa)
    #children.index2[[i]]�������ǽڵ��ܹ���·���г��ֵĴ�����children.index[[i]]���м���list��Ӧ��index2[[i]]��Ԫ�ظ���������ڵ��ܹ����ּ���
    #index2[[i]]���Ǵ�·��������k������1��Ӧchildren.index2[[i]]��ĵ�һ��list��list���Ԫ�ؾ����������·��k�µĶ��С·�������class���ڵ�λ�ã���NA��ȱʧ��
    children.index2[[except.root.labels2[i]]] <- c(children.index2[[except.root.labels2[i]]],nextindex2[[k]])
    
    }
  children.nodes2[[i]] <- unique(children.nodes2[[i]])
  parents.nodes2[[i]] <- unique(parents.nodes2[[i]])
  nodes.to.children2[[parentclasses2]] <- grep(paste(children.nodes2[[i]],collapse = "|"),except.root.labels2)
  nodes.to.parents2[[parentclasses2]] <- grep(paste(parents.nodes2[[i]],collapse = "|"),except.root.labels2)
  children.nodes2[[i]] <- na.omit(children.nodes2[[i]])
  #����Ҷ�ӽڵ�
  if (length(nodes.to.children2[[except.root.labels2[i]]]) == 0)
  {
    go.leaf.nodes2 <- c(go.leaf.nodes2,names(nodes.to.children2[except.root.labels2[i]]))
  }
  
  nodes.to.ancestors2[[except.root.labels2[i]]] <- grep(paste(exrootClassesPaths[[i]],collapse = "|"),except.root.labels2)
  ipos <- paste("^",i,"$",sep="")
  nodes.to.ancestors2[[except.root.labels2[i]]] <- nodes.to.ancestors2[[except.root.labels2[i]]][-grep(ipos,nodes.to.ancestors2[[except.root.labels2[i]]])]
  #����nodes.to.descendants2
  nodes.to.descendants2[[except.root.labels2[i]]] <- nodes.to.children2[[except.root.labels2[i]]]
  
  if(length(nodes.to.children2[[except.root.labels2[i]]]) > 0)
  {
  for (m in 1:length(nodes.to.children2[[except.root.labels2[i]]])) {
    nodes.to.descendants2[[except.root.labels2[i]]] <- c(nodes.to.descendants2[[except.root.labels2[i]]], nodes.to.children2[[except.root.labels2[nodes.to.children2[[except.root.labels2[i]]][m]]]])
  }
  }
  nodes.to.descendants2[[except.root.labels2[i]]] <- unique(nodes.to.descendants2[[except.root.labels2[i]]])
}


 