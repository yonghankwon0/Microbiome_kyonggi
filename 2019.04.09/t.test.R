#메타데이터 부르기
meta<-read.delim(file.choose())

#데이터 확인
str(meta)

#데이터 분류
meta_cancer<-subset(meta,dx=="cancer")
meta_normal<-subset(meta,dx=="normal")

#otu데이터 부르기
otu<-read.delim(file.choose())

#데이터 순서 비교
meta$sample==otu$Group

#shannon index 만들기 위한 데이터 생성
otu_1<-otu[,5:length(otu)-1]

#메타 데이터에서 찾은 cancer,normal을 otu에서 분류
otu_gem_cancer<-otu_gem[which(meta$dx=="cancer"),]
otu_gem_normal<-otu_gem[which(meta$dx=="normal"),]


#shannon index 계산 함수
function1<-function(a){
  pi<-otu_gem_cancer[a,]/as.numeric(apply(otu_gem_cancer,1,sum))[a]
  print(-1*sum(pi*log(pi),na.rm=T))
}
function2<-function(b){
  pi<-otu_gem_normal[b,]/as.numeric(apply(otu_gem_normal,1,sum))[b]
  print(-1*sum(pi*log(pi),na.rm=T))
}
dim(otu_gem_cancer)
dim(otu_gem_normal)
A_list<-list(1:120)
B_list<-list(1:172)

# Shannon diversity (6.3)식
-1*sum(pi*log(pi),na.rm=T)

# 계산
for(i in seq(1,dim(otu_gem_cancer)[1],by=1)){
  A_list[i]<-function1(i)
}

for(i in seq(1,dim(otu_gem_normal)[1],by=1)){
  B_list[i]<-function2(i)
}

#


#t.test
A<-as.numeric(A_list)
B<-as.numeric(B_list)

t.test(A,B)
