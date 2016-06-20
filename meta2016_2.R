# ----
# title : metadata2016_20130349
# author: HeeYeon Kim
# date : 2016.06.15 - 2016.06.20
# Reference : https://github.com/Annie201/meta2016/blob/master/term_clustering.R
# ----

library(rjson)
library(RCurl)
library(tm)
library(SnowballC)

# Combine json file
json_art<-fromJSON(file = "talks_art.json")
json_free<-fromJSON(file = "talks_free.json")
json_music<-fromJSON(file = "talks_music.json")
json_time<-fromJSON(file = "talks_time.json")
json_list<-list(json_art, json_free, json_music, json_time)
json_total<-toJSON(json_list)
json_total

# create total json file
write(json_total, file="talks_total.json")

# Set working dir path
j_file2<-"talks_total.json"

# Read a json file to workspace 
talk_creativ<-fromJSON(file=j_file2, method="C") 
str(talk_creativ) 


talk_names<-names(talk_creativ[[1]]$talks[[1]]$talk) 
#talk_names<-names(talk_creativ$talks[[1]]$talk)
# talks_total.json
#List of 4
#$ :List of 2
#..$ talks :List of 20
#.. ..$ :List of 1
#.. .. ..$ talk:List of 10
#list의 형태가 위와 같기 때문에 명령문 수정
talk_names 

###Convertobject type 
talk_creativ_list<-lapply(talk_creativ[[1]]$talks, function(x){unlist(x)}) 
#talk_creativ_list<-lapply(talk_creativ$talks, function(x){unlist(x)})
str(talk_creativ_list) 
parse_talk_all<-data.frame() 
df_parse_talk_all<-do.call("rbind", c(parse_talk_all, talk_creativ_list)) 
str(df_parse_talk_all) 

df_parse_talk_all<-data.frame(df_parse_talk_all) 
str(df_parse_talk_all) 

###Chage datatype of talk description 
df_parse_talk_all$talk.description<-as.character(df_parse_talk_all$talk.description) 
str(df_parse_talk_all$talk.description) 


###Change names of variables 
names(df_parse_talk_all)<-talk_names 
str(df_parse_talk_all) 

#####Term Clustering 

###Convert object type 
class(df_parse_talk_all$description) 
ted_docs <- Corpus(VectorSource(df_parse_talk_all$description)) 
class(ted_docs) 


###Pre-processing 
ted_docs <- tm_map(ted_docs, tolower) 
ted_docs <- tm_map(ted_docs, removeNumbers) 
ted_docs <- tm_map(ted_docs, removePunctuation) 
ted_docs <- tm_map(ted_docs, removeWords, stopwords("SMART")) 
ted_docs <- tm_map(ted_docs, removeWords, "ted") 
 

###Tokenizing 
strsplit_space_tokenizer <- function(x) 
   unlist(strsplit(as.character(x), "[[:space:]]+")) 
#token_docs<-(sapply(ted_docs, strsplit_space_tokenizer)) 
token_docs<-(sapply(ted_docs$content, strsplit_space_tokenizer)) 
token_freq<-table(unlist(token_docs)) 
summary(data.frame(token_freq)$Freq) 


###Stemming 
stem_docs <- sapply(token_docs, stemDocument) 
stem_freq<-table(unlist(stem_docs)) 
summary(data.frame(stem_freq)$Freq) 


df_stem_freq<-data.frame(stem_freq) 
str(df_stem_freq) 
 

###Term-Doc Matrix with Stemming 
class(stem_docs) 
stem_docs <- Corpus(VectorSource(stem_docs)) 
class(stem_docs) 


###term weight: TfIDF 
ted_tdm <- TermDocumentMatrix(stem_docs,
                              control = list(removePunctuation = TRUE, 
                                             weighting=weightTfIdf, 
                                             stopwords = TRUE))
inspect(ted_tdm[1,])

 

#####Hierachical Clustering: Term Clustering 
###Remove sparse terms 
ted_tdm_sparse <- removeSparseTerms(ted_tdm, sparse = 0.90) 
ted_tdm_sparse$nrow 
ted_tdm<-ted_tdm_sparse 
 

###Convert to matrix 
ted_m <- as.matrix(ted_tdm) 
 

###Calculate similarity 
###dist {stats} Distance Matrix Computation 
###scale {base} Scaling and Centering of Matrix-like Objects 
distMatrix<- dist(scale(ted_m)) 
 
#----
###Execute hierarchial clustering 
###hclust {stats} Hierarchical Clustering 
###method=c("ward.D") 
ward_fit <- hclust(distMatrix, method="ward.D") 
 
###Draw dendrogram(ward.D)
plot(ward_fit) 
###rect.hclust {stats} Draw Rectangles Around Hierarchical Clusters 
###k = number of clusters 
rect.hclust(ward_fit, k=11) 
 

###Save the dendrogram as PNG image file 
png("./total_ward.png", width = 1200, height=600) 
plot(ward_fit) 
###k= number of clusters 
rect.hclust(ward_fit, k=11) 
dev.off() 
 

###Assign a cluster to a term 
###cutree {stats} Cut a Tree into Groups of Data 
###k= number of clusters 
ward_groups <- cutree(ward_fit, k=11) 
ward2_groups <- data.frame(ward_groups) 
str(ward2_groups) 
ward2_groups$KWD <- rownames(ward2_groups) 
str(ward2_groups) 


###Write the clustering result to text file 
write.table(ward2_groups, "./total_ward.txt", row.names=FALSE, col.names=TRUE, sep="\t") 
#----

#----
###method=c("single") 
single_fit <- hclust(distMatrix, method="single") 

###Draw dendrogram(single)
plot(single_fit) 
rect.hclust(single_fit, k=10) 


###Save the dendrogram as PNG image file 
png("./total_single.png", width = 1200, height=600) 
plot(single_fit) 
###k= number of clusters 
rect.hclust(single_fit, k=10) 
dev.off() 


###Assign a cluster to a term 
###cutree {stats} Cut a Tree into Groups of Data 
###k= number of clusters 
single_groups <- cutree(single_fit, k=10) 
single2_groups <- data.frame(single_groups) 
str(single2_groups) 
single2_groups$KWD <- rownames(single2_groups) 
str(single2_groups) 


###Write the clustering result to text file 
write.table(single2_groups, "./total_single.txt", row.names=FALSE, col.names=TRUE, sep="\t") 
#----

#----
###method=c("complete") 
complete_fit <- hclust(distMatrix, method="complete") 

###Draw dendrogram(complete)
plot(complete_fit) 
rect.hclust(complete_fit, k=7) 


###Save the dendrogram as PNG image file 
png("./total_complete.png", width = 1200, height=600) 
plot(complete_fit) 
###k= number of clusters 
rect.hclust(complete_fit, k=7) 
dev.off() 


###Assign a cluster to a term 
###cutree {stats} Cut a Tree into Groups of Data 
###k= number of clusters 
complete_groups <- cutree(complete_fit, k=7) 
complete2_groups <- data.frame(complete_groups) 
str(complete2_groups) 
complete2_groups$KWD <- rownames(complete2_groups) 
str(complete2_groups) 


###Write the clustering result to text file 
write.table(complete2_groups, "./total_complete.txt", row.names=FALSE, col.names=TRUE, sep="\t") 
#----

#----
###method=c("average") 
average_fit <- hclust(distMatrix, method="average") 

###Draw dendrogram(average)
plot(average_fit) 
rect.hclust(average_fit, k=8) 


###Save the dendrogram as PNG image file 
png("./total_average.png", width = 1200, height=600) 
plot(average_fit) 
###k= number of clusters 
rect.hclust(average_fit, k=8) 
dev.off() 


###Assign a cluster to a term 
###cutree {stats} Cut a Tree into Groups of Data 
###k= number of clusters 
average_groups <- cutree(average_fit, k=8) 
average2_groups <- data.frame(average_groups) 
str(average2_groups) 
average2_groups$KWD <- rownames(average2_groups) 
str(average2_groups) 


###Write the clustering result to text file 
write.table(average2_groups, "./total_average.txt", row.names=FALSE, col.names=TRUE, sep="\t") 
#----