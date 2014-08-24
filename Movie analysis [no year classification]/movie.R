library(ggplot2)
library(googleVis)

'scrape_ratings' <- function(url)
{
  require(XML)
  # get HTML of url
  doc <- htmlParse(url)
  
  # find all tables in webpage
  tables <- readHTMLTable(doc)
  
  # find largest table and return as dataframe
  nrows <- unlist(lapply(tables, function(t) dim(t)[1]))
  table <- tables[[which.max(nrows)]]
  
  return(table)
}

'substrRight' <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


urla<-0;film.ratings<-data.frame(V1=NA,V2=NA,V3=NA)
film<-film.ratings
url <-paste("http://www.imdb.com/search/title?at=0&languages=hi%7C1&sort=moviemeter,asc&start=1&title_type=feature")
#film.ratings <- scrape_ratings(url)
#film<-film.ratings
colnames(film)<-c("V1","V2","V3")
colnames(film.ratings)<-c("V1","V2","V3")
pgstrt=c(1,51,101);k<-0
for(i in pgstrt){
  k<-k+1
  #NOTE: The ordering type can be changed by changing the parameter 'moviemeter' in
  #      the below code mentioned after sort to your desired  type
  urla[k] <-paste("http://www.imdb.com/search/title?at=0&languages=hi%7C1&sort=moviemeter,asc&start=",i,"&title_type=feature")
  url<-urla[k]
  cat("\nfilm nrow",nrow(film))
  cat("\nfilm.ratings nrow",nrow(film.ratings))
  film<- scrape_ratings(url)

#defining the variables as it gives an error of variables not defined

# Variables for storing the url of the website
urla<-0,url<-0;

# pre defining of the data frame that will store the movie data
film.ratings<-data.frame(V1=NA,V2=NA,V3=NA)
film<-film.ratings
colnames(film)<-c("V1","V2","V3")
colnames(film.ratings)<-c("V1","V2","V3")

tmp<-"a";ratings<-1;duration<-1;b<-1;mvdatatmp<-1;genre<-"a"
year<-1;tmp1<-1;mvdata<-0


# The starting index of the pages
pgstrt=c(1,51,101)
urlno<-0


#url <-paste("http://www.imdb.com/search/title?at=0&languages=hi%7C1&sort=moviemeter,asc&start=1&title_type=feature")

#film.ratings <- scrape_ratings(url)
#film<-film.ratings

for(i in pgstrt)
  {
  
    urlno<-urlno+1
    
    # url being incrementally entered and processed
    urla[urlno] <-paste("http://www.imdb.com/search/title?at=0&languages=hi%7C1&sort=moviemeter,asc&start=",i,"&title_type=feature")
    url<-urla[urlno]
    
    #cat("\nfilm nrow",nrow(film))
    #cat("\nfilm.ratings nrow",nrow(film.ratings))
    
    # url is passed as a parameter to the function scrape_ratings
    film<- scrape_ratings(url)


  z<-rbind(film.ratings,film)
  film.ratings<-z
}
# names of the movies
sub("\n.*", "", film.ratings$V3)

tmp<-"a";ratings<-1;duration<-1;b<-1;mvdatatmp<-1;genre<-"a"
year<-1;tmp1<-1;mvdata<-0;k<-0

#a<-sub("\n.*", "", film.ratings$V3)  #movie name

#manual scraping values used in earlies commit
#n1<-c(1:5,8:9,11:20,23:26,28:34,38:40,45:50,52:55,57:59,61,62,64,65,67:69,72:73,75:78,80,84,87:88,90:100)
#n1<-c(n1,101,105:107,112,114:116,118,121,122,125:129,131:134,136,139,140)
#n1<-c(n1,143:147,149:150)
#n<-n1
n<-1:150
#142,124,111,104,83,71,,63,44,42,37,27,22,7

for(i in n)
{mvdata[i]<-sub("\n.*", "", film.ratings$V3[i])
 
 mvdata[i]<- sub(mvdata[i], "", film.ratings$V3[i])
 
 #a<-sub(a, "", film.ratings$V3[1])
 mvdata[i]<-sub("\n","",mvdata[i])
 
 mvdata[i]<-sub("\\(","",mvdata[i])
 year[i]<-gsub("\\s", "",sub("\\).*", "", mvdata[i]))   # removing blank spaces in the beginning and )
 
 #b[124]<-sub("\\(.*","",a[124])
 #a[124]<-sub(b[124],"",a[124])
 #while()
 #############
 while(is.na(as.numeric(year[i])) & !is.na(year[i])){
   temp<-sub("\\(.*","",mvdata[i])
   mvdata[i]<-sub(temp,"",mvdata[i])
   mvdata[i]<-sub("\\(","",mvdata[i])
   year[i]<-gsub("\\s", "",sub("\\).*", "", mvdata[i]))
   b[i]<-sub("\n.*", "", mvdata[i]) #removing year
   mvdata[i]<-sub(b[i], "", mvdata[i])
 }
 
 ########
 #tmp[i]<-sub(").*","",mvdata[i])
 
 #mvdata[i]<-sub(tmp[i],"",mvdata[i])
 
 tmp[i]<-sub("10.*","",mvdata[i])
 mvdata[i]<-sub(tmp[i],"",mvdata[i])
 
 mvdata[i]<-sub("10\n\n","",mvdata[i])
 
 ratings[i]<-sub("/.*", "", mvdata[i])
 duration[i]<-sub(".*\n", "", mvdata[i])
 duration[i]<-gsub("\\s", "", duration[i]) #time
 
 #part 2 extracting genres
 tmp1[i]<-sub(".*\n", "", mvdata[i])
 mvdatatmp[i]<-sub(tmp1[i],"",mvdata[i])
 mvdatatmp[i]<-sub("\n    \n$","",mvdatatmp[i])
 
 genre[i]<-sub(".*\n", "", mvdatatmp[i])
 genre[i]<-gsub("\\s", "", genre[i])
}
genre[genre==""]<-NA



foo <- data.frame(do.call('rbind', strsplit(as.character(genre),'|',fixed=TRUE)),stringsAsFactors=F)
foo$ratings<-as.numeric(ratings)
foo$year<-as.numeric(year)
duration[substrRight(duration,5)!="mins."]<-NA
foo$duration<-duration[substrRight(duration,5)=="mins."]
foo<-foo[complete.cases(foo),]


#Action Comedy Romance Thriller Drama Crime Musical
#Adventure Sci-Fi Mystery History Family

bol<-T
Actionrt<-0; Comedyrt<-0; Romancert<-0; Thrillerrt<-0; Crimert<-0;Dramart<-0;
Actioncnt<-0; Comedycnt<-0; Romancecnt<-0; Thrillercnt<-0; Crimecnt<-0;Dramacnt<-0;
for(i in n)
{
  bol<-foo[i,] %in% "Action"
  if(any(bol==T))
  {
    Actionrt<-Actionrt+foo$ratings[i]
    Actioncnt<-Actioncnt+1
  }
  
  bol<-foo[i,] %in% "Comedy"
  if(any(bol==T))
  {
    Comedyrt<-Comedyrt+foo$ratings[i]
    Comedycnt<-Comedycnt+1
  }
  
  bol<-foo[i,] %in% "Romance"
  if(any(bol==T))
  {
    Romancert<-Romancert+foo$ratings[i]
    Romancecnt<-Romancecnt+1
  }
  
  bol<-foo[i,] %in% "Drama"
  if(any(bol==T))
  {
    Dramart<-Dramart+foo$ratings[i]
    Dramacnt<-Dramacnt+1
  }
  
  bol<-foo[i,] %in% "Thriller"
  if(any(bol==T))
  {
    Thrillerrt<-Thrillerrt+foo$ratings[i]
    Thrillercnt<-Thrillercnt+1
  }
  
  bol<-foo[i,] %in% "Crime"
  if(any(bol==T))
  {
    Crimert<-Crimert+foo$ratings[i]
    Crimecnt<-Crimecnt+1
  }
}

ans<-data.frame(Genre=c("Action","Comedy","Romance","Thriller","Crime","Drama"))
ans$Cnt<-c(Actioncnt,Comedycnt,Romancecnt,Thrillercnt, Crimecnt,Dramacnt)
ans$totrat<-c(Actionrt,Comedyrt,Romancert,Thrillerrt, Crimert,Dramart)
ans$avgrat<-c(Actionrt/Actioncnt,Comedyrt/Comedycnt,Romancert/Romancecnt,Thrillerrt/Thrillercnt, Crimert/Crimecnt,Dramart/Dramacnt)

# Plots

ggplot(ans, aes(x=Genre, y=Cnt)) +    geom_bar(stat="identity", colour="black")+ ylab("Genre Count") + geom_text(aes(label=Cnt), vjust=-0.2)

# a take at the googleVis package
#plot through that package
newdf=data.frame(country=ans$Genre, 
              GenreCount=ans$Cnt, 
              TotalRating=ans$totrat)

Area <- gvisAreaChart(newdf)
plot(Area)

#ggplot2 package graph


ggplot(ans, aes(x=avgrat, y=totrat, fill=Genre)) +  
  geom_bar(stat="identity", position="dodge") +  
  geom_text(aes(label=totrat), vjust=-1.5, colour="black",      
            position=position_dodge(.9), size=3)+
  xlim(min(ans$avgrat)-0.2,max(ans$avgrat)+0.2)+
  xlab("Average Rating")+ ylab("Total Rating")
