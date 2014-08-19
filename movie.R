'scrape_ratings' <- function(url)
{
  require(XML)
  #   Get HTML of url
  doc <- htmlParse(url)
  
  #   Find all tables in the imdb webpage
  tables <- readHTMLTable(doc)
  
  #   Find largest table and return as dataframe
  nrows <- unlist(lapply(tables, function(t) dim(t)[1]))
  table <- tables[[which.max(nrows)]]
  
  return(table)
}


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


#a<-sub("\n.*", "", film.ratings$V3)  #movie name
k<-0
n<-c(1:5,8:9,11:20,23:26,28:34,38:40,45:50,52:55,57:59,61,62,64,65,67:69,72:73,75:78,80,84,87:88,90:100)
n<-c(n,101,105:107,112,114:116,118,121,122,125:129,131:134,136,139,140)
n<-c(n,143:147,149:150)
#n<-n1

#142,124,111,104,83,71,,63,44,42,37,27,22,7

for(i in n)
{
  mvdata[i]<-sub("\n.*", "", film.ratings$V3[i])
 
  mvdata[i]<- sub(mvdata[i], "", film.ratings$V3[i])
 
  #a<-sub(a, "", film.ratings$V3[1])
  mvdata[i]<-sub("\n","",mvdata[i])
 
  mvdata[i]<-sub("\\(","",mvdata[i])
  year[i]<-gsub("\\s", "",sub("\\).*", "", mvdata[i]))   # removing blank spaces in the beginning and )
 
  b[i]<-sub("\n.*", "", mvdata[i]) #removing year
  mvdata[i]<-sub(b[i], "", mvdata[i])
 
 
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
  #k<-k+1
  #foo[i][] <- data.frame(do.call('rbind', strsplit(as.character(genre[i]),'|',fixed=TRUE)))
}

#na.omit(a)

foo <- data.frame(do.call('rbind', strsplit(as.character(na.omit(genre[genre!=""])),'|',fixed=TRUE)),stringsAsFactors=F)
foo$ratings<-as.numeric(na.omit(ratings[ratings!=""]))
foo$year<-as.numeric(na.omit(year[ratings!=""]))
foo$duration<-na.omit(duration[duration!=""])

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


#exp
#############################
#y <- sub("[[:digit:]]+","",film.ratings$V3[1])
#:(.*)
#
#grepl("\\<is\\>", c("this", "who is it?", "is it?", "it is!", "iso"))
#[1] FALSE  TRUE  TRUE  TRUE FALSE
#
#sub("\\<(*)\\>","",film.ratings$V3[1])
#
#
#foo <- data.frame(do.call('rbind', strsplit(as.character(genre),'|',fixed=TRUE)))
#
#List <- lapply(as.list(Data$col1), type.convert, as.is=TRUE)
