install.packages("plyr")
install.packages("ggplot2")
install.packages("googleVis")

library(plyr)
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

#defining the variables as it gives an error of variables not defined

urla<-0;film.ratings<-data.frame(V1=NA,V2=NA,V3=NA)
film<-film.ratings
url <-paste("http://www.imdb.com/search/title?at=0&languages=hi%7C1&sort=moviemeter,asc&start=1&title_type=feature")
#film.ratings <- scrape_ratings(url)
#film<-film.ratings
colnames(film)<-c("V1","V2","V3")
colnames(film.ratings)<-c("V1","V2","V3")

# The starting index of the pages
# The indices can be added or a for/while loop/ a seq can be put as per your convenience

#pgno is the last page that will be scraped
#pgno=301
#pgstrt=seq(1,pgno, by = 50)

#here pgstrt is manually entered
pgstrt=c(1,51,101);
k<-0
  
  
  tmp<-"a";ratings<-1;duration<-1;b<-1;mvdatatmp<-1;genre<-"a"
  year<-1;tmp1<-1;mvdata<-0
  
  
  urlno<-0
  
  
  
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
  Genreyear<-0
  od<-order(foo$year)
  foo<-foo[od,]
  
  j<-1;k<-0
  m<-1
  i<-1
  rows<-nrow(foo)
  n<-1:rows
  foo[rows+1,]<-foo[rows,]

    while(i %in% n)
      { #Actionrt[m]<-0;Actioncnt[m]<-0
        Actionrt[m]<-0; Comedyrt[m]<-0; Romancert[m]<-0; Thrillerrt[m]<-0; Crimert[m]<-0;Dramart[m]<-0;
        Actioncnt[m]<-0; Comedycnt[m]<-0; Romancecnt[m]<-0; Thrillercnt[m]<-0; Crimecnt[m]<-0;Dramacnt[m]<-0;
        
        Genreyear[m]<-0
        k<-j
        #j<-i
        cat("Year:",foo$year[j],"\t")
          while(j==k & i<rows)
            { 
                #i<-i+1
                Genreyear[m]<-foo$year[i]
                bol<-foo[i,] %in% "Action"
            
                if(any(bol==T))
                { 
                  #Genreyear[m]<-foo$year[i]
            
                  Actionrt[m]<-Actionrt[m]+foo$ratings[i]
                  Actioncnt[m]<-Actioncnt[m]+1
              
                  cat("\tActionrt[",m,"]:",Actionrt[m])
                }
            
                bol<-foo[i,] %in% "Comedy"
            
                if(any(bol==T))
                {
                  Comedyrt[m]<-Comedyrt[m]+foo$ratings[i]
                  Comedycnt[m]<-Comedycnt[m]+1
                }
            
                bol<-foo[i,] %in% "Romance"
            
                if(any(bol==T))
                {
                  Romancert[m]<-Romancert[m]+foo$ratings[i]
                  Romancecnt[m]<-Romancecnt[m]+1
                }
            
                bol<-foo[i,] %in% "Drama"
                
                if(any(bol==T))
                {
                  Dramart[m]<-Dramart[m]+foo$ratings[i]
                  Dramacnt[m]<-Dramacnt[m]+1
                }
            
                bol<-foo[i,] %in% "Thriller"
                
                if(any(bol==T))
                {
                  Thrillerrt[m]<-Thrillerrt[m]+foo$ratings[i]
                  Thrillercnt[m]<-Thrillercnt[m]+1
                }
            
                bol<-foo[i,] %in% "Crime"
          
                if(any(bol==T))
                {
                  Crimert[m]<-Crimert[m]+foo$ratings[i]
                  Crimecnt[m]<-Crimecnt[m]+1
                }
    
          
                if(foo$year[j]!=foo$year[j+1])
                  {k<-0
                   m<-m+1
                  }
          
                #cat("in:",i,"\t")
                #cat("\t j:",j,"\t")
                #cat("year",foo$year[j])
          
                if(foo$year[j]==foo$year[j+1])
                {i<-i+1}
          
                j<-j+1;k<-k+1;
          }
    
        cat("\t outn:",i,"\n")
        i<-i+1
  
    }

  foo<-foo[-(rows+1),]


  #Data frame creation of all the above computed values
  #Action
  ansperyear<-data.frame(Genre="Action",Year=Genreyear,Ratings=Actionrt,Count=Actioncnt)
  
  #Comedy
  ansperyeartemp<-data.frame(Genre="Comedy",Year=Genreyear,Ratings=Comedyrt,Count=Comedycnt)
  ansperyear<-rbind(ansperyear,ansperyeartemp)
  
  #Romance
  ansperyeartemp<-data.frame(Genre="Romance",Year=Genreyear,Ratings=Romancert,Count=Romancecnt)
  ansperyear<-rbind(ansperyear,ansperyeartemp)
  
  #Thriller
  ansperyeartemp<-data.frame(Genre="Thriller",Year=Genreyear,Ratings=Thrillerrt,Count=Thrillercnt)
  ansperyear<-rbind(ansperyear,ansperyeartemp)
  
  #Crime
  ansperyeartemp<-data.frame(Genre="Crime",Year=Genreyear,Ratings=Crimert,Count=Crimecnt)
  ansperyear<-rbind(ansperyear,ansperyeartemp)
  
  #Drama
  ansperyeartemp<-data.frame(Genre="Drama",Year=Genreyear,Ratings=Dramart,Count=Dramacnt)
  ansperyear<-rbind(ansperyear,ansperyeartemp)
  
  
  #dataframe ansperyear contains all the recordings of every genre for every year individually
  ansperyear1<-ansperyear[ansperyear$Count>0,]
  ansperyear1<- ddply(ansperyear, "Count", transform, label_y=cumsum(Count)) 
  
 
  
  # a plot with graphs build over one another and a transperancy value is set
  ggplot(ansperyear, aes(x=Year,y=Count,fill=Genre)) + 
    geom_histogram(stat="identity",position="identity",alpha=0.4) 
  
  #ggplot(ansperyear1, aes(x=Year, y=Count, fill=Genre)) +    geom_bar(stat="identity")
 
  # plot using facet_grid to show the data of different genres differently
  ggplot(ansperyear, aes(x=Year,y=Count)) + 
    geom_bar(stat="identity") + scale_y_continuous(expand = c(0,0.2)) + 
    facet_grid(Genre ~ .,scales="free")+
    theme(panel.background = element_rect(fill = "white",color="black",linetype="dotted"))
  
  
  #Variable being summed up together
  Actioncnttot<-sum(Actioncnt);Comedycnttot<-sum(Comedycnt);Romancecnttot<-sum(Romancecnt);Thrillercnttot<-sum(Thrillercnt); Crimecnttot<-sum(Crimecnt);Dramacnttot<-sum(Dramacnt)
  Actionrttot<-sum(Actionrt);Comedyrttot<-sum(Comedyrt);Romancerttot<-sum(Romancert);Thrillerrttot<-sum(Thrillerrt); Crimerttot<-sum(Crimert);Dramarttot<-sum(Dramart)
  
    ans<-data.frame(Genre=c("Action","Comedy","Romance","Thriller","Crime","Drama"))
    ans$cnt<-c(Actioncnttot,Comedycnttot,Romancecnttot,Thrillercnttot, Crimecnttot,Dramacnttot)
    ans$totrat<-c(Actionrttot,Comedyrttot,Romancerttot,Thrillerrttot, Crimerttot,Dramarttot)
    ans$avgrat<-c(Actionrttot/Actioncnttot,Comedyrttot/Comedycnttot,Romancerttot/Romancecnttot,Thrillerrttot/Thrillercnttot, Crimerttot/Crimecnttot,Dramarttot/Dramacnttot)
    
  ggplot(ansperyear1, aes(x=Year, y=Count, fill=Genre)) + 
    geom_bar(position="dodge",stat="identity")+ 
    geom_text(aes(label=Count),  colour="white")
  
  
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
    
