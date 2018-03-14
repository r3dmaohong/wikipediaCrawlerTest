#library(devtools)
#install_github("Ironholds/WikipediR")
library(WikipediR)
library(tcltk)
library(clipr)
library(ropencc)

gsubTags <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

( dat <- read_clip() )

en_dat <- dat[grepl('^[a-z].+[a-z]$', dat, ignore.case = TRUE)]
dat <- setdiff(dat, en_dat)
dat <- dat[dat!=""]

df <- data.frame('query'=dat, 'wiki_title' = '', 'content'='', 'html_content'='', stringsAsFactors = F)

for(i in 1:nrow(df)){
  print(paste0(i, ' of ', nrow(df)))
  string <- df$query[i]
  tryCatch({
    content <- page_content(language = 'zh', project = "wikipedia", page_name = string)
    df$wiki_title[i] <- content$parse$title
    df$content[i] <- gsubTags(content$parse$text$`*`)
    df$html_content[i] <- content$parse$text$`*`
    print( paste0(df$query[i], ' : ok!') )
    e <<- 'ok'
  }, error = function(e) {
    #print( conditionMessage(e) )
    e <<- e$message
    print( paste0(df$query[i], ' not found.') )
  })
  if( (e!='ok') & !grepl("The page you specified doesn't exist.", 
                         e)){
    print(e)
    break
  }
  Sys.sleep(runif(1,1,5))
}


# recrawl 
re_search_str <- '重定向至：'

indx=0
x = 1
while(length(indx)>0){
  if(all(indx==which(grepl(re_search_str, df$content)))){
    print(paste0('Maybe get stuck.'))
    break
  }
  indx <- which(grepl(re_search_str, df$content))
  other_title <- gsub(re_search_str, "", df$content[indx])
  other_title <- trimws( ifelse( grepl( "\n", other_title ), substr(other_title, 1, sapply(other_title, function(x) gregexpr("\n", x)[[1]][1])), other_title ) )
  
  df$wiki_title[indx] <- other_title
  
  for(i in indx){
    print(paste0(i, ' of ', nrow(df)))
    string <- df$wiki_title[i]
    tryCatch({
      content <- page_content(language = 'zh', project = "wikipedia", page_name = string)
      df$wiki_title[i] <- content$parse$title
      df$content[i] <- gsubTags(content$parse$text$`*`)
      df$html_content[i] <- content$parse$text$`*`
      print( paste0(df$wiki_title[i], ' : ok!') )
      e <<- 'ok'
    }, error = function(e) {
      #print( conditionMessage(e) )
      e <<- e$message
      print( paste0(df$wiki_title[i], ' not found.') )
    })
    if( (e!='ok') & !grepl("The page you specified doesn't exist.", 
                           e)){
      print(e)
      break
    }
    Sys.sleep(runif(1,1,5))
  }
  print(paste0('Round ', x, ' over.'))
}

df$wiki_title   <- converter(S2T)[df$wiki_title]
df$content      <- converter(S2T)[df$content]
df$html_content <- converter(S2T)[df$html_content]

df$wiki_title   <- trimws(df$wiki_title)
df$content      <- trimws(df$content)
df$html_content <- trimws(df$html_content)

write.csv(df, 'jobs_wiki_part2.csv', row.names = FALSE)
saveRDS(df, 'jobs_wiki_part2.rds')
