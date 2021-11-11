library(tidyverse)
library(stm)
library(tidytext)
library(LDAvis)
library(tm)

ggpropovertime <- function(eff_model,topic_model,topics=c(1,2),npts=100){
  dat_plot <- plot(eff_model,'year',method='continuous',topics=topics,model=topic_model,npoints=npts)
  names(dat_plot$means) <- paste0('T',dat_plot$topics)
  names(dat_plot$ci) <- paste0('T',dat_plot$topics)
  p1 <- as_tibble(do.call('rbind',lapply(dat_plot$ci,t))) %>% 
    rename('lower'=1,'upper'=2) %>%
    mutate(mean=unlist(dat_plot$means),
           topic=as.factor(rep(dat_plot$topics,each=npts)),
           x=rep(dat_plot$x,length(topics))) %>%
    ggplot(aes(x=x,y=mean,ymin=lower,ymax=upper,color=topic)) +
    geom_pointrange()
  print(p1)
}


### loop to generate and clean corpus
files <- file.path('D:/Dropbox/beam/Full_Texts',read_csv('D:/Dropbox/beam/text_titles.csv') %>% select('Text') %>% unlist())
start_words <- c('^abstract|^background|^introduction|abstract$|background$|introduction$')
end_words <- c('^references|^acknowledgements|references$|acknowledgements$')
months <- str_to_lower(month.name)
df <- data_frame()
j <- 1
for (f in files){
  print(j)
  txt <- read_lines(f)
  if (length(txt) < 2) next # skip file if near empty
  
  fn <- str_replace(f,'^D:/Dropbox/beam/Full_Texts/(.*)\\.txt$','\\1')

  i <- 1
  string <- ''
  yr <- NA
  while (i < length(txt)){
    l <- txt[i]
    ll <- str_to_lower(l)
    ll <- str_remove_all(ll,' ')
    
    # look for a published date in beginning of text
    if (is.na(yr)){yr <- str_extract(ll,'^[1-2][9|0][0-9][0-9]$')}
    if (is.na(yr)){
      if (any(str_detect(ll,months))){
        yr <- str_extract(ll,'[1-2][9|0][0-9][0-9]$')
      }
    }
    
    # look for start words (abstract, introduction, etc) to start extraction
    if (str_detect(ll,start_words)){
      i <- i+1
      for (line in txt[i:length(txt)]){
        if (is.na(line)) next
        line <- str_to_lower(line)
        if (str_detect(line,end_words)) break
        line <- str_replace_all(line, '[\r\n]f',' ')
        if (line == '') next
        if (str_detect(line,'issn')) next 
        if (str_detect(line,'$\\[[0-9]+\\]|$\\([0-9]+\\)|^[0-9]+\\.')) next # screen for refs 
        line <- str_remove_all(line,'\\[[0-9]+\\]|\\([0-9]+\\)')
        line <- str_replace_all(line,'[^[:alnum:]]',' ') # remove all non-alphanum
        line <- str_replace_all(line,'Bayesian confidence propagation neural network','BCPNN')
        string <- str_c(string,line)
      }
      break
    }
    i <- i+1
  }
  
  if (string != '') df <- bind_rows(df,data_frame('doc'=fn,'year'=yr,'text'=string))
  
  j <- j+1
}


### creating dataframes
df <- readRDS('D:/Dropbox/beam/beam_tm_df.rds') %>%
  left_join(read_csv('D:/Dropbox/beam/text_titles.csv') %>%
              select(doc=Text,year2=`Published Year`) %>%
              mutate(doc=str_remove(doc,'\\.txt$')),
            by='doc') %>%
  mutate(year=ifelse(!is.na(year2),year2,year)) %>%
  select(-year2)

# saveRDS(df,'D:/Dropbox/beam/beam_tm_df.rds')

df_covs <- df %>% 
  filter(!is.na(year)) %>% 
  mutate(year=as.integer(year)) %>%
  filter(year>2000,year<2022)

df_tidy <- df %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  filter(!(word %in% as.character(1:100))) 

tfidf <- df_tidy %>%
  count(doc, word, sort = TRUE)


### preprocess corpus
clean_tmp <- textProcessor(documents=df$text,metadata=df,
                       ucp=TRUE,wordLengths=c(2,20),
                       stem=FALSE)
clean_tmp_stemmed <- textProcessor(documents=df$text,metadata=df,
                           ucp=TRUE,wordLengths=c(2,20),
                           stem=TRUE)
clean_covs_tmp <- textProcessor(documents=df_covs$text,metadata=df_covs,
                                ucp=TRUE,wordLengths=c(2,20),
                                stem=FALSE)
clean_covs_tmp_stemmed <- textProcessor(documents=df_covs$text,metadata=df_covs,
                                ucp=TRUE,wordLengths=c(2,20),
                                stem=TRUE)

# plotRemoved(clean_tmp$documents,lower.thresh=seq(1,50,by=10))

clean <- prepDocuments(clean_tmp$documents,clean_tmp$vocab,
                       clean_tmp$meta,lower.thresh=10)

clean_covs <- prepDocuments(clean_covs_tmp$documents,clean_covs_tmp$vocab,
                            clean_covs_tmp$meta,lower.thresh=10)

clean_stemmed <- prepDocuments(clean_tmp_stemmed$documents,clean_tmp_stemmed$vocab,
                       clean_tmp_stemmed$meta,lower.thresh=10)

clean_covs_stemmed <- prepDocuments(clean_covs_tmp_stemmed$documents,clean_covs_tmp_stemmed$vocab,
                                    clean_covs_tmp_stemmed$meta,lower.thresh=10)


### search for optimal K
# kres <- searchK(clean$documents,clean$vocab,K=seq(5,30,5),data=clean$meta,heldout.seed=1123)
# saveRDS(kres,'D:/Dropbox/beam/beam_kres.rds')

kres <- readRDS('D:/Dropbox/beam/beam_kres.rds')
kres

### create topic models
set.seed(02138)
# corr topic model 15 topics
tm <- stm(documents=clean$documents,vocab=clean$vocab,
          K=15,verbose=TRUE,init.type='Spectral')

# corr topic model 30 topics
tm_many <- stm(documents=clean$documents,vocab=clean$vocab,
               K=30,verbose=TRUE,init.type='Spectral')

# corr topic model with 30 topics w/ topic props regressed on year
tm_cov <- stm(documents=clean_covs_stemmed$documents,vocab=clean_covs_stemmed$vocab,
              K=30,prevalence=~year,data=clean_covs_stemmed$meta,
              verbose=TRUE,init.type='Spectral')

# corr topic model with 30 topics w/ topic props regressed/spline on year
tm_cov_spline <- stm(documents=clean_covs_stemmed$documents,vocab=clean_covs_stemmed$vocab,
              K=30,prevalence=~s(year,df=3),data=clean_covs_stemmed$meta,
              verbose=TRUE,init.type='Spectral')

# corr topic model 15 topics
tm_stemmed <- stm(documents=clean_stemmed$documents,vocab=clean_stemmed$vocab,
          K=15,verbose=TRUE,init.type='Spectral')

# corr topic model 30 topics
tm_many_stemmed <- stm(documents=clean_stemmed$documents,vocab=clean_stemmed$vocab,
               K=30,verbose=TRUE,init.type='Spectral')

# corr topic model with 30 topics w/ topic props regressed on year
tm_cov_stemmed <- stm(documents=clean_covs$documents,vocab=clean_covs$vocab,
              K=30,prevalence=~year,data=clean_covs$meta,
              verbose=TRUE,init.type='Spectral')


# saveRDS(list(tm=tm,tm_cov=tm_cov,tm_many=tm_many,
#              tm_stemmed=tm_stemmed,tm_cov_stemmed=tm_cov_stemmed,
#              tm_many_stemmed=tm_many_stemmed,tm_cov_spline,
#              clean_stemmed=clean_stemmed,clean_covs_stemmed=clean_covs_stemmed,
#              clean=clean,clean_covs=clean_covs),'D:/Dropbox/beam/beam_tm.rds')


dat <- readRDS('D:/Dropbox/beam/beam_tm.rds')
tm <- dat$tm
tm_cov <- dat$tm_cov
tm_many <- dat$tm_many
tm_stemmed <- dat$tm_stemmed
tm_cov_stemmed <- dat$tm_cov_stemmed
tm_many_stemmed <- dat$tm_many_stemmed
tm_cov_spline <- dat$tm_cov_spline
clean <- dat$clean
clean_covs <- dat$clean_covs
clean_stemmed <- dat$clean_stemmed
clean_covs_stemmed <- dat$clean_covs_stemmed


### LDAvis
toLDAvis(tm,docs=clean$documents,out.dir='D:/Dropbox/beam/tm')
toLDAvis(tm_cov,docs=clean_covs$documents,out.dir='D:/Dropbox/beam/tm_cov')
toLDAvis(tm_many,docs=clean$documents,out.dir='D:/Dropbox/beam/tm_many')
toLDAvis(tm_stemmed,docs=clean_stemmed$documents,out.dir='D:/Dropbox/beam/tm_stemmed')
toLDAvis(tm_cov_stemmed,docs=clean_covs_stemmed$documents,out.dir='D:/Dropbox/beam/tm_cov_stemmed')
toLDAvis(tm_many_stemmed,docs=clean_stemmed$documents,out.dir='D:/Dropbox/beam/tm_many_stemmed')


### generate topic effect from year
eff <- estimateEffect(1:30 ~ year,tm_cov,meta=clean_covs$meta,
                      uncertainty='Global')
summary(eff)

eff_stemmed <- estimateEffect(1:30 ~ year,tm_cov_stemmed,meta=clean_covs_stemmed$meta,
                      uncertainty='Global')
summary(eff_stemmed)

eff_spline <- estimateEffect(1:30 ~ s(year,df=3),tm_cov_spline,meta=clean_covs$meta,
                              uncertainty='Global')
summary(eff_spline)


### plot effects: topic prop over time
plot(eff,'year',method='continuous',topics=c(10,27),model=tm_cov)
plot(eff,'year',method='continuous',topics=c(2,9,19,24,29),model=tm_cov)
plot(eff_stemmed,'year',method='continuous',topics=c(10,27),model=tm_cov_stemmed)
plot(eff_stemmed,'year',method='continuous',topics=c(2,9,19,24,29),model=tm_cov_stemmed)
plot(eff_spline,'year',method='continuous',topics=c(11),model=tm_cov_stemmed)

### to extract plot for ggplot
ggpropovertime(eff,tm_cov,topics=c(10,27))



### some nice summary data
plot(tm,type='summary',n=10)
plot(tm_cov,type='summary',n=10)
plot(tm_many,type='summary',n=10)
plot(tm_cov_spline,type='summary',n=10)

plot(tm_stemmed,type='summary',n=10)
plot(tm_cov_stemmed,type='summary',n=10)
plot(tm_many_stemmed,type='summary',n=10)


### topic correlations
tm_corr <- topicCorr(tm)
tm_cov_corr <- topicCorr(tm_cov)
tm_cov_spline_corr <- topicCorr(tm_cov_spline)
tm_many_corr <- topicCorr(tm_many)
tm_corr_stemmed <- topicCorr(tm_stemmed)
tm_cov_corr_stemmed <- topicCorr(tm_cov_stemmed)
tm_many_corr_stemmed <- topicCorr(tm_many_stemmed)


### plot topic correlations
plot(tm_corr)
plot(tm_cov_corr)
plot(tm_many_corr)
plot(tm_cov_spline_corr)
plot(tm_corr_stemmed)
plot(tm_cov_corr_stemmed)
plot(tm_many_corr_stemmed)
