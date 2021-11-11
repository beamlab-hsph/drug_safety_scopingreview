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
    geom_pointrange() + 
    scale_color_manual(labels = c('Info Extraction', 'Deep Learning'),
                       values = c('black','blue')) +
    labs(x='Year',
         y='Topic Proportion',
         title='Change in topic prevalence over time') +
    theme_bw()
  return(p1)
}


dat <- readRDS('src/lda//beam_tm.rds')
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


### generate topic effect from year
eff <- estimateEffect(1:30 ~ year,tm_cov,meta=clean_covs$meta,
                      uncertainty='Global')
summary(eff)

eff_stemmed <- estimateEffect(1:30 ~ year,tm_cov_stemmed,meta=clean_covs_stemmed$meta,
                              uncertainty='Global')
summary(eff_stemmed)


### plot effects: topic prop over time
plot(eff,'year',method='continuous',topics=c(10,27),model=tm_cov)
plot(eff,'year',method='continuous',topics=c(2,9,19,24,29),model=tm_cov)
plot(eff_stemmed,'year',method='continuous',topics=c(10,27),model=tm_cov_stemmed)
plot(eff_stemmed,'year',method='continuous',topics=c(2,9,19,24,29),model=tm_cov_stemmed)

### to extract plot for ggplot
prop_plot <- ggpropovertime(eff,tm_cov,topics=c(10,27))

ggsave(filename='figures/deep_learning_prev.jpeg', plot=prop_plot)
