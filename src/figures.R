library(tidyverse)
library(ggthemes)
library(ggridges)
library(cowplot)

data <- read_csv('data/review_data_cleaned.csv')
titles <- read_csv('data/text_titles.csv') %>% select(Title, Authors, `Published Year`, `Published Month`, `Covidence #`)
titles$`Covidence #` <- as.numeric(str_remove(titles$`Covidence #`, "#"))
data <- data %>% inner_join(titles, by="Covidence #")
plot_theme <- theme_light()


data <- data %>%
          mutate(Dataset = replace(Dataset, str_detect(Dataset, "Other"), "Other")) %>%
          mutate(Dataset = replace(Dataset, str_detect(Dataset, "FAERS"), "FAERS")) %>%
          mutate(Dataset = replace(Dataset, str_detect(Dataset, "JAERS"), "JAERS")) %>%
          mutate(Dataset = replace(Dataset, str_detect(Dataset, "KAERS"), "KAERS")) %>%
          mutate(Dataset = replace(Dataset, str_detect(Dataset, "Literature"), "Literature")) %>%
          mutate(Dataset = replace(Dataset, str_detect(Dataset, "VAERS"), "VAERS")) 

data <- data %>%
  mutate(`Primary Algorithm` = replace(`Primary Algorithm`, str_detect(`Primary Algorithm`, "Other"), "Other")) %>%
  mutate(`Primary Algorithm` = replace(`Primary Algorithm`, str_detect(`Primary Algorithm`, "Confi"), "BCPNN"))

data <- 
  data %>%
  mutate(`Task Type` = replace(`Task Type`, str_detect(`Task Type`, "Other"), "Other"))

# Bar plot of dataset type
dataset_summary<- 
  data %>%
      group_by(Cleaned_Dataset) %>%
      summarize(Percentage=round(n()/nrow(data)*100)) %>%
      ggplot(aes(x=reorder(Cleaned_Dataset,(-Percentage)), y=Percentage)) +
      geom_bar(stat='identity') + 
      labs(title='Dataset used', 
            x='Dataset',
            y='Percentage of studies') +
      plot_theme +
      theme(text = element_text(size = 15), 
            axis.text.x = element_text(
              angle = 45,
              hjust = 1,
              vjust = 0.9
      ))

ggsave(filename='figures/dataset_summary.jpeg', plot=dataset_summary)

algorithm_summary <- 
  data %>%
  group_by(`Primary Algorithm`) %>%
  summarize(Percentage=round(n()/nrow(data)*100)) %>%
  ggplot(aes(x=reorder(`Primary Algorithm`,(-Percentage)), y=Percentage)) +
  geom_bar(stat='identity') + 
  labs(title='Models used', 
       x='Model',
       y='Percentage of studies') +
  plot_theme +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  theme(text = element_text(size = 15), 
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          vjust = 0.9
        ))

ggsave(filename='figures/algorithm_summary.jpeg', plot=algorithm_summary)

task <-
  data %>%
  group_by(`PV Task`) %>%
  summarize(Percentage=round(n()/nrow(data)*100)) %>%
  ggplot(aes(x=reorder(`PV Task`,(-Percentage)), y=Percentage)) +
  geom_bar(stat='identity') + 
  labs(title='Task Summary', 
       x='Model',
       y='Percentage of studies') +
  plot_theme +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  theme(text = element_text(size = 15), 
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          vjust = 0.9
        ))

ggsave(filename='figures/task.jpeg', plot=task)

data_aglorithm <- plot_grid(
                    plot_grid(dataset_summary, algorithm_summary, nrow=2, labels=c('A','B'), label_size=15),
                  task, label_size = 15, labels=c("","C"))
ggsave(filename='figures/data_algorithm.jpeg', plot=data_aglorithm, width=16, height=9)

papers_time <- 
  data %>%
  group_by(`Published Year`) %>%
  summarize(Count = n()) %>%
  filter(`Published Year` <= 2020) %>%
    ggplot(aes(x=`Published Year`, y=Count)) +
    geom_point() +
    geom_line() + 
    plot_theme +
    labs(title='Number PV/ML Publications by Year', 
         x='Year',
         y='Number of studies') +
   theme(text = element_text(size = 15))

task_time <- 
  data %>%
  group_by(`Published Year`, `PV Task`) %>%
  summarize(Count = n()) %>%
  filter(`Published Year` <= 2020) %>%
    ggplot(aes(x=`Published Year`, y=Count, color=`PV Task`)) +
    geom_point() +
    geom_line() + 
    plot_theme +
    labs(title='Task Type by Year', 
       x='Year',
       y='Number of studies') +
    theme(text = element_text(size = 15), legend.position = "bottom")
  
deep_methods <- c('Transformer', 'LSTM / RNN', 'Deep learning (other)')
pv_methods <- c('ROR', 'BCPNN', 'Empirical Bayes', 'Gamma-Poisson Shrinker')
models_to_plot <- c('Traditional PV Methods', 'Deep Learning', 'Decision tree methods', 'SVM', 'Logistic Regression')
models_time <-
  data %>%
  mutate(`Primary Model` = ifelse(`Primary Algorithm` %in% deep_methods, 
                                  "Deep Learning", 
                                  ifelse(`Primary Algorithm` %in% pv_methods, "Traditional PV Methods", `Primary Algorithm`))) %>%
  group_by(`Published Year`, `Primary Model`) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  filter(`Primary Model` %in% models_to_plot, `Published Year` <= 2020) %>%
    ggplot(aes(x=`Published Year`, y=Count, color=`Primary Model`)) +
    geom_point() +
    geom_line() + 
    labs(title='Model Usage by Year', 
         x='Year',
         y='Number of studies') +
    plot_theme +
    theme(text = element_text(size = 15),
          legend.position = "bottom")

temporal_plots <- plot_grid(papers_time, task_time, models_time, nrow=3, labels=c('A','B', 'C'), label_size=15)
ggsave(filename='figures/temporal plot.jpeg', plot=temporal_plots, width=12, height=12)


# Sub analysis
pipeline_plot <-
  data %>%
  filter(`PV Task` == "Data ingestion") %>%
  group_by(`Primary Algorithm`) %>%
  summarize(Percentage= round(100*n()/length(which(data$`PV Task`=="Data ingestion")))) %>%
  ggplot(aes(x=reorder(`Primary Algorithm`,(-Percentage)), y=Percentage)) +
  geom_bar(stat='identity') +
  labs(title='Most popular models for data intake and processing', 
       x='Model',
       y='Percentage of studies') +
  plot_theme +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  theme(text = element_text(size = 15), 
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          vjust = 0.9
        ))

pipeline_pretraining <-
  data %>%
  filter(`PV Task` == "Data ingestion") %>%
  group_by(Training) %>%
  summarize(Percentage= round(100*n()/length(which(data$`PV Task`=="Data ingestion")))) %>%
  ggplot(aes(x=reorder(Training,(-Percentage)), y=Percentage)) +
  geom_bar(stat='identity') +
  labs(title='Use of Transfer Learning in Data Intake Studies', 
       x='Used Pretraining?',
       y='Percentage of studies') +
  plot_theme +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  theme(text = element_text(size = 15), 
        axis.text.x = element_text(
          angle = 10,
          hjust = 1,
          vjust = 0.9
        ))

pipeline_novelty <-
  data %>%
  filter(`PV Task` == "Data ingestion") %>%
  group_by(`Method Novelty`) %>%
  summarize(Percentage= round(100*n()/length(which(data$`PV Task`=="Data ingestion")))) %>%
  ggplot(aes(x=reorder(`Method Novelty`,(-Percentage)), y=Percentage)) +
  geom_bar(stat='identity') +
  labs(title='Methodological Novelty in Data Intake Studies', 
       x='Method Novelty',
       y='Percentage of studies') +
  plot_theme +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  theme(text = element_text(size = 15), 
        axis.text.x = element_text(
          angle = 10,
          hjust = 1,
          vjust = 0.9
        ))

pipeline <- plot_grid(
  plot_grid(pipeline_pretraining, pipeline_novelty, nrow=2, labels=c('A','B'), label_size=15),
  pipeline_plot, label_size = 15, labels=c("","C"))

ggsave(filename='figures/pipeline.jpg', plot=pipeline, width=16, height=9)

sample_size_data <- 
  data %>%
  filter(Dataset %in% datasets_to_plot) %>%
  filter(`Sample Size Explicit` == "Yes") 
sample_size_data$`Sample Size` <- as.numeric(sample_size_data$`Sample Size`)

pipeline_sample_size <-
  sample_size_data %>%
  filter(`PV Task` == "Data ingestion") %>%
  group_by(Dataset) %>%
  summarize(
    n = n(),
    mean = mean(`Sample Size`),
    median = median(`Sample Size`),
    sd = sd(`Sample Size`),
    iqr = IQR(`Sample Size`)
  )

write_csv(pipeline_sample_size, path='figures/pipeline_sample_size.csv')


datasets_to_plot <- c('Other', 'FAERS', 'KAERS', 'EHR data', 'Twitter', 'VAERS')

sample_size_summary <-
  sample_size_data %>%
  group_by(Dataset) %>%
    summarize(
      n = n(),
      mean = mean(`Sample Size`),
      median = median(`Sample Size`),
      sd = sd(`Sample Size`),
      iqr = IQR(`Sample Size`)
    )

write_csv(sample_size_summary, path='figures/sample_size.csv')

sample_size_plot <-
  sample_size_data %>%
  ggplot(aes(y = Dataset, x = `Sample Size`)) + 
  geom_density_ridges() +
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000, 100000, 1e6, 10e6)) +
  plot_theme +
  labs(title='Sample Size by Dataset', 
       x='Sample Size (log scale)',
       y='Dataset') +
  plot_theme +
  theme(text = element_text(size = 15),
        legend.position = "bottom")

ggsave(filename='figures/sample_size_plot.jpeg', plot=sample_size_plot, width=12, height=12)
  

# Number of studies using traditional PV methods
x = data %>% filter(`Primary Algorithm` == 'ROR' | `Primary Algorithm` == 'BCPNN' | `Primary Algorithm` == 'Empirical Bayes' | `Primary Algorithm` == 'Gamma-Poisson Shrinker')
nrow(x) / nrow(data)

# Method novelty 
table(data$`Method Novelty`)/nrow(data)

table(data$Training)/nrow(data)

table(data$`External information incorporated?`)/nrow(data)

table(data$`Data Available`)/nrow(data)
table(data$`Code Available`)/nrow(data)


table(data$`This paper belongs on the short list of papers to re-examine`)/nrow(data)
pipe_line_papers <- data %>% filter(`PV Task` == "Data ingestion")
table(pipe_line_papers$`This paper belongs on the short list of papers to re-examine`)/nrow(pipe_line_papers)

pipeline_novelty_tab <-
  data %>%
  filter(`PV Task` == "Data ingestion") %>%
  group_by(`Method Novelty`) %>%
  summarize(Count=n(), Total=length(which(data$`PV Task`=="Data ingestion"))) %>%
  filter(`Method Novelty` == "Authors made novel changes") %>%
  select(Count, Total)

novelty_tab <-
  data %>%
  group_by(`Method Novelty`) %>%
  summarize(Count=n(), Total=nrow(data)) %>%
  filter(`Method Novelty` == "Authors made novel changes") %>%
  select(Count, Total)

prop.test(x=c(pipeline_novelty_tab$Count, novelty_tab$Count), n=c(pipeline_novelty_tab$Total, novelty_tab$Total))



pipeline_pretrain_tab <-
  data %>%
  filter(`PV Task` == "Data ingestion") %>%
  group_by(`Training`) %>%
  summarize(Count=n(), Total=length(which(data$`PV Task`=="Data ingestion"))) %>%
  filter(`Training` == "Transfer learning") %>%
  select(Count, Total)

pretrain_tab <-
  data %>%
  group_by(`Training`) %>%
  summarize(Count=n(), Total=nrow(data)) %>%
  filter(`Training` == "Transfer learning") %>%
  select(Count, Total)

prop.test(x=c(pipeline_pretrain_tab$Count, pretrain_tab$Count), n=c(pipeline_pretrain_tab$Total, pretrain_tab$Total))
