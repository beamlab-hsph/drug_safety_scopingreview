library(tidyverse)
library(ggthemes)
library(cowplot)

data <- read_csv('data/review_data.csv')
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

# Bar plot of dataset type
dataset_summary<- 
  data %>%
      group_by(Dataset) %>%
      summarize(Count=n()) %>%
      ggplot(aes(x=reorder(Dataset,(-Count)), y=Count)) +
      geom_bar(stat='identity') + 
      labs(title='Summary of data sets used in included studies', 
            x='Dataset',
            y='Number of studies') +
      plot_theme +
      theme(text = element_text(size = 20), 
            axis.text.x = element_text(
              angle = 45,
              hjust = 1,
              vjust = 0.9
      ))

ggsave(filename='figures/dataset_summary.jpeg', plot=dataset_summary)

algorithm_summary <- 
  data %>%
  group_by(`Primary Algorithm`) %>%
  summarize(Count=n()) %>%
  ggplot(aes(x=reorder(`Primary Algorithm`,(-Count)), y=Count)) +
  geom_bar(stat='identity') + 
  labs(title='Summary of models used in included studies', 
       x='Model',
       y='Number of studies') +
  plot_theme +
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          vjust = 0.9
        ))

ggsave(filename='figures/algorithm_summary.jpeg', plot=algorithm_summary)

data_aglorithm <- plot_grid(dataset_summary, algorithm_summary, labels = c('A', 'B'), label_size = 20)
ggsave(filename='figures/data_algorithm.jpeg', plot=data_aglorithm, width=16, height=9)
