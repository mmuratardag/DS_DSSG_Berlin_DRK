
# library(feather)
# path <- "../Pre_Cleaned_Dataset/DRK_Jobs_basic_merged.feather"
# df <- read_feather(path)
# df <- vroom::vroom("../Pre_Cleaned_Dataset/DRK_Jobs_basic_merged.parquet.gzip")

library(tidyverse)

DataDictionary <- read_csv("../Pre_Cleaned_Dataset/DataDictionary.csv")
df <- read_csv("../Pre_Cleaned_Dataset/DRK.csv")

str(df)
glimpse(df)

n_distinct(df$subject.sector.meta.name.value) # 1
head(df$subject.tags)

n_distinct(df$subject.employer.meta.name.value) # 234

round(prop.table(table(df$subject.employmentTypes)), 3)
# Ausbildung          Befristet       Ehrenamtlich       Freiberufler Freiwilligendienst          Praktikum           Sonstige           Teilzeit 
# 0.012                 0.004              0.004              0.003              0.012              0.001              0.050              0.224 
# Vollzeit 
# 0.692 

table(df$subject.location.countryCode)

df_tags <- df %>% separate(subject.tags, c("t1", "t2", "t3", "t4", "t5",
                                           "t6", "t7", "t8", "t9", "t10",
                                           "t11", "t12", "t13", "t14", "t15")) %>%
  select(t1:t15)

library(naniar)
df_tags %>% vis_miss() + labs(title = "Missing %s of the Job Description Tags")

library(qdapTools)
tags_ohc <- mtabulate(strsplit(as.character(df$subject.tags), ","))
# dim 1807 x 2750
# colnames(tags_ohc)

d <- bind_cols(df, df_tags)

colnames(d)

rm(df, df_tags, tags_ohc)

skimr::skim(d[,23:25])

melted_cols <- reshape2::melt(d [, c(23:25)])
melted_cols %>% ggplot(aes(x = value)) + 
  geom_histogram(bins = 100) + 
  facet_wrap(~ variable) + theme_bw()

library(lubridate)
glimpse(d)
class(d$subject.createdOn)
class(d$subject.modifiedOn)
class(d$subject.validFrom)
class(d$subject.validTo)

min(d$subject.createdOn); max(d$subject.createdOn)
min(d$subject.modifiedOn); max(d$subject.modifiedOn)
min(d$subject.validFrom); max(d$subject.validFrom) # error here; there is 1970
min(d$subject.validTo); max(d$subject.validTo)

d$subject.createdOn <- ymd_hms(d$subject.createdOn)
d$subject.modifiedOn <- ymd_hms(d$subject.modifiedOn)
d$subject.validFrom <- ymd_hms(d$subject.validFrom)
d$subject.validTo <- ymd_hms(d$subject.validTo)

d %>% mutate(validFrom = year(subject.validFrom),
             validTo = year(subject.validTo)) %>%
  select(validFrom:validTo) %>% filter(validFrom < 2020)
# there are 2 1970s; a few 2016s, 2019s

d <- d %>% mutate(j_val_from_to_diff = (subject.validFrom %--% subject.validTo)/ddays(1))

ggplot(d, aes(d$j_val_from_to_diff)) +
  geom_histogram(bins = 100) + theme_bw() +
  labs(title = "# of days the job posting remains on the website",
       caption = "a few errors in the data set with dates; some 1970s
       \nmight be a parsing error after my file format conversion",
       x = " ")

psych::describe(d$j_val_from_to_diff)

n_distinct(d$subject.location.countryCode) 
unique(d$subject.location.countryCode)
n_distinct(d$subject.location.city)

DE_csw <- scan("DE_sw.txt",what = "character", sep = "\n")

library(quanteda)

DRK_job_desp_dfm <- tokens(d$jobdescription,
                           remove_punct = T,
                           remove_symbols = T,
                           remove_numbers = T) %>%
  tokens_select(pattern = DE_csw, selection = 'remove') %>%
  dfm()
topfeatures(DRK_job_desp_dfm, 500)

DRK_job_titl_dfm <- tokens(d$subject.title,
                           remove_punct = T,
                           remove_symbols = T,
                           remove_numbers = T) %>%
  tokens_select(pattern = DE_csw, selection = 'remove') %>%
  dfm()
topfeatures(DRK_job_titl_dfm, 500)

DRK_job_tags_dfm <- tokens(d$subject.tags,
                           remove_punct = T,
                           remove_symbols = T,
                           remove_numbers = T) %>%
  tokens_select(pattern = DE_csw, selection = 'remove') %>%
  dfm()
topfeatures(DRK_job_tags_dfm, 500)

DRK_sub_titl_dfm <- tokens(d$subject.title,
                           remove_punct = T,
                           remove_symbols = T,
                           remove_numbers = T) %>%
  tokens_select(pattern = DE_csw, selection = 'remove') %>%
  dfm()
topfeatures(DRK_sub_titl_dfm, 500)

tstat_dist <- as.dist(quanteda.textstats::textstat_dist(DRK_sub_titl_dfm))
clust <- hclust(tstat_dist)
plot(clust)
clusters_h4 <- cutree(clust, h = 4)
table(clusters_h4)
clusters <- tibble(clusters_h4)
clusters$clusters_h4 <- ifelse(clusters$clusters_h4 == 1, 1, 2)
round(prop.table(table(clusters$clusters_h4)),2) 

d$title_cluster <- clusters$clusters_h4

colnames(d)

d$jobdescription_n_token <- ntoken(d$jobdescription)
ggplot(d, aes(jobdescription_n_token)) + geom_histogram(bins = 100) + theme_bw() +
  labs(title = "# of tokens in the Job Description")

colnames(d)

d$tags_n_token <- ntoken(d$subject.tags)

d %>% ggplot(aes(subject.employmentTypes)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..), label = ifelse((..count..)==0,"",
                                                scales::percent((..count..)/sum(..count..)))),
            stat="count",colour="black") + 
  labs(title = "Emplotment Types",
       x = " ") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

unique(d$subject.employmentTypes)

d$emp_type <- fct_collapse(d$subject.employmentTypes,
                           FullTime = c("Vollzeit"),
                           Other = c("Teilzeit", "Sonstige", "Ausbildung",
                                     "Freiwilligendienst", "Praktikum", 
                                     "Befristet", "Freiberufler", "Ehrenamtlich"))

d %>% ggplot(aes(emp_type)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..), label = ifelse((..count..)==0,"",
                                                scales::percent((..count..)/sum(..count..)))),
            stat="count",colour="black") + 
  labs(title = "Emplotment Types (collapsed categories)",
       x = " ") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

d %>% select(Unique_Page_Views, Goal_Completions,
             subject.employmentTypes) %>%
  group_by(subject.employmentTypes) %>%
  summarise(median_Unique_Page_Views = median(Unique_Page_Views),
            median_Goal_Completions = median(Goal_Completions)) %>%
  knitr::kable()

d %>% select(Unique_Page_Views, Goal_Completions,
             emp_type) %>%
  group_by(emp_type) %>%
  summarise(median_Unique_Page_Views = median(Unique_Page_Views),
            median_Goal_Completions = median(Goal_Completions)) %>%
  knitr::kable()

d %>% select(Unique_Page_Views, Goal_Completions,
             emp_type) %>%
  group_by(emp_type) %>%
  summarise(mean_Unique_Page_Views = mean(Unique_Page_Views),
            mean_Goal_Completions = mean(Goal_Completions)) %>%
  knitr::kable()

d %>% ggplot(aes(x=Unique_Page_Views, fill=emp_type)) +
  geom_density(alpha=0.6) + 
  labs(x = " ",
       title = "Unique Page Views ",
       subtitle = "by Employment Type") + theme_bw()

d %>% ggplot(aes(x=Goal_Completions, fill=emp_type)) +
  geom_density(alpha=0.6) + 
  labs(x = " ",
       title = "Goal Completions",
       subtitle = "by Employment Type") + theme_bw()

library(easystats)

d %>% select(Unique_Page_Views, Goal_Completions, j_val_from_to_diff,
             subject.employmentTypes) %>%
  group_by(subject.employmentTypes) %>%
  correlation(method = "spearman") %>%
  knitr::kable(digits = 3)

d %>% select(Unique_Page_Views, Goal_Completions,
             emp_type) %>%
  group_by(emp_type) %>%
  correlation(method = "spearman") %>%
  knitr::kable(digits = 3)

save(d, file = "../Pre_Cleaned_Dataset/DRK.RData")

