
library(tidyverse)
DataDictionary <- read_csv("../Pre_Cleaned_Dataset/DataDictionary.csv")
load("../Pre_Cleaned_Dataset/DRK.RData")

library(quanteda)
library(ldatuning)
DE_csw <- scan("DE_sw.txt", what = "character", sep = "\n")
DRK_job_desp_dfm <- tokens(d$jobdescription,
                           remove_punct = T,
                           remove_symbols = T,
                           remove_numbers = T) %>%
  tokens_select(pattern = DE_csw, selection = 'remove') %>%
  dfm()
dfm_topmod <- convert(DRK_job_desp_dfm, to = "topicmodels")
results30 <- FindTopicsNumber(dfm_topmod, topics = seq(from = 2, to = 30, by = 2),
                              metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                              method = "Gibbs",
                              control = list(seed = 666),
                              mc.cores = 7L, verbose = T)
FindTopicsNumber_plot(results30)

d_meta_data <- d %>% select(j_val_from_to_diff, title_cluster, tags_n_token,
                            subject.employmentTypes, subject.locale)
library(stm)
corpus_processed <- textProcessor(documents = d$jobdescription,
                                  metadata = d_meta_data,
                                  language = "de",
                                  onlycharacter = T,
                                  stem = F,
                                  removestopwords = T,
                                  customstopwords = DE_csw,
                                  removenumbers = T,
                                  removepunctuation = T) # trimming is strongly adviced
                                                         # the trimming process is subjective and requires both inductive & deductive reasoning
                                                         # with scholarly justifications
                                                         # possible additional arguments to trim the dfm
                                                         # lower.thresh = ...
                                                         # sparselevel = 
                                                         # adding more words (with high frequency) to the customstopwords list, which will be removed
                                                         # with common German stop-words
                                                         # in this case words like drk, mwd, rote, kreisverband, rotkreuz, kreuz etc.
                                                         # see the following

# drk                         e.v                      suchen                      bieten                    aufgaben 
# 2696                        2645                        2047                        1788                        1711 
# unterstützung                      freuen                   bewerbung                     unseres            drk-kreisverband 
# 1593                        1536                        1455                        1453                        1428 
# teams                       kreuz                kreisverband                        team                  ausbildung 
# 1394                        1317                        1255                        1210                        1154 
# teilzeit                       roten                      profil                   vergütung                      sofort 
# 1145                        1113                        1080                        1039                        1038 
# arbeit                bereitschaft                angesprochen                      fühlen                     bereich 
# 850                         849                         823                         822                         820 
# vollzeit              abgeschlossene                       ggmbh                     kreuzes                      pflege 
# 789                         775                         769                         766                         764 
# eignung                    arbeiten                 unbefristet                betriebliche                      umgang 
# 756                         750                         745                         716                         684 
# grundsätzen                       hilfe                    bewerber            schwerbehinderte                    gleicher 
# 680                         663                         656                         652                         652 
# berücksichtigt                 mitarbeiter                   betreuung              altersvorsorge               teamfähigkeit 
# 650                         643                         624                         622                         613 
# nächstmöglichen                   zeitpunkt                durchführung              zusammenarbeit                     unseren 
# 610                         602                         593                         583                         568 
# fort-                       gerne                     soziale                      kinder                       rotes 
# 565                         550                         520                         520                         497 
# klasse                        rote                      fragen                     kindern                   deutsches 
# 491                         489                         486                         484                         480 
# innen                       bitte                   tätigkeit                     hamburg                    erzieher 
# 477                         476                         466 

                                  
corpus_prep_docs_output <- prepDocuments(corpus_processed$documents,
                                         corpus_processed$vocab,
                                         corpus_processed$meta)
corpus_docs <- corpus_prep_docs_output$documents
corpus_vocab <- corpus_prep_docs_output$vocab
corpus_meta <- corpus_prep_docs_output$meta

K<-c(4, 10, 14, 15, 16, 20, 30)
kresults <- searchK(corpus_docs, corpus_vocab, K,
                    prevalence =~ j_val_from_to_diff + title_cluster + tags_n_token + subject.employmentTypes + subject.locale,
                    data = corpus_meta, seed = 666)
kresults_df <- data.frame(matrix(unlist(kresults$results), nrow = 7, byrow = F))
colnames(kresults_df) <- c("K","exclus","semcoh","heldout","residual","bound","lbound","em.its")
sc_lp <- ggplot(data=kresults_df, aes(x=K, y=semcoh)) + geom_line() + geom_point() +
  labs(x = "Number of Topics", y = "Semantic Coherence",
       #title = "Diagnostic Values by Number of Topics",
       subtitle = "Semantic Coherence") + theme_bw()
re_lp <- ggplot(data=kresults_df, aes(x=K, y=residual)) + geom_line() + geom_point() +
  labs(x = "Number of Topics", y = "Residuals",
       #title = "Diagnostic Values by Number of Topics",
       subtitle = "Residuals") + theme_bw()
gridExtra::grid.arrange(sc_lp,re_lp, ncol = 2, top = "Diagnostic Values by Number of Topics")

stm_results_14_topics <- stm(corpus_docs,
                             corpus_vocab,
                             K = 14,
                             prevalence =~ j_val_from_to_diff + title_cluster + tags_n_token + subject.employmentTypes + subject.locale,
                             data = corpus_meta, init.type = "LDA", seed = 666)

labelTopics(stm_results_14_topics)

library(tidytext)

td_beta <- tidy(stm_results_14_topics)
td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Structural Topic Modeling | 14 topics solution",
       subtitle = "Highest word probabilities for each topic") + theme_bw()

stm_similarity <- as.data.frame(stm_results_14_topics[["beta"]][["logbeta"]][[1]]) %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "ward.D2")
plot(stm_similarity,
     main = "STM topic similarity by features",
     xlab = "",
     sub = "")

theta <- stm_results_14_topics$theta
thetas <- data.frame(topic01 = theta[,1],
                     topic02 = theta[,2],
                     topic03 = theta[,3],
                     topic04 = theta[,4],
                     topic05 = theta[,5],
                     topic06 = theta[,6],
                     topic07 = theta[,7],
                     topic08 = theta[,8],
                     topic09 = theta[,9],
                     topic10 = theta[,10],
                     topic11 = theta[,11],
                     topic12 = theta[,12],
                     topic13 = theta[,13],
                     topic14 = theta[,14])

thetas_selected <- thetas %>%  mutate(topic01_cluster = topic01,
                                      topic10_cluster = topic10,
                                      topic_cluster3 = topic14 + topic07 + topic09 + topic04 + topic12,
                                      topic_cluster4 = topic05 + topic08,
                                      topic_cluster5 = topic02 + topic06 + topic11 + topic03 + topic13) %>%
  select(topic01_cluster, topic10_cluster, topic_cluster3, topic_cluster4, topic_cluster5)


d_select <- d %>% select(Unique_Page_Views, Goal_Completions, j_val_from_to_diff,
                         subject.employmentTypes)

df <- bind_cols(d_select, thetas_selected)

labels <- c("UPW", "T1C", "T10C", "TC3", "TC4", "TC5")

library(bootnet)
ggm_nw <- estimateNetwork(df[,c(1,5:9)], default = "EBICglasso", tuning = 0.5, missing = "pairwise", threshold = T, lambda.min.ratio = 0.1)
plot(ggm_nw, layout = "spring", maximum = 1, edge.labels = T,
     edge.label.cex = 1, vTrans = 200, labels = labels,
     title = "Associations between estimated topics & unique page views")
