rm(list = ls(all = TRUE))

library(rjson) #manejo de json files
library(Rfacebook) #scraping de facebook
library(reshape2) #manipulaciÃ³n de data frames
library(dplyr) #manipulaciÃ³n de data frames
library(tidytext) #text mining que utiliza 'dplyr'
library(ggplot2) #gráficos
library(grid) #mas de un gráfico por marco
library(tm) #text mining
library(wordcloud) #nube de palabras
library(tidyr) #funciones especiales para tidy
library(igraph) #network analysis
library(ggraph) #grafica redes
library(topicmodels) #topic models


###########################################################################
##ANALISIS
###########################################################################

##GENERA UN CORPUS CON LOS DATOS

##TRABAJA CON EL FORMATO TIDY

tidy_fboi<-dtm.tidy %>%
    count(key_0, term, sort = TRUE) %>%
    ungroup()

colnames(tidy_fboi)[1] <- "document"

##TOTALES POR DOCUMENTO
total_wordsbydoc <- tidy_fboi %>% 
  group_by(document) %>% 
  summarize(total = sum(n))

fboi_words <- left_join(tidy_fboi, total_wordsbydoc)

##TOTALES POR PALABRA
total_words <- tidy_fboi %>% 
  group_by(term) %>% 
  summarize(total = sum(n))

# ##CARGA LAS STOPWORDS EN PORTUGUES
# pt_stopwords <- read.table("C:/Users/OB186007/Documents/Clientes/Oi/pt_stopwords.txt",
#                            stringsAsFactors = FALSE)
# colnames(pt_stopwords)<-'term'
# 
# ##REMUEVE LAS STOPWORDS
# total_words2 <- total_words %>%
#   anti_join(pt_stopwords)

###########################################################################
##GRAFICO DE PALABRAS MAS COMUNES
total_words %>%
  filter(total >150) %>%
  mutate(term = reorder(term, total)) %>%
  ggplot(aes(term, total)) +
  geom_col() +
  xlab(NULL) + 
  ggtitle("Palavras mais comuns") +
  coord_flip()

##WORDCLOUD DE LAS PALABRAS MAS COMUNES

wordcloud(words = total_words$term, freq = total_words$total, min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


###########################################################################
##PALABRAS QUE DEFINEN A CADA DOCUMENTO (TF-IDF)

total_words <- tidy_fboi %>% 
  group_by(document) %>% 
  summarize(total = sum(n))

fboi_words <- left_join(fboi_words, total_words)

fboi_words2 <- fboi_words %>%
  bind_tf_idf(term, document, n)

##LAS PALABRAS MAS COMUNES CON TF-IDF MUY BAJO
fboi_words2

##LAS PALABRAS MENOS COMUNES CON TF-IDF MUY ALTO
fboi_words2 %>%
  select(-total) %>%
  arrange(desc(tf_idf))

##GRAFICO

plot_fboi <- fboi_words2 %>%
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term))))

plot_fboi %>% 
  top_n(10) %>%
  ggplot(aes(term, tf_idf, fill = document)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  ggtitle("Palavras distintivas") +
  coord_flip()

###########################################################################
##BIGRAMS

fboi_bigrams <- tidy_fboi %>%
  unnest_tokens(bigram, term, token = "ngrams", n = 2)

bigrams_separated <- fboi_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# ##MANIPULA PARA ELIMINAR STOPWORDS
# 
# ##elimina stopwords
# bigrams_filtered <- bigrams_separated %>%
#   filter(!word1 %in% pt_stopwords$term) %>%
#   filter(!word2 %in% pt_stopwords$term)

bigram_counts <- bigrams_separated %>% 
  count(word1, word2, sort = TRUE)

#bigram_counts

##vuelve a unir en una única sentencia
bigrams_united <- bigrams_separated %>%
  unite(bigram, word1, word2, sep = " ")

#bigrams_united

##GRAFICA LOS BIGRAMS MAS COMUNES

##filtra las combinaciones más comunes
bigram_graph <- bigram_counts %>%
  filter(nn >= 10) %>%
  graph_from_data_frame()

#bigram_graph

##GRAFICACION MEDIANTE EL PAQUETE ggraph

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle("Pares de palavras que aparecem nas chamadas")

###########################################################################
##TRIGRAMS

fboi_trigrams <- tidy_fboi %>%
  unnest_tokens(trigram, term, token = "ngrams", n = 3)

trigrams_separated <- fboi_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

# ##MANIPULA PARA ELIMINAR STOPWORDS
# 
# ##elimina stopwords
# trigrams_filtered <- trigrams_separated %>%
#   filter(!word1 %in% pt_stopwords$term) %>%
#   filter(!word2 %in% pt_stopwords$term) %>%
#   filter(!word3 %in% pt_stopwords$term)

trigram_counts <- trigrams_separated %>% 
  count(word1, word2, word3, sort = TRUE)

#trigram_counts

##vuelve a unir en una única sentencia
trigrams_united <- trigrams_separated %>%
  unite(trigram, word1, word2, word3, sep = " ")

#trigrams_united

##GRAFICA LOS TRIGRAMS MAS COMUNES

##filtra las combinaciones más comunes
trigram_graph <- trigram_counts %>%
  filter(nn >= 3) %>%
  graph_from_data_frame()

#trigram_graph

##GRAFICACION MEDIANTE EL PAQUETE ggraph

set.seed(1999)

ggraph(trigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle("Tríades de palavras que aparecem nas chamadas")

###########################################################################
##LDA
rowTotals <- apply(dtm , 1, sum) #encuentra la suma de palabras de cada documento
dtm.new   <- dtm[rowTotals> 0, ]           #remueve todos los documentos sin palabras

ap_lda <- LDA(dtm.new, k = 12, control = list(seed = 1234))

#ap_lda

##EXTRAE LAS PROBABILIDADES POR TOPICO Y POR PALABRA
ap_topics <- tidy(ap_lda, matrix = "beta")

#ap_topics

##TOP 10 TERMINOS Y GRAFICO
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  ggtitle("Palavras mais repetidas em cada cluster") +
  coord_flip()

##PALABRAS CON MAYOR DIFERENCIA Y GRAFICO
beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

#beta_spread

# top_topic<-rbind(beta_spread %>% top_n(-15),beta_spread %>% top_n(15))
# 
# ggplot(top_topic, aes(reorder(term,log_ratio),log_ratio )) + 
#   geom_col(show.legend = FALSE) +
#   coord_flip() +  
#   ggtitle("Palavras mais importantes em cada cluster") +
#   labs(x="term",y="Log2 ratio of beta in topic2/topic1") 

###########################################################################
#GUARDA LOS RESULTADOS
#docs to topics
ldaOut.topics <- as.matrix(topics(ap_lda))
write.csv(ldaOut.topics,file="~/Clientes/Oi/ldaOutTopics03.csv")

#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ap_lda,20))
write.csv(ldaOut.terms,file="~/Clientes/Oi/ldaOutTerms03.csv")


#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ap_lda@gamma)
write.csv(topicProbabilities,file="~/Clientes/Oi/TopicProbabilities03.csv")


assignments <- augment(ap_lda, data = fboi_dtm)

pred<-assignments %>% select(document,.topic) %>%
  group_by(document) %>%
  summarise(cluster = max(.topic))

base.pred<-merge(base,pred,by.x="name",by.y="document")
write.csv(base.pred,file="~/Clientes/Oi/Cluster03.csv")