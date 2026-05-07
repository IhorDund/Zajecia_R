#' ---
#' title: "Analiza Asocjacji - Recenzje LOT"
#' author: "Analiza Tekstowa"
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable
#'     highlight: kate
#'     toc: true
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show
#' ---

#+ setup, include=FALSE
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)

#' # Wymagane pakiety
library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(ggthemes)

#' # Wczytanie i przygotowanie danych
data <- read.csv("C:/Users/id474466/Downloads/LOT_reviews.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
corpus <- VCorpus(VectorSource(data$Review_Text))

# Przetwarzanie tekstu
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "\\|")
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

# Usuniêcie s³ów kluczowych, które dominuj¹ w tekœcie, aby lepiej widzieæ asocjacje
corpus <- tm_map(corpus, removeWords, c("flight", "lot", "airline", "flights"))
corpus <- tm_map(corpus, stripWhitespace)

# Tworzenie macierzy TDM
tdm <- TermDocumentMatrix(corpus)

#' # Analiza Asocjacji dla s³owa: "food"
#' W tej sekcji sprawdzamy, jakie pojêcia najczêœciej wspó³wystêpuj¹ z jedzeniem serwowanym na pok³adzie.

#+ food_analysis
target_word_1 <- "food"
cor_limit_1 <- 0.25 # Próg obni¿ony, aby uzyskaæ szerszy kontekst

assoc_1 <- findAssocs(tdm, target_word_1, corlimit = cor_limit_1)
assoc_df_1 <- data.frame(
  word = factor(names(assoc_1[[target_word_1]]), levels = rev(names(assoc_1[[target_word_1]]))),
  score = assoc_1[[target_word_1]]
)

# Wykres lizakowy klasyczny
ggplot(assoc_df_1, aes(x = score, y = word)) +
  geom_segment(aes(x = 0, xend = score, y = word, yend = word), color = "#a6bddb", size = 1.2) +
  geom_point(color = "#0570b0", size = 4) +
  geom_text(aes(label = round(score, 2)), hjust = -0.3, size = 3.5) +
  theme_minimal() +
  labs(title = paste("Asocjacje ze s³owem:", target_word_1),
       x = "Korelacja Pearsona", y = "S³owo")

# Wykres lizakowy z natê¿eniem koloru
ggplot(assoc_df_1, aes(x = score, y = word, color = score)) +
  geom_segment(aes(x = 0, xend = score, y = word, yend = word), size = 1.2) +
  geom_point(size = 4) +
  scale_color_gradient(low = "#a6bddb", high = "#08306b") +
  theme_minimal() +
  labs(title = paste("Natê¿enie asocjacji:", target_word_1),
       x = "Korelacja Pearsona", y = "S³owo", color = "Si³a")

#' # Analiza Asocjacji dla s³owa: "comfort"
#' Sprawdzamy, co pasa¿erowie ³¹cz¹ z komfortem podró¿y (np. siedzenia, przestrzeñ).

#+ comfort_analysis
target_word_2 <- "comfort"
cor_limit_2 <- 0.25

assoc_2 <- findAssocs(tdm, target_word_2, corlimit = cor_limit_2)
assoc_df_2 <- data.frame(
  word = factor(names(assoc_2[[target_word_2]]), levels = rev(names(assoc_2[[target_word_2]]))),
  score = assoc_2[[target_word_2]]
)

# Wykres lizakowy klasyczny
ggplot(assoc_df_2, aes(x = score, y = word)) +
  geom_segment(aes(x = 0, xend = score, y = word, yend = word), color = "#addd8e", size = 1.2) +
  geom_point(color = "#238b45", size = 4) +
  geom_text(aes(label = round(score, 2)), hjust = -0.3, size = 3.5) +
  theme_minimal() +
  labs(title = paste("Asocjacje ze s³owem:", target_word_2),
       x = "Korelacja Pearsona", y = "S³owo")

# Wykres lizakowy z natê¿eniem koloru
ggplot(assoc_df_2, aes(x = score, y = word, color = score)) +
  geom_segment(aes(x = 0, xend = score, y = word, yend = word), size = 1.2) +
  geom_point(size = 4) +
  scale_color_gradient(low = "#addd8e", high = "#00441b") +
  theme_minimal() +
  labs(title = paste("Natê¿enie asocjacji:", target_word_2),
       x = "Korelacja Pearsona", y = "S³owo", color = "Si³a")

