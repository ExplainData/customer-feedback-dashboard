#####################
# Datenvorbereitung #
#####################
# Installieren und laden der benötigten Pakete
install.packages("rvest")
install.packages("dplyr")
install.packages("ggplot2")

library("rvest")
library("dplyr")
library("ggplot2")

# Anzahl der Seiten festlegen, die gescraped werden sollen
pageSequence <- seq(from = 1705, to = 1796)

# Vektoren für gesammelte Daten
review_dates_all <- c()
review_comments_all <- c()
review_stars_all <- c()

# Schleife, um jede Seite zu durchlaufen und die Daten zu sammeln
for (i in pageSequence) {
  # HTML-Inhalt der Seite speichern
  if (i == 1) {
    url <- "https://www.trustpilot.com/review/bulk.com/uk"
  } else {
    url <- paste0("https://www.trustpilot.com/review/bulk.com/uk", '?page=', i)
  }
  
  print(i)
  content <- read_html(url)
  
  # Kommentare, Bewertungen und Daten aus dem Inhalt extrahieren
  review_comments <- content %>%
    html_nodes(".styles_reviewContent__0Q2Tg") %>%
    html_text()
  
  review_stars <- content %>%
    html_elements(xpath = "//*[@class='styles_reviewHeader__iU9Px']") %>%
    html_attr("data-service-review-rating")
  
  review_dates <- content %>%
    html_elements(xpath = "//*[@class='typography_body-m__xgxZ_ typography_appearance-default__AAY17 typography_color-black__5LYEn']") %>%
    html_text()
  
  # Die extrahierten Daten den Vektoren hinzufügen
  review_comments_all <- append(review_comments_all, review_comments)
  review_dates_all <- append(review_dates_all, review_dates)
  review_stars_all <- append(review_stars_all, review_stars)
}

# Datensatz erstellen und als CSV-Datei speichern
reviews <- data.frame(date = review_dates_all, stars = review_stars_all, comments = review_comments_all)
write.csv(reviews, "reviews.csv", row.names = TRUE)

# Aufbereitung der Daten
reviews_split <- reviews

# Entfernen der letzten 6 Zeichen aus den Kommentaren
reviews_split$comments <- substr(reviews_split$comments, 1, nchar(reviews_split$comments) - 34)

# ID-Spalte hinzufügen
reviews_split$id <- row.names(reviews_split)
reviews_split$id <- as.numeric(reviews_split$id)

# Datumsformatierung
reviews_split$date <- substring(reviews_split$date, 21)
reviews_split <- reviews_split %>%
  mutate(date = as.Date(date, format = "%B %d, %Y"))

# Auswahl der benötigten Spalten
reviews_split <- select(reviews_split, "id", "date", "stars", "comments")

# Bereinigen der Kommentare
remove_signs_emojis_except_pound <- function(text) {
  pattern <- "[^[:alnum:]£[:space:]]"
  cleaned_text <- gsub(pattern, " ", text)
  return(cleaned_text)
}

reviews_split$comments <- sapply(reviews_split$comments, remove_signs_emojis_except_pound)

# Wörter von einander trennen
reviews_split$comments <- gsub("([a-z])([A-Z])", "\\1 \\2", reviews_split$comments)

# Alle Kommentare mit weniger als 14 Buchstaben entfernen
reviews_split <- reviews_split[nchar(reviews_split$comments) >= 14,]

# Gleiche Wörter ersetzen (vereinheitlichen)
reviews_split <- reviews_split %>%
  mutate(comments = stringr::str_replace_all(comments, "prices", "price")) %>%
  mutate(comments = stringr::str_replace_all(comments, "Prices", "price")) %>%
  mutate(comments = stringr::str_replace_all(comments, "PRICES", "price")) %>%
  mutate(comments = stringr::str_replace_all(comments, "products", "product")) %>%
  mutate(comments = stringr::str_replace_all(comments, "Products", "product")) %>%
  mutate(comments = stringr::str_replace_all(comments, "PRODUCTS", "product")) %>%
  mutate(comments = stringr::str_replace_all(comments, "powders", "powder")) %>%
  mutate(comments = stringr::str_replace_all(comments, "Powders", "powder")) %>%
  mutate(comments = stringr::str_replace_all(comments, "POWDERS", "powder"))

#########################
# deskriptive Statistik #
#########################
# Sternebewertungen in numerische Werte konvertieren
reviews_split$stars <- as.numeric(reviews_split$stars)

# Zusammenfassung der Sternebewertungen
summary(reviews_split$stars)

# Berechnung von Durchschnitt, Median, Standardabweichung und Spannweite
# ---------------------------------------------------------------------
durchschnitt <- mean(reviews_split$stars)
median <- median(reviews_split$stars)
standardabweichung <- sd(reviews_split$stars)
spannweite <- range(reviews_split$stars)

# Ergebnisse ausgeben
# -------------------
cat("Durchschnitt: ", durchschnitt, "\n",
    "Median: ", median, "\n",
    "Standardabweichung: ", standardabweichung, "\n",
    "Spannweite: ", spannweite[1], "-", spannweite[2], "\n")

# Visualisierung der Sternebewertungen
# ------------------------------------
ggplot(reviews_split, aes(x = stars)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogramm der Sternebewertungen",
       x = "Sternebewertung",
       y = "Anzahl der Bewertungen") +
  theme_minimal()
#####################
# Sentiment-Analyse #
#####################
# Installieren und laden benötigte R-Pakete
# ----------------------------------------
install.packages("tidytext")
install.packages("tidyverse")

library(tidytext)
library(tidyverse)

# Daten für die Textanalyse vorbereiten
# -------------------------------------
daten_tidy <- reviews_split %>%
  unnest_tokens(word, comments) %>%
  anti_join(stop_words)

# Sentiment-Lexikon laden und alle doppelten Wörter löschen
# --------------------------------------------------------
data("sentiments")
sentiments <- sentiments %>%
  group_by(word) %>%
  slice(which.min(n())) %>%
  ungroup()

# Entfernen von Wörtern, die nicht im Sentiment-Lexikon enthalten sind
# -------------------------------------------------------------------
daten_tidy <- daten_tidy %>%
  semi_join(sentiments, by = "word")

# Sentiment Analyse durchführen
# -----------------------------
sentiment_analysis <- daten_tidy %>%
  inner_join(sentiments) %>%
  group_by(id) %>%
  summarise(sentiment_score = sum(if_else(sentiment == "positive", 1, -1))) %>%
  ungroup()

# Sentiment score zum Datensatz hinzufügen
# ---------------------------------------
daten_sentiment <- reviews_split %>%
  right_join(sentiment_analysis, by = "id")

# Berechnung von Durchschnittswert, positiven, negativen und neutralen Kommentaren
# --------------------------------------------------------------------------------
average_sentiment_score <- mean(daten_sentiment$sentiment_score, na.rm = TRUE)
positive_comments <- sum(daten_sentiment$sentiment_score > 0, na.rm = TRUE)
negative_comments <- sum(daten_sentiment$sentiment_score < 0, na.rm = TRUE)
neutral_comments <- sum(daten_sentiment$sentiment_score == 0, na.rm = TRUE)

# Visualisierung
# --------------
ggplot(daten_sentiment, aes(x = sentiment_score)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Sentiment Scores",
       x = "Sentiment Score",
       y = "Number of Comments") +
  theme_minimal()
######################
# Themenmodellierung #
######################
# Installieren und laden benötigte R-Pakete
# Benötigte Pakete installieren und laden
# ---------------------------------------
install.packages("topicmodels")
install.packages("tm")
install.packages("stringr")

library(tm)
library(topicmodels)
library(stringr)

# Daten vorbereiten
# -----------------
# Erstelle einen VCorpus aus den Kommentaren
corpus <- VCorpus(VectorSource(reviews_split$comments))

# Textbereinigung
# ----------------
# Umwandlung in Kleinbuchstaben
corpus <- tm_map(corpus, content_transformer(tolower))
# Entfernen von Zahlen
corpus <- tm_map(corpus, removeNumbers)
# Entfernen von Satzzeichen
corpus <- tm_map(corpus, removePunctuation)
# Entfernen von Stoppwörtern
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Erstellen einer Dokument-Wort-Matrix
# ------------------------------------
dtm <- DocumentTermMatrix(corpus)

# Entfernen von Duplikaten
ui = unique(dtm$i)
text_dtm.new = dtm[ui,]

# LDA-Modell erstellen
# --------------------
lda_modell <- LDA(text_dtm.new, k = 4, control = list(seed = 1234))

# Anzeigen der Themen und ihrer wichtigsten Wörter
# ------------------------------------------------
themen <- tidytext::tidy(lda_modell, matrix = "beta")

top_themen_woerter <- themen %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Ausgabe der Top-Wörter für jedes Thema
print(top_themen_woerter, n=50)

#######################
# Korrelationsanalyse #
#######################
# Benötigte Pakete installieren und laden
# ---------------------------------------
install.packages("tidyr")
install.packages("tidytext")
install.packages("stopwords")

library(tm)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(stopwords)

# Daten vorbereiten
# -----------------

# Erstelle einen Dataframe aus dem bereinigten Text
bereinigter_text <- data.frame(id = 1:length(corpus), text = sapply(corpus, as.character))

# Tokenisierung der Texte
# -----------------------
tokens <- bereinigter_text %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# Zähle die Häufigkeit der Wörter
# -------------------------------
wort_haeufigkeiten <- tokens %>%
  count(word, sort = TRUE)

# Identifiziere wichtige Themen oder Schlüsselwörter
# --------------------------------------------------
schluesselwoerter <- c("product", "delivery", "service", "powder", "price", "quality")

# Codiere die Themen oder Schlüsselwörter binär
# ---------------------------------------------
tokens_binär <- tokens %>%
  mutate(value = 1) %>%
  filter(word %in% schluesselwoerter) %>%
  group_by(id, word) %>%
  summarise(value = max(value)) %>%
  ungroup() %>%
  complete(id = 1:max(id), word = schluesselwoerter, fill = list(value = 0)) %>%
  spread(word, value, fill = 0)

# Füge Sternebewertungen zu den Daten hinzu
# -----------------------------------------
daten_mit_themen <- reviews_split %>%
  mutate(id = 1:n()) %>%
  left_join(tokens_binär, by = "id")

# Berechne die Korrelationskoeffizienten
# --------------------------------------
korrelationen <- cor(daten_mit_themen[,c("stars", schluesselwoerter)], method = "pearson", use = "pairwise.complete.obs")

# Berechne Korrelationen und P-Werte für alle Paar-Kombinationen der Spalten
# -------------------------------------------------------------------------
results <- combn(c("stars", schluesselwoerter), 2, function(column_pair) {
  test_result <- cor.test(daten_mit_themen[[column_pair[1]]], daten_mit_themen[[column_pair[2]]], method = "pearson")
  return(tibble(var1 = column_pair[1], var2 = column_pair[2], cor = test_result$estimate, p_value = test_result$p.value))
}, simplify = FALSE) %>% bind_rows()

# Ergebnisse anzeigen
# -------------------
results_df <- data.frame(results[c(1:6),])

##################
# Visualisierung #
##################

######################
# Zeitreihendiagramm #
######################
# Benötigte Bibliotheken laden
# -----------------------------
library(lubridate)

# Daten vorbereiten
# -----------------

# Datumsspalte in ein Date-Objekt konvertieren
daten_sentiment$date <- as.Date(daten_sentiment$date, format = "%Y-%m-%d")

# Erstelle eine neue Spalte für Jahr und Monat
daten_sentiment$year <- format(daten_sentiment$date, "%Y")
daten_sentiment$month <- format(daten_sentiment$date, "%m/%Y")

# Berechnung der Durchschnittswerte
# ----------------------------------

# Durchschnittliche Sternebewertungen und Sentiment-Scores pro Monat oder Jahr berechnen
monthly_averages <- daten_sentiment %>%
  group_by(month) %>% # Oder group_by(year) für jährliche Daten
  summarise(mean_rating = mean(stars), mean_sentiment = mean(sentiment_score))

# Durchschnittliche Sternebewertungen und Sentiment-Scores pro Jahr berechnen
yearly_averages <- daten_sentiment %>%
  group_by(year) %>% # Oder group_by(year) für jährliche Daten
  summarise(mean_rating = mean(stars), mean_sentiment = mean(sentiment_score))

# Filtern der Daten ab dem gewünschten Datum
monthly_averages <- monthly_averages %>% 
  filter(month >= as.Date("2011-10-01"))

yearly_averages <- yearly_averages %>% 
  filter(year >= as.Date("2011-01-01"))

# Datumskonvertierung für die Grafik
monthly_averages$month <- as.Date(paste0("01/", monthly_averages$month), format = "%d/%m/%Y")
yearly_averages$year <- as.Date(paste0("01/01/", yearly_averages$year), format = "%d/%m/%Y")

# Grafik erstellen
# -----------------

# Erstellen eines Zeitdiagramms der durchschnittlichen Sentiment-Scores
zd_sentiment <- ggplot(yearly_averages, aes(x = year))+
  geom_line(aes(y = mean_sentiment, color = "Mean Sentiment")) +
  scale_color_manual(values = c("Mean Rating" = "blue", "Mean Sentiment" = "red")) +
  labs(title = "Durchschnittlicher Sentiment-Score im Laufe der Zeit",
       x = "Zeit",
       y = "Durchschnittswerte",
       color = "Legende") +
  theme_minimal() +
  scale_x_date(date_labels="%Y", date_breaks="1 year") +
  scale_y_continuous(limits=c(0, NA), expand=c(0,0))

# Grafik als JPEG speichern
# --------------------------

# Speichere das ggplot-Objekt als Jpeg im Arbeitsverzeichnis
ggsave("zd_sentiment.jpg", plot = zd_sentiment, dpi = 300)

##################
# Balkendiagramm #
##################

# Daten vorbereiten
# -----------------


# Zählen Sie die Anzahl der Bewertungen für jede Sternebewertung
bewertung_count <- table(reviews_split$stars)

# Erstellen Sie einen Dataframe aus der Zusammenfassung
bewertung_df <- data.frame(
  Sternebewertung = as.factor(names(bewertung_count)),
  Anzahl = as.integer(bewertung_count)
)

# Balkendiagramm erstellen
# ------------------------

# Erstellen Sie das Balkendiagramm
Balkendiagramm_stars <- ggplot(data = bewertung_df, aes(x = Sternebewertung, y = Anzahl)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Verteilung der Sternebewertungen",
       x = "Sternebewertung",
       y = "Anzahl der Bewertungen") +
  theme_minimal()

# Balkendiagramm als JPEG speichern
# --------------------------------

# Speichere das ggplot-Objekt als Jpeg im Arbeitsverzeichnis
ggsave("Balkendiagramm_starst.jpg", plot = Balkendiagramm_stars, dpi = 300)

##############
# Word Cloud #
##############
# Pakete installieren und laden
install.packages(c("wordcloud", "RColorBrewer", "udpipe", "hunspell", "Cairo"))
library(RColorBrewer)
library(wordcloud)
library(udpipe)
library(hunspell)
library(Cairo)

# Term-Dokument-Matrix erstellen
tdm <- TermDocumentMatrix(corpus)

# Tidy Data erstellen und Stop-Wörter entfernen
daten_tidy_2 <- reviews_split %>%
  unnest_tokens(word, comments) %>%
  anti_join(stop_words)

# Zahlen und Satzzeichen entfernen
daten_tidy_2$word <- gsub("[[:punct:]]|\\d+", "", daten_tidy_2$word)

# Buchstaben in Kleinbuchstaben umwandeln
daten_tidy_2$word <- tolower(daten_tidy_2$word)

# Leere Zeilen entfernen
daten_tidy_2 <- subset(daten_tidy_2, word != "")


# Zusammengeschriebene Wörter trennen 

word_counter <- 0

# Funktion zum Trennen von Wörtern
separate_words <- function(text) {
  print(text)
  word_counter <<- word_counter + 1
  print(word_counter)
  
  if (hunspell_check(text)) {
    return(text)
  }
  
  n <- nchar(text)
  for (i in 1:(n - 1)) {
    prefix <- substr(text, 1, i)
    suffix <- substr(text, i + 1, n)
    
    if (hunspell_check(prefix) || hunspell_check(suffix)) {
      return(paste(prefix, suffix, sep = " "))
    }
  }
  return(text)
}

# Anwenden der Funktion zum Trennen von Wörtern
daten_tidy_2$word <- sapply(daten_tidy_2$word, separate_words)

# Als CSV-Datei speichern
write.csv(daten_tidy_2, "daten_tidy_2.csv", row.names=TRUE)

# Tokenisierung der getrennten Wörter und Entfernen von Stop-Wörtern
daten_tidy_2 <- daten_tidy_2 %>%
  unnest_tokens(word, word) %>%
  anti_join(stop_words)

# Wörter filtern, die nicht im englischen Wörterbuch existieren
daten_tidy_2 <- daten_tidy_2 %>%
  filter(hunspell_check(word))

# Duplikate basierend auf ID und Wort entfernen
daten_tidy_2 <- daten_tidy_2 %>%
  distinct(id, word, .keep_all = TRUE)

# Durchschnittliche Sternebewertungen pro Wort berechnen
avg_word_ratings <- daten_tidy_2 %>%
  group_by(word) %>%
  summarize(total_stars = sum(stars), word_count = n()) %>%
  mutate(average_rating = total_stars / word_count)

# Häufigkeit der Wörter berechnen
word_freq <- rowSums(as.matrix(tdm))
word_freq <- data.frame(Words = names(word_freq), Frequency = word_freq)
word_freq <- word_freq[order(-word_freq$Frequency),]
word_freq <- word_freq[word_freq$Words != "don",]

# Farben basierend auf Sternebewertungen zuweisen
color_mapping <- function(avg_rating) {
  if (avg_rating <= 1.5) {
    return("red")
  } else if (avg_rating <= 2.5) {
    return("orange")
  } else if (avg_rating <= 3.5) {
    return("yellow")
  } else if (avg_rating <= 4.5) {
    return("blue")
  } else {
    return("green")
  }
}


# Verknüpfen von word_freq und avg_word_ratings
names(avg_word_ratings)[1] <- "Words"
word_freq <- merge(word_freq, avg_word_ratings, by = "Words")

# Laden des englischen Sprachmodells
ud_model_de <- udpipe_download_model(language = "english")

# Erstellen eines udpipe-Objekts
ud_model_de <- udpipe_load_model(ud_model_de$file_model)

# Durchführen des POS-Taggings
annotated_words <- udpipe_annotate(ud_model_de, x = word_freq$Words)
annotated_words <- as.data.frame(annotated_words)

# Nur Nomen und Adjektive behalten
selected_words <- annotated_words[annotated_words$upos %in% c("NOUN", "ADJ"), "token"]
word_freq <- word_freq[word_freq$Words %in% selected_words,]

word_freq$Color <- sapply(word_freq$average_rating, color_mapping)

# Word cloud erstellen
set.seed(1234)  
wordcloud(words = word_freq$Words, 
          freq = word_freq$Frequency, 
          min.freq = 5,  
          max.words = 150,  
          random.order = FALSE,
          colors = word_freq$Color)


    
    
    