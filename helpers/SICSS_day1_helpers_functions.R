



# Compute cosine similarity between two vectors
cos_sim <- function(x, y) {
  x <- x / sqrt(sum(x^2))  # normalize x
  y <- y / sqrt(sum(y^2))  # normalize y
  sum(x * y)               # dot product of unit vectors
}

# Return the n most/least similar words to a target word based on cosine similarity
most_similar <- function(word, embeddings, n = 10, direction = "most") {
  if (!(word %in% rownames(embeddings))) return(NULL)
  target <- embeddings[word, , drop = FALSE]
  
  # Compute cosine similarity between target and all other words
  sims <- apply(embeddings, 1, function(x) cos_sim(x, target))
  
  # Remove the target word itself
  sims <- sims[names(sims) != word]
  
  # Sort based on requested direction
  if (direction == "most") {
    sims <- sort(sims, decreasing = TRUE)
  } else if (direction == "least") {
    sims <- sort(sims, decreasing = FALSE)
  } else {
    stop("direction must be either 'most' or 'least'")
  }
  
  return(head(sims, n))
}


# Plot Word Embeddings in 2D using PCA

plot_words <- function(words, embeddings) {
  # This function takes a list of words and a word embedding matrix,
  # reduces the embedding dimensions to 2 using PCA,
  # and plots the words in a 2D scatterplot.
  found <- words[words %in% rownames(embeddings)]    # filter missing words
  vectors <- embeddings[found, ]                     # Subset the embeddings matrix to only the selected words
  
  pca <- prcomp(vectors)                             # run PCA
  df <- data.frame(pca$x[, 1:2], word = found)       # extract PC1 and PC2
  
  ggplot(df, aes(x = PC1, y = PC2, label = word)) +  # build plot
    geom_point() +
    geom_text(vjust = 1.5, hjust = 0.5) +
    ggtitle("Word Embedding Visualization (PCA)") +
    theme_minimal()
}


# Solve word analogies: word1 is to word2 as word3 is to ?

analogy <- function(word1, word2, word3, embeddings, n = 5, verbose = TRUE) {
  # Computes: vec(word2) - vec(word1) + vec(word3)
  # If verbose = TRUE, prints the analogy equation
  
  # Ensure all words exist in the embeddings
  words <- c(word1, word2, word3)
  if (!all(words %in% rownames(embeddings))) return(NULL)
  
  if (verbose) {
    cat(sprintf("Solving: %s - %s + %s = ?\n", word1, word2, word3))
  }
  
  # Compute analogy vector
  target <- embeddings[word1, ] - embeddings[word2, ] + embeddings[word3, ]
  target <- target / sqrt(sum(target^2))  # normalize
  
  # Compute cosine similarity with all embeddings
  sims <- apply(embeddings, 1, function(x) {
    x <- x / sqrt(sum(x^2))
    sum(x * target)
  })
  
  # Exclude original words from results
  sims <- sort(sims[!names(sims) %in% words], decreasing = TRUE)
  
  head(sims, n)
}


#Plot a given word across time, per candidate

plot_word_mentions <- function(word,
                               data,
                               text_column = text,
                               candidates = NULL,
                               time_unit = c("year_quarter", "month", "year")) {
  
  time_unit <- match.arg(time_unit)
  text_col <- rlang::enquo(text_column)
  
  if (!is.null(candidates)) {
    data <- data %>% filter(candidate %in% candidates)
  }
  
  data <- data %>%
    mutate(
      time_group = case_when(
        time_unit == "year_quarter" ~ str_c(lubridate::year(date), "_Q", lubridate::quarter(date)),
        time_unit == "month" ~ str_c(lubridate::year(date), "_", stringr::str_pad(lubridate::month(date), 2, "left", "0")),
        time_unit == "year" ~ as.character(lubridate::year(date))
      ),
      word_count = str_count(!!text_col, "\\w+"),
      word_mentions = str_count(!!text_col, regex(word, ignore_case = TRUE)),
      word_prop = word_mentions / word_count
    ) %>%
    group_by(candidate, time_group) %>%
    summarise(
      avg_proportion = mean(word_prop, na.rm = TRUE),
      .groups = "drop"
    )
  
  ggplot(data, aes(x = time_group, y = avg_proportion, 
                   color = candidate, group = candidate)) +
    geom_line(linewidth = 1.2) +
    geom_point() +
    labs(
      title = paste("Proportion of", shQuote(word), "Mentions Over Time"),
      subtitle = if (!is.null(candidates)) paste("Candidates:", paste(candidates, collapse = ", ")) else NULL,
      x = stringr::str_to_title(gsub("_", " ", time_unit)),
      y = "Avg Word Proportion (per Speech)",
      color = "Candidate"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}

