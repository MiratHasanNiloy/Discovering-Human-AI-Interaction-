
needed <- c("jsonlite", "dplyr", "readr", "stringr", "tibble", "purrr")
to_install <- setdiff(needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(purrr)
})


ARXIV_JSON <- "C:/Users/Mashrur/Documents/human_ai_clustering/archive/arxiv-metadata-oai-snapshot.json"
OUT_CSV    <- "C:/Users/Mashrur/Documents/human_ai_clustering/arxiv_HAI_corpus.csv"

MIN_YEAR <- 2015
PAGESIZE <- 50000   


HC_CAT_RX <- "(?:^|\\s)cs\\.HC(?:\\s|$)"
AI_CAT_RX <- "(?:^|\\s)cs\\.AI(?:\\s|$)"

HAI_TERMS_RX <- regex(
  paste(c(
    "human[- ]AI", "human[- ]in[- ]the[- ]loop",
    "AI[- ]assisted", "decision support",
    "user trust", "trust in AI", "trust calibration",
    "explainab", "interpretab", "usability",
    "human[- ]centered AI", "human[- ]centred AI"
  ), collapse = "|"),
  ignore_case = TRUE
)

if (!file.exists(ARXIV_JSON)) {
  stop(
    "\nSnapshot not found at:\n  ", ARXIV_JSON,
    "\n\nDownload `arxiv-metadata-oai-snapshot.json` from",
    "\n  https://www.kaggle.com/datasets/Cornell-University/arxiv",
    "\nUnzip it, and place the .json at the path above (or change ARXIV_JSON).\n"
  )
}

cat(sprintf("Snapshot size: %.2f GB\n",
            file.info(ARXIV_JSON)$size / 1e9))

parse_year <- function(versions_entry, update_date) {
  v_created <- NA_character_
  if (!is.null(versions_entry)) {
    if (is.data.frame(versions_entry) && nrow(versions_entry) > 0) {
      v_created <- versions_entry$created[1]
    } else if (is.list(versions_entry) && length(versions_entry) > 0 &&
               !is.null(versions_entry[[1]]$created)) {
      v_created <- versions_entry[[1]]$created
    }
  }
  
  yr <- NA_integer_
  if (!is.na(v_created) && nzchar(v_created)) {
    m <- str_extract(v_created, "(?:19|20)\\d{2}")
    if (!is.na(m)) yr <- as.integer(m)
  }
  if (is.na(yr) && !is.na(update_date) && nzchar(update_date)) {
    yr <- suppressWarnings(as.integer(substr(update_date, 1, 4)))
  }
  yr
}

kept_chunks <- list()
total_seen  <- 0L
t0 <- Sys.time()

handler <- function(df) {
  total_seen <<- total_seen + nrow(df)
  
  is_hc <- str_detect(df$categories, HC_CAT_RX)
  is_ai <- str_detect(df$categories, AI_CAT_RX)
  keep  <- is_hc | (is_ai & str_detect(df$abstract, HAI_TERMS_RX))
  
  if (any(keep)) {
    sub <- df[keep, , drop = FALSE]
    sub_is_hc <- is_hc[keep]
    sub_is_ai <- is_ai[keep]
    
    years <- map2_int(sub$versions, sub$update_date, parse_year)
    
    src <- ifelse(sub_is_hc & sub_is_ai, "cs.HC+cs.AI",
                  ifelse(sub_is_hc,             "cs.HC",
                         "cs.AI-HAI"))
    
    tidy <- tibble(
      id          = sub$id,
      title       = str_squish(sub$title),
      abstract    = str_squish(sub$abstract),
      authors     = sub$authors,
      categories  = sub$categories,
      primary_cat = str_extract(sub$categories, "^\\S+"),
      year        = years,
      doi         = ifelse(is.na(sub$doi) | sub$doi == "", NA_character_, sub$doi),
      source      = src
    ) |>
      filter(
        !is.na(abstract),
        nchar(abstract) >= 200,
        !is.na(year),
        year >= MIN_YEAR
      )
    
    if (nrow(tidy) > 0) {
      kept_chunks[[length(kept_chunks) + 1L]] <<- tidy
    }
  }
  
  elapsed <- round(as.numeric(Sys.time() - t0, units = "secs"))
  total_kept <- if (length(kept_chunks)) sum(map_int(kept_chunks, nrow)) else 0L
  message(sprintf("scanned %s | kept %s | %ds elapsed",
                  format(total_seen, big.mark = ","),
                  format(total_kept,  big.mark = ","),
                  elapsed))
}

stream_in(
  file(ARXIV_JSON),
  handler  = handler,
  pagesize = PAGESIZE,
  verbose  = FALSE
)

corpus <- bind_rows(kept_chunks) |>
  distinct(id, .keep_all = TRUE) |>
  arrange(desc(year))

write_csv(corpus, OUT_CSV)

cat("\n=========== arXiv HAI corpus summary ===========\n")
cat("Saved to: ", OUT_CSV, "\n")
cat("Rows:     ", format(nrow(corpus), big.mark = ","), "\n\n")

cat("By source:\n")
print(count(corpus, source, sort = TRUE))

cat("\nBy primary category (top 15):\n")
print(head(count(corpus, primary_cat, sort = TRUE), 15))

cat("\nBy year:\n")
print(count(corpus, year))

needed <- c("quanteda", "textstem", "lexicon", "dplyr", "readr", "stringr", "tibble")
to_install <- setdiff(needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

suppressPackageStartupMessages({
  library(quanteda)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

IN_CSV  <- "C:/Users/Mashrur/Documents/human_ai_clustering/arxiv_HAI_corpus.csv"
OUT_CSV <- "C:/Users/Mashrur/Documents/human_ai_clustering/arxiv_HAI_clean.csv"

PHRASES <- c(
  "human ai",  "human in the loop",
  "human centered ai", "human centred ai",
  "ai assisted", "decision support",
  "user trust", "user study", "user experience", "user interface",
  "machine learning", "deep learning", "reinforcement learning",
  "natural language processing", "neural network",
  "computer vision",
  "explainable ai", "explainable artificial intelligence",
  "large language model", "large language models",
  "active learning", "transfer learning"
)

EXTRA_STOP <- c(
  "paper", "papers", "study", "studies", "research", "researcher",
  "result", "results", "method", "methods", "approach", "approaches",
  "propose", "proposed", "present", "presented", "introduce", "introduces",
  "show", "shows", "showed", "find", "finds", "found",
  "abstract", "introduction", "conclusion",
  "use", "used", "using", "based", "different", "various", "novel",
  "may", "can", "however", "also", "well",
  "arxiv", "et", "al", "fig", "figure", "table",
  "one", "two", "three", "first", "second"
)

cat("Reading: ", IN_CSV, "\n", sep = "")
docs <- read_csv(IN_CSV, show_col_types = FALSE)
cat("Rows in:               ", format(nrow(docs), big.mark = ","), "\n", sep = "")

docs <- docs |>
  filter(!is.na(abstract), nchar(abstract) >= 200)
cat("After abstract filter: ", format(nrow(docs), big.mark = ","), "\n", sep = "")

cat("\nBuilding corpus + tokenizing...\n")
qcorp <- corpus(docs, docid_field = "id", text_field = "abstract")

t0 <- Sys.time()
toks <- tokens(
  qcorp,
  what              = "word",
  remove_punct      = TRUE,
  remove_symbols    = TRUE,
  remove_numbers    = TRUE,
  remove_url        = TRUE,
  remove_separators = TRUE,
  split_hyphens     = FALSE
)
toks <- tokens_tolower(toks)
cat("  tokenize: ", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n", sep = "")

t0 <- Sys.time()
toks <- tokens_compound(toks, phrase(PHRASES), concatenator = "_")
cat("  compound: ", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n", sep = "")

t0 <- Sys.time()
toks <- tokens_remove(
  toks,
  pattern = c(stopwords("en"), EXTRA_STOP),
  padding = FALSE
)
toks <- tokens_select(toks, min_nchar = 3, padding = FALSE)
cat("  stopwords + short: ",
    round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n", sep = "")

cat("\nLemmatizing...\n")
t0 <- Sys.time()
vocab_in  <- types(toks)
vocab_out <- textstem::lemmatize_words(vocab_in)
diff_idx <- which(vocab_in != vocab_out)
cat("  unique types: ", format(length(vocab_in), big.mark = ","),
    " | changed by lemmatization: ", format(length(diff_idx), big.mark = ","), "\n", sep = "")

if (length(diff_idx) > 0) {
  toks <- tokens_replace(
    toks,
    pattern     = vocab_in[diff_idx],
    replacement = vocab_out[diff_idx],
    valuetype   = "fixed"
  )
}
cat("  lemmatize: ", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n", sep = "")

cat("\nAssembling cleaned text...\n")
text_clean <- vapply(
  as.list(toks),
  function(tk) paste(tk, collapse = " "),
  character(1)
)

clean_df <- docs |>
  mutate(text_clean = text_clean,
         n_tokens   = str_count(text_clean, "\\S+")) |>
  filter(n_tokens >= 20) |>     # drop near-empty cleaned abstracts
  select(id, year, primary_cat, source, n_tokens, abstract, text_clean)

cat("After clean-text filter (>=20 tokens): ",
    format(nrow(clean_df), big.mark = ","), "\n", sep = "")

write_csv(clean_df, OUT_CSV)

cat("\n=========== Preprocessing summary ===========\n")
cat("Saved to:        ", OUT_CSV, "\n", sep = "")
cat("Rows:            ", format(nrow(clean_df), big.mark = ","), "\n", sep = "")

cat("\nTokens per cleaned document:\n")
print(summary(clean_df$n_tokens))

cat("\nVocabulary size after cleaning + lemmatization: ",
    format(length(vocab_out |> unique()), big.mark = ","), "\n", sep = "")

cat("\nSample (first 200 chars of text_clean for 3 random docs):\n")
set.seed(1)
samp <- clean_df |> slice_sample(n = 3)
for (i in seq_len(nrow(samp))) {
  cat("---\n[", samp$id[i], "] ", substr(samp$text_clean[i], 1, 200),
      if (nchar(samp$text_clean[i]) > 200) " ..." else "", "\n", sep = "")
}

needed <- c("text2vec", "Matrix", "dplyr", "readr", "stringr")
to_install <- setdiff(needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

suppressPackageStartupMessages({
  library(text2vec)
  library(Matrix)
  library(dplyr)
  library(readr)
  library(stringr)
})

PROJECT_DIR <- "C:/Users/Mashrur/Documents/human_ai_clustering"
IN_CSV      <- file.path(PROJECT_DIR, "arxiv_HAI_clean.csv")
OUT_TFIDF   <- file.path(PROJECT_DIR, "hai_tfidf.rds")
OUT_VOCAB   <- file.path(PROJECT_DIR, "hai_vocab.rds")
OUT_META    <- file.path(PROJECT_DIR, "hai_doc_meta.rds")

MIN_DOC_FREQ <- 10L
MAX_DOC_PROP <- 0.50

cat("Reading: ", IN_CSV, "\n", sep = "")
clean_df <- read_csv(IN_CSV, show_col_types = FALSE)
cat("Documents: ", format(nrow(clean_df), big.mark = ","), "\n", sep = "")

doc_meta <- clean_df |>
  select(id, year, primary_cat, source)

texts <- clean_df$text_clean
n_docs <- length(texts)
tok_fun <- word_tokenizer

cat("\nBuilding vocabulary...\n")
t0 <- Sys.time()
it <- itoken(texts, tokenizer = tok_fun, progressbar = TRUE)
vocab <- create_vocabulary(it, progressbar = TRUE)

cat("  raw vocabulary size: ", format(nrow(vocab), big.mark = ","), "\n", sep = "")

vocab <- prune_vocabulary(
  vocab,
  term_count_min = MIN_DOC_FREQ,
  doc_proportion_max = MAX_DOC_PROP
)

cat("  pruned vocabulary:   ", format(nrow(vocab), big.mark = ","),
    " (", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s)\n", sep = "")

term_names <- vocab$term

cat("\nBuilding TF-IDF matrix...\n")
t0 <- Sys.time()

vectorizer <- vocab_vectorizer(vocab)
tfm <- create_dtm(it, vectorizer, progressbar = TRUE)

tfidf <- TfIdf$new(norm = "l2", sublinear_tf = TRUE, smooth_idf = TRUE)
dtm_tfidf <- fit_transform(tfm, tfidf)

cat("  matrix: ", format(object.size(dtm_tfidf), units = "auto"),
    " | ", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n", sep = "")

stopifnot(nrow(dtm_tfidf) == n_docs)
stopifnot(ncol(dtm_tfidf) == length(term_names))
stopifnot(all(rownames(dtm_tfidf) == seq_len(n_docs)))

rownames(dtm_tfidf) <- clean_df$id
colnames(dtm_tfidf) <- term_names

sparsity <- 1 - length(dtm_tfidf@x) / (nrow(dtm_tfidf) * ncol(dtm_tfidf))
nonzero_per_doc <- Matrix::rowSums(dtm_tfidf > 0)

saveRDS(dtm_tfidf, OUT_TFIDF)
saveRDS(term_names, OUT_VOCAB)
saveRDS(doc_meta, OUT_META)

cat("\n=========== TF-IDF summary ===========\n")
cat("Saved:\n")
cat("  ", OUT_TFIDF, "\n", sep = "")
cat("  ", OUT_VOCAB, "\n", sep = "")
cat("  ", OUT_META, "\n", sep = "")
cat("\nDimensions: ", format(nrow(dtm_tfidf), big.mark = ","), " docs x ",
    format(ncol(dtm_tfidf), big.mark = ","), " terms\n", sep = "")
cat("Sparsity:   ", round(100 * sparsity, 2), "%\n", sep = "")
cat("Nonzero terms per doc:\n")
print(summary(nonzero_per_doc))

cat("\nTop 20 terms by total TF-IDF mass:\n")
term_mass <- colSums(dtm_tfidf)
top_terms <- sort(term_mass, decreasing = TRUE)[1:20]
print(data.frame(term = names(top_terms), mass = round(top_terms, 2)))



needed <- c("Matrix", "irlba", "uwot", "readr")
to_install <- setdiff(needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

suppressPackageStartupMessages({
  library(Matrix)
  library(irlba)
  library(uwot)
  library(readr)
})

PROJECT_DIR <- "C:/Users/Mashrur/Documents/human_ai_clustering"
IN_TFIDF    <- file.path(PROJECT_DIR, "hai_tfidf.rds")
IN_META     <- file.path(PROJECT_DIR, "hai_doc_meta.rds")

OUT_SVD50   <- file.path(PROJECT_DIR, "hai_svd50.rds")
OUT_UMAP2   <- file.path(PROJECT_DIR, "hai_umap2.rds")
OUT_UMAP15  <- file.path(PROJECT_DIR, "hai_umap15.rds")
OUT_META    <- file.path(PROJECT_DIR, "hai_reduction_meta.rds")

SVD_DIMS <- 50L
SEED     <- 42L

cat("Loading TF-IDF matrix...\n")
X <- readRDS(IN_TFIDF)
doc_meta <- readRDS(IN_META)
stopifnot(nrow(X) == nrow(doc_meta))
n_docs <- nrow(X)
cat("  ", format(n_docs, big.mark = ","), " docs x ",
    format(ncol(X), big.mark = ","), " terms\n", sep = "")

cat("\nTruncated SVD (k = ", SVD_DIMS, ")...\n", sep = "")
t0 <- Sys.time()
svd_fit <- irlba(X, nv = SVD_DIMS, nu = SVD_DIMS, verbose = FALSE)
X_svd <- svd_fit$u %*% diag(svd_fit$d)   # documents in 50D latent space
rownames(X_svd) <- rownames(X)
cat("  done in ", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n", sep = "")
fro_norm2 <- sum(X^2)
var_expl  <- sum(svd_fit$d^2) / fro_norm2
cat("  approx. variance captured (top ", SVD_DIMS, " SVD): ",
    round(100 * var_expl, 1), "%\n", sep = "")
cat("  singular values 1-5: ", paste(round(svd_fit$d[1:5], 2), collapse = ", "), "\n", sep = "")

saveRDS(X_svd, OUT_SVD50)

cat("\nUMAP 2D (this may take several minutes on ~44k docs)...\n")
t0 <- Sys.time()
umap2 <- umap(
  X_svd,
  n_neighbors = 30,
  min_dist    = 0.1,
  metric      = "cosine",
  n_components = 2,
  verbose     = TRUE,
  n_threads   = max(1L, parallel::detectCores() - 1L),
  seed        = SEED
)
rownames(umap2) <- rownames(X_svd)
cat("  done in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min\n", sep = "")

saveRDS(umap2, OUT_UMAP2)

cat("\nUMAP 15D (clustering space)...\n")
t0 <- Sys.time()
umap15 <- umap(
  X_svd,
  n_neighbors = 30,
  min_dist    = 0.0,
  metric      = "cosine",
  n_components = 15,
  verbose     = TRUE,
  n_threads   = max(1L, parallel::detectCores() - 1L),
  seed        = SEED
)
rownames(umap15) <- rownames(X_svd)
cat("  done in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min\n", sep = "")

saveRDS(umap15, OUT_UMAP15)

reduction_meta <- list(
  n_docs = n_docs,
  svd_dims = SVD_DIMS,
  singular_values = svd_fit$d,
  seed = SEED,
  umap_params = list(n_neighbors = 30, metric = "cosine")
)
saveRDS(reduction_meta, OUT_META)

cat("\n=========== Reduction summary ===========\n")
cat("Saved:\n  ", OUT_SVD50, "\n", sep = "")
cat("  ", OUT_UMAP2, "\n", sep = "")
cat("  ", OUT_UMAP15, "\n", sep = "")
cat("  ", OUT_META, "\n", sep = "")




needed <- c("uwot", "cluster", "dbscan", "Matrix", "dplyr", "readr", "tibble")
to_install <- setdiff(needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

suppressPackageStartupMessages({
  library(cluster)
  library(dbscan)
  library(Matrix)
  library(dplyr)
  library(readr)
  library(tibble)
})

PROJECT_DIR <- "C:/Users/Mashrur/Documents/human_ai_clustering"
IN_UMAP15   <- file.path(PROJECT_DIR, "hai_umap15.rds")
IN_SVD50    <- file.path(PROJECT_DIR, "hai_svd50.rds")
IN_TFIDF    <- file.path(PROJECT_DIR, "hai_tfidf.rds")
IN_META     <- file.path(PROJECT_DIR, "hai_doc_meta.rds")

OUT_CLUSTERS <- file.path(PROJECT_DIR, "hai_clusters.rds")
OUT_EVAL     <- file.path(PROJECT_DIR, "cluster_evaluation.csv")
OUT_TOPTERMS <- file.path(PROJECT_DIR, "cluster_top_terms.csv")

SEED <- 42L
K_RANGE <- 6:20          
HDBSCAN_MIN_SIZE <- 50L  

davies_bouldin <- function(X, labels) {
  labs <- unique(labels)
  labs <- labs[labs >= 0]
  if (length(labs) < 2) return(NA_real_)
  k <- length(labs)
  centroids <- matrix(NA_real_, nrow = k, ncol = ncol(X))
  scatters  <- numeric(k)
  for (i in seq_along(labs)) {
    idx <- which(labels == labs[i])
    centroids[i, ] <- colMeans(X[idx, , drop = FALSE])
    scatters[i] <- mean(sqrt(rowSums((X[idx, , drop = FALSE] - centroids[i, ])^2)))
  }
  db <- 0
  for (i in seq_len(k)) {
    max_ratio <- 0
    for (j in seq_len(k)) {
      if (i == j) next
      d_ij <- sqrt(sum((centroids[i, ] - centroids[j, ])^2))
      ratio <- (scatters[i] + scatters[j]) / d_ij
      if (ratio > max_ratio) max_ratio <- ratio
    }
    db <- db + max_ratio
  }
  db / k
}

mean_silhouette_sample <- function(X, labels, n_sample = 5000L, seed = SEED) {
  keep <- labels > 0
  if (sum(keep) < 10) return(NA_real_)
  X <- X[keep, , drop = FALSE]
  labels <- labels[keep]
  labs <- unique(labels)
  if (length(labs) < 2) return(NA_real_)
  set.seed(seed)
  n <- nrow(X)
  idx <- if (n <= n_sample) seq_len(n) else sample.int(n, n_sample)
  d <- dist(X[idx, , drop = FALSE])
  sil <- silhouette(labels[idx], d)
  mean(sil[, "sil_width"])
}

top_terms_per_cluster <- function(X, labels, top_n = 15L) {
  labs <- sort(unique(labels[labels >= 0]))
  terms <- colnames(X)
  out <- vector("list", length(labs))
  for (cl in labs) {
    idx <- which(labels == cl)
    mu <- Matrix::colMeans(X[idx, , drop = FALSE])
    ord <- order(mu, decreasing = TRUE)[seq_len(min(top_n, length(mu)))]
    out[[as.character(cl)]] <- tibble(
      cluster = cl,
      rank = seq_along(ord),
      term = terms[ord],
      mean_tfidf = as.numeric(mu[ord])
    )
  }
  bind_rows(out)
}

cat("Loading embeddings + TF-IDF...\n")
X_umap <- readRDS(IN_UMAP15)
X_svd  <- readRDS(IN_SVD50)
X_tfidf <- readRDS(IN_TFIDF)
doc_meta <- readRDS(IN_META)

stopifnot(identical(rownames(X_umap), rownames(X_tfidf)))
n_docs <- nrow(X_umap)

cat("\nKMeans on UMAP-15D (k = ", min(K_RANGE), ":", max(K_RANGE), ")...\n", sep = "")
eval_rows <- list()

for (k in K_RANGE) {
  set.seed(SEED)
  km <- kmeans(X_umap, centers = k, nstart = 10, iter.max = 100)
  sil <- mean_silhouette_sample(X_umap, km$cluster)
  db  <- davies_bouldin(X_umap, km$cluster)
  eval_rows[[length(eval_rows) + 1L]] <- tibble(
    method = "kmeans_umap15",
    k = k,
    n_clusters = k,
    silhouette = sil,
    davies_bouldin = db,
    params = paste0("nstart=10")
  )
  cat(sprintf("  k=%2d  silhouette=%.4f  DB=%.4f\n", k, sil, db))
}

eval_df <- bind_rows(eval_rows)
best_k_row <- eval_df |> slice_max(silhouette, n = 1, with_ties = FALSE)
best_k <- best_k_row$k
cat("\nBest k by silhouette (sampled): ", best_k, "\n", sep = "")

set.seed(SEED)
km_best <- kmeans(X_umap, centers = best_k, nstart = 25, iter.max = 100)
labels_kmeans <- km_best$cluster

cat("\nHDBSCAN on UMAP-15D (minPts = ", HDBSCAN_MIN_SIZE, ")...\n", sep = "")
hdb <- hdbscan(X_umap, minPts = HDBSCAN_MIN_SIZE)
labels_hdb <- hdb$cluster
n_hdb_clusters <- length(unique(labels_hdb[labels_hdb > 0]))
n_noise <- sum(labels_hdb == 0)
cat("  clusters: ", n_hdb_clusters, " | noise points: ",
    format(n_noise, big.mark = ","), " (",
    round(100 * n_noise / n_docs, 1), "%)\n", sep = "")

sil_hdb <- mean_silhouette_sample(X_umap, labels_hdb)
db_hdb  <- davies_bouldin(X_umap, labels_hdb)
eval_df <- bind_rows(
  eval_df,
  tibble(
    method = "hdbscan_umap15",
    k = NA_integer_,
    n_clusters = n_hdb_clusters,
    silhouette = sil_hdb,
    davies_bouldin = db_hdb,
    params = paste0("minPts=", HDBSCAN_MIN_SIZE)
  )
)

cat("\nPrimary labels: KMeans k = ", best_k, "\n", sep = "")
primary_labels <- labels_kmeans

cat("Computing top TF-IDF terms per cluster...\n")
top_terms <- top_terms_per_cluster(X_tfidf, primary_labels, top_n = 20L)
write_csv(top_terms, OUT_TOPTERMS)

cluster_result <- list(
  labels_kmeans = labels_kmeans,
  labels_hdbscan = labels_hdb,
  kmeans_k = best_k,
  kmeans_model = km_best,
  hdbscan_model = hdb,
  primary_method = "kmeans_umap15",
  primary_labels = primary_labels,
  doc_meta = doc_meta |> mutate(cluster = primary_labels)
)

saveRDS(cluster_result, OUT_CLUSTERS)
write_csv(eval_df, OUT_EVAL)

cat("\n=========== Clustering summary ===========\n")
cat("Saved: ", OUT_CLUSTERS, "\n", sep = "")
cat("       ", OUT_EVAL, "\n", sep = "")
cat("       ", OUT_TOPTERMS, "\n", sep = "")

cat("\nCluster sizes (KMeans k=", best_k, "):\n", sep = "")
print(sort(table(primary_labels), decreasing = TRUE))

cat("\nTop 5 terms per cluster (preview):\n")
top_terms |>
  group_by(cluster) |>
  slice_head(n = 5) |>
  summarise(keywords = paste(term, collapse = ", "), .groups = "drop") |>
  print(n = Inf)



needed <- c("ggplot2", "dplyr", "readr", "scales")
to_install <- setdiff(needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(scales)
})

PROJECT_DIR <- "C:/Users/Mashrur/Documents/human_ai_clustering"
IN_UMAP2    <- file.path(PROJECT_DIR, "hai_umap2.rds")
IN_CLUSTERS <- file.path(PROJECT_DIR, "hai_clusters.rds")
OUT_PNG1    <- file.path(PROJECT_DIR, "umap_clusters.png")
OUT_PNG2    <- file.path(PROJECT_DIR, "umap_by_year.png")

umap2 <- readRDS(IN_UMAP2)
cl    <- readRDS(IN_CLUSTERS)

plot_df <- cl$doc_meta |>
  mutate(
    UMAP1 = umap2[id, 1],
    UMAP2 = umap2[id, 2],
    cluster = factor(cluster)
  )

# ---- Main cluster plot --------------------------------------------------------
p1 <- ggplot(plot_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
  geom_point(size = 0.25, alpha = 0.5) +
  scale_color_viridis_d(option = "turbo", name = "Cluster") +
  labs(
    title = "Human–AI arXiv corpus (cs.HC + cs.AI)",
    subtitle = paste0("KMeans on UMAP-15D, k = ", cl$kmeans_k,
                      " | n = ", format(nrow(plot_df), big.mark = ",")),
    x = "UMAP 1", y = "UMAP 2"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right")

ggsave(OUT_PNG1, p1, width = 10, height = 7, dpi = 150)
cat("Saved: ", OUT_PNG1, "\n", sep = "")

plot_df <- plot_df |>
  mutate(year_bin = cut(year, breaks = c(2014, 2018, 2020, 2022, 2024, 2027),
                        labels = c("2015-18", "2019-20", "2021-22", "2023-24", "2025+"),
                        include.lowest = TRUE))

p2 <- ggplot(plot_df, aes(x = UMAP1, y = UMAP2)) +
  geom_point(size = 0.15, alpha = 0.15, color = "grey40") +
  geom_point(data = function(d) dplyr::filter(d, !is.na(year_bin)),
             aes(color = year_bin), size = 0.3, alpha = 0.4) +
  scale_color_viridis_d(option = "plasma", name = "Year") +
  facet_wrap(~ year_bin, ncol = 3) +
  labs(title = "Corpus layout by publication period", x = "UMAP 1", y = "UMAP 2") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")

ggsave(OUT_PNG2, p2, width = 12, height = 8, dpi = 150)
cat("Saved: ", OUT_PNG2, "\n", sep = "")

cat("\nOpen the PNG files from your project folder to view the maps.\n")



needed <- c("ggplot2", "dplyr", "readr", "Matrix", "forcats", "stringr")
to_install <- setdiff(needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(Matrix)
  library(forcats)
  library(stringr)
})

PROJECT_DIR <- "C:/Users/Mashrur/Documents/human_ai_clustering"
IN_TFIDF    <- file.path(PROJECT_DIR, "hai_tfidf.rds")
IN_CLUSTERS <- file.path(PROJECT_DIR, "hai_clusters.rds")
IN_TOPTERMS <- file.path(PROJECT_DIR, "cluster_top_terms.csv")

OUT_FREQ    <- file.path(PROJECT_DIR, "top_words_overall.png")
OUT_TFIDF   <- file.path(PROJECT_DIR, "top_words_tfidf.png")
OUT_CLUSTER <- file.path(PROJECT_DIR, "top_words_by_cluster.png")
OUT_CSV     <- file.path(PROJECT_DIR, "top_words_summary.csv")

TOP_N <- 30L
TOP_PER_CLUSTER <- 8L

label_term <- function(x) {
  dplyr::recode(
    x,
    "datum" = "data",
    "learn" = "learning",
    .default = x
  )
}

cat("Loading TF-IDF matrix...\n")
X <- readRDS(IN_TFIDF)
terms <- colnames(X)
n_docs <- nrow(X)

doc_freq   <- Matrix::colSums(X > 0)
tfidf_mass <- Matrix::colSums(X)

term_stats <- tibble(
  term = terms,
  term_label = label_term(terms),
  doc_freq = as.integer(doc_freq),
  doc_pct = 100 * doc_freq / n_docs,
  tfidf_mass = as.numeric(tfidf_mass)
) |>
  filter(!is.na(term_label), nzchar(term_label))

term_stats <- term_stats |>
  group_by(term_label) |>
  summarise(
    doc_freq = sum(doc_freq),
    tfidf_mass = sum(tfidf_mass),
    doc_pct = sum(doc_pct),
    .groups = "drop"
  )

write_csv(term_stats |> arrange(desc(doc_freq)), OUT_CSV)
cat("Saved summary table: ", OUT_CSV, "\n", sep = "")

plot_top_bars <- function(df, x_col, title, subtitle, xlab, out_path) {
  p <- ggplot(df, aes(x = .data[[x_col]], y = term_label, fill = .data[[x_col]])) +
    geom_col(show.legend = FALSE) +
    scale_fill_viridis_c(option = "mako", direction = -1) +
    labs(title = title, subtitle = subtitle, x = xlab, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank()
    )
  
  ggsave(out_path, p, width = 9, height = 8, dpi = 150, bg = "white")
  cat("Saved: ", out_path, "\n", sep = "")
}

top_freq <- term_stats |>
  slice_max(doc_freq, n = TOP_N, with_ties = FALSE) |>
  mutate(term_label = fct_reorder(term_label, doc_freq, .na_rm = TRUE))

plot_top_bars(
  top_freq,
  x_col = "doc_freq",
  title = "Most common words in the Human–AI arXiv corpus",
  subtitle = paste0(
    "Top ", TOP_N, " terms by number of abstracts containing the word (n = ",
    format(n_docs, big.mark = ","), " documents)"
  ),
  xlab = "Number of documents containing the term",
  out_path = OUT_FREQ
)

top_tfidf <- term_stats |>
  slice_max(tfidf_mass, n = TOP_N, with_ties = FALSE) |>
  mutate(term_label = fct_reorder(term_label, tfidf_mass, .na_rm = TRUE))

plot_top_bars(
  top_tfidf,
  x_col = "tfidf_mass",
  title = "Most salient words (total TF-IDF weight)",
  subtitle = paste0(
    "Top ", TOP_N, " terms summed across all documents — emphasizes distinctive usage"
  ),
  xlab = "Sum of TF-IDF scores across corpus",
  out_path = OUT_TFIDF
)

if (file.exists(IN_CLUSTERS) && file.exists(IN_TOPTERMS)) {
  cl <- readRDS(IN_CLUSTERS)
  top_cl <- read_csv(IN_TOPTERMS, show_col_types = FALSE) |>
    mutate(term_label = label_term(term)) |>
    filter(!is.na(term_label)) |>
    group_by(cluster, term_label) |>
    summarise(mean_tfidf = max(mean_tfidf), .groups = "drop") |>
    group_by(cluster) |>
    slice_max(mean_tfidf, n = TOP_PER_CLUSTER, with_ties = FALSE) |>
    ungroup() |>
    mutate(cluster = factor(cluster))
  
  theme_map <- c(
    "1" = "Games", "2" = "ML / interpretability", "3" = "EEG / BCI",
    "4" = "LLMs & agents", "5" = "Emotion / speech", "6" = "LLM reasoning",
    "7" = "Fairness & decisions", "8" = "UX & design tools", "9" = "Visualization",
    "10" = "Autonomous driving", "11" = "Social & privacy", "12" = "RL agents",
    "13" = "Gaze & haptics", "14" = "Education", "15" = "Crowdsourcing / HITL",
    "16" = "VR / immersive", "17" = "XAI explanations", "18" = "Clinical AI",
    "19" = "Sensors & gestures", "20" = "Robotics / HRI"
  )
  top_cl <- top_cl |>
    mutate(
      cluster_label = paste0("C", cluster, ": ", theme_map[as.character(cluster)])
    )
  
  p3 <- ggplot(top_cl, aes(x = mean_tfidf, y = fct_reorder(term_label, mean_tfidf, .na_rm = TRUE),
                           fill = mean_tfidf)) +
    geom_col(show.legend = FALSE) +
    scale_fill_viridis_c(option = "turbo", direction = -1) +
    facet_wrap(~ cluster_label, scales = "free_y", ncol = 4) +
    labs(
      title = "Top words per research theme (cluster)",
      subtitle = paste0("Top ", TOP_PER_CLUSTER, " TF-IDF terms within each KMeans cluster (k = ", cl$kmeans_k, ")"),
      x = "Mean TF-IDF in cluster",
      y = NULL
    ) +
    theme_minimal(base_size = 8) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      strip.text = element_text(size = 7)
    )
  
  ggsave(OUT_CLUSTER, p3, width = 14, height = 12, dpi = 150, bg = "white")
  cat("Saved: ", OUT_CLUSTER, "\n", sep = "")
}

cat("\nDone. Open these PNG files in your project folder:\n")
cat("  ", OUT_FREQ, "   <-- best for 'words mostly found'\n", sep = "")
cat("  ", OUT_TFIDF, "\n", sep = "")
if (file.exists(OUT_CLUSTER)) cat("  ", OUT_CLUSTER, "\n", sep = "")


needed <- c("ggplot2", "dplyr", "readr", "tidyr", "stringr", "tibble", "forcats", "scales")
to_install <- setdiff(needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(forcats)
  library(scales)
})

PROJECT_DIR <- "C:/Users/Mashrur/Documents/human_ai_clustering"
IN_EVAL     <- file.path(PROJECT_DIR, "cluster_evaluation.csv")
IN_TOP      <- file.path(PROJECT_DIR, "cluster_top_terms.csv")
IN_CLUSTERS <- file.path(PROJECT_DIR, "hai_clusters.rds")
OUT_PLOT    <- file.path(PROJECT_DIR, "eval_silhouette_db.png")
OUT_THEMES  <- file.path(PROJECT_DIR, "hai_theme_summary.csv")
OUT_CLUSTER <- file.path(PROJECT_DIR, "hai_theme_by_cluster.csv")
OUT_REPORT  <- file.path(PROJECT_DIR, "evaluation_report.txt")

THEME_LEXICON <- list(
  trust = c(
    "trust", "trustworthy", "reliability", "reliable", "confidence",
    "calibration", "transparency", "accountability", "fairness", "bias",
    "privacy", "ethical", "ethics", "responsible"
  ),
  usability = c(
    "usability", "usable", "user", "ux", "interface", "design",
    "experience", "accessibility", "workflow", "tool", "software",
    "cognitive", "workload", "satisfaction", "adoption"
  ),
  collaboration = c(
    "collaboration", "collaborative", "team", "teaming", "partner",
    "cooperation", "shared", "multiplayer", "human", "interaction",
    "communication", "dialogue", "conversation", "social"
  ),
  explainability = c(
    "explainability", "explainable", "interpretability", "interpretable",
    "xai", "explanation", "transparent", "rationale", "attribution",
    "shap", "lime", "saliency"
  ),
  human_in_the_loop = c(
    "human_in_the_loop", "human-in-the-loop", "crowdsourcing", "crowd",
    "annotation", "label", "labeling", "feedback", "oversight",
    "decision", "decision_support", "hitl", "worker"
  )
)

CLUSTER_LABELS <- c(
  "1"  = "Games & player experience",
  "2"  = "ML interpretability / neural models",
  "3"  = "EEG & brain-computer interfaces",
  "4"  = "LLMs & conversational agents",
  "5"  = "Affective computing (emotion/speech)",
  "6"  = "LLM reasoning & language models",
  "7"  = "Fairness, decisions & human oversight",
  "8"  = "UX, design tools & creative systems",
  "9"  = "Visual analytics & data visualization",
  "10" = "Autonomous vehicles & driving",
  "11" = "Social media, privacy & online platforms",
  "12" = "Reinforcement-learning agents",
  "13" = "Gaze, haptics & multimodal displays",
  "14" = "Education & learning technologies",
  "15" = "Crowdsourcing & data labeling (HITL)",
  "16" = "Virtual / immersive environments",
  "17" = "XAI for explanations & decisions",
  "18" = "Clinical & medical AI",
  "19" = "Wearable sensing & activity recognition",
  "20" = "Human-robot interaction (HRI)"
)

CLUSTER_HAI_FOCUS <- c(
  "1"  = "usability; collaboration",
  "2"  = "explainability",
  "3"  = "usability",
  "4"  = "collaboration; usability",
  "5"  = "usability",
  "6"  = "explainability; collaboration",
  "7"  = "trust; human-in-the-loop",
  "8"  = "usability; collaboration",
  "9"  = "explainability; usability",
  "10" = "trust; human-in-the-loop",
  "11" = "trust; usability",
  "12" = "collaboration",
  "13" = "usability",
  "14" = "usability; collaboration",
  "15" = "human-in-the-loop; collaboration",
  "16" = "usability; collaboration",
  "17" = "explainability; trust; human-in-the-loop",
  "18" = "trust; human-in-the-loop; explainability",
  "19" = "usability",
  "20" = "collaboration; trust"
)

score_theme <- function(terms, lexicon) {
  hits <- sum(terms %in% lexicon)
  hits / length(terms)
}

eval_df <- read_csv(IN_EVAL, show_col_types = FALSE)
km_eval <- eval_df |> filter(method == "kmeans_umap15")

cl <- readRDS(IN_CLUSTERS)
chosen_k <- cl$kmeans_k

km_balanced <- km_eval |>
  filter(k >= 8, k <= 16) |>
  slice_min(davies_bouldin, n = 1, with_ties = FALSE)
suggested_k <- km_balanced$k

p_eval <- ggplot(km_eval, aes(x = k)) +
  geom_line(aes(y = silhouette, color = "Silhouette"), linewidth = 1) +
  geom_point(aes(y = silhouette, color = "Silhouette"), size = 2) +
  geom_line(aes(y = scales::rescale(davies_bouldin, to = c(0, 1)),
                color = "Davies-Bouldin (rescaled)"), linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = scales::rescale(davies_bouldin, to = c(0, 1)),
                 color = "Davies-Bouldin (rescaled)"), size = 2) +
  geom_vline(xintercept = chosen_k, linetype = "dotted", color = "red") +
  annotate("text", x = chosen_k, y = 0.05, hjust = -0.1, size = 3,
           label = paste0("chosen k = ", chosen_k), color = "red") +
  geom_vline(xintercept = suggested_k, linetype = "dotted", color = "darkgreen") +
  annotate("text", x = suggested_k, y = 0.12, hjust = -0.1, size = 3,
           label = paste0("suggested k = ", suggested_k, " (best DB)"), color = "darkgreen") +
  scale_color_manual(
    name = NULL,
    values = c("Silhouette" = "#2196F3", "Davies-Bouldin (rescaled)" = "#FF9800")
  ) +
  scale_x_continuous(breaks = km_eval$k) +
  labs(
    title = "Cluster quality vs. number of clusters (KMeans on UMAP-15D)",
    subtitle = paste0(
      "Silhouette: higher is better (max at k=", chosen_k, ", score=",
      round(km_eval$silhouette[km_eval$k == chosen_k], 3),
      "). Davies-Bouldin: lower is better (best in k=8–16 at k=", suggested_k, ", DB=",
      round(km_balanced$davies_bouldin, 3), ")."
    ),
    x = "Number of clusters (k)",
    y = "Score (Silhouette on 0–1 scale; DB rescaled to 0–1 for comparison)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(OUT_PLOT, p_eval, width = 10, height = 6, dpi = 150, bg = "white")
cat("Saved: ", OUT_PLOT, "\n", sep = "")

OUT_DB <- file.path(PROJECT_DIR, "eval_davies_bouldin.png")
p_db <- ggplot(km_eval, aes(k, davies_bouldin)) +
  geom_line(color = "#FF9800", linewidth = 1) +
  geom_point(color = "#FF9800", size = 2) +
  labs(
    title = "Davies-Bouldin Index vs. k (lower is better)",
    x = "k", y = "Davies-Bouldin Index"
  ) +
  theme_minimal()
ggsave(OUT_DB, p_db, width = 8, height = 5, dpi = 150, bg = "white")

top_terms <- read_csv(IN_TOP, show_col_types = FALSE)

theme_scores <- top_terms |>
  group_by(cluster) |>
  summarise(
    top_terms = paste(term[rank <= 15], collapse = ", "),
    trust = score_theme(term, THEME_LEXICON$trust),
    usability = score_theme(term, THEME_LEXICON$usability),
    collaboration = score_theme(term, THEME_LEXICON$collaboration),
    explainability = score_theme(term, THEME_LEXICON$explainability),
    human_in_the_loop = score_theme(term, THEME_LEXICON$human_in_the_loop),
    .groups = "drop"
  ) |>
  mutate(
    cluster_label = CLUSTER_LABELS[as.character(cluster)],
    hai_focus = CLUSTER_HAI_FOCUS[as.character(cluster)],
    primary_theme = {
      m <- cbind(trust, usability, collaboration, explainability, human_in_the_loop)
      nm <- c("trust", "usability", "collaboration", "explainability", "human_in_the_loop")
      nm[max.col(m, ties.method = "first")]
    }
  )

sizes <- as.integer(table(cl$primary_labels))
size_lookup <- setNames(sizes, names(table(cl$primary_labels)))
cluster_summary <- theme_scores |>
  mutate(
    n_docs = size_lookup[as.character(cluster)],
    pct_corpus = round(100 * n_docs / sum(sizes, na.rm = TRUE), 1)
  ) |>
  filter(!is.na(n_docs)) |>
  arrange(desc(n_docs))

write_csv(cluster_summary, OUT_CLUSTER)
cat("Saved: ", OUT_CLUSTER, "\n", sep = "")

theme_corpus <- cluster_summary |>
  summarise(
    trust = weighted.mean(trust, n_docs, na.rm = TRUE),
    usability = weighted.mean(usability, n_docs, na.rm = TRUE),
    collaboration = weighted.mean(collaboration, n_docs, na.rm = TRUE),
    explainability = weighted.mean(explainability, n_docs, na.rm = TRUE),
    human_in_the_loop = weighted.mean(human_in_the_loop, n_docs, na.rm = TRUE)
  ) |>
  pivot_longer(everything(), names_to = "theme", values_to = "score") |>
  mutate(
    theme = str_replace_all(theme, "_", " ") |> str_to_title(),
    theme = fct_reorder(theme, score, .na_rm = TRUE)
  ) |>
  filter(!is.na(score), !is.na(theme))

write_csv(theme_corpus |> select(theme, score), OUT_THEMES)

OUT_THEME_BAR <- file.path(PROJECT_DIR, "hai_dominant_themes.png")
p_theme <- ggplot(theme_corpus, aes(x = theme, y = score, fill = score)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  coord_flip() +
  labs(
    title = "Dominant Human-AI interaction themes in the corpus",
    subtitle = "Theme scores = share of top cluster keywords matching each theme (weighted by cluster size)",
    x = NULL, y = "Weighted theme score"
  ) +
  theme_minimal(base_size = 12)
ggsave(OUT_THEME_BAR, p_theme, width = 9, height = 5, dpi = 150, bg = "white")
cat("Saved: ", OUT_THEME_BAR, "\n", sep = "")

hdb <- eval_df |> filter(method == "hdbscan_umap15")

report_lines <- c(
  "=================================================================",
  "Human-AI Interaction Discovery — Clustering Evaluation Report",
  "=================================================================",
  "",
  "DATA",
  paste0("  Documents clustered: ", format(sum(sizes), big.mark = ",")),
  paste0("  Features: TF-IDF (12,237 terms) -> SVD-50 -> UMAP-15 -> KMeans"),
  "",
  "METHOD",
  "  Unsupervised document clustering (KMeans) on UMAP-reduced embeddings.",
  "  Top keywords per cluster = term grouping for thematic interpretation.",
  "",
  "EVALUATION METRICS (KMeans, k = 6 to 20)",
  "  Silhouette score: measures how similar a document is to its own cluster",
  "    vs. other clusters. Range [-1, 1]; higher is better.",
  paste0("    At chosen k=", chosen_k, ": ", round(km_eval$silhouette[km_eval$k == chosen_k], 4)),
  paste0("    Suggested k=", suggested_k, " (best Davies-Bouldin in k=8-16): ",
         round(km_eval$silhouette[km_eval$k == suggested_k], 4)),
  "",
  "  Davies-Bouldin Index: average similarity between each cluster and the",
  "    cluster most similar to it. Lower is better.",
  paste0("    At k=", chosen_k, ": ", round(km_eval$davies_bouldin[km_eval$k == chosen_k], 4)),
  paste0("    At k=", suggested_k, " (best in range): ",
         round(km_eval$davies_bouldin[km_eval$k == suggested_k], 4)),
  "",
  "  Note: Silhouette rose monotonically with k; Davies-Bouldin worsened after k~12.",
  "  For reporting, discuss k=12 (parsimonious) AND k=20 (max silhouette).",
  "",
  "HDBSCAN (comparison)",
  paste0("  Clusters found: ", hdb$n_clusters, " | Silhouette: ", round(hdb$silhouette, 4),
         " | DB: ", round(hdb$davies_bouldin, 4)),
  "  39.5% of documents labeled as noise -> less suitable as primary solution.",
  "",
  "DOMINANT HUMAN-AI THEMES (from cluster keywords)",
  paste0("  ", paste(sprintf("%s: %.2f", theme_corpus$theme, theme_corpus$score), collapse = " | ")),
  "",
  "LARGEST CLUSTERS (document groups)",
  paste0("  C", cluster_summary$cluster[1:5], " (", cluster_summary$cluster_label[1:5], "): ",
         cluster_summary$n_docs[1:5], " docs each"),
  "",
  "KEY INTERACTION PATTERNS FOR STUDENTS",
  "  Trust:           clusters 7, 11, 17, 18, 20 (fairness, privacy, XAI, clinical, HRI)",
  "  Usability:       clusters 8, 1, 13, 14, 16 (design, games, gaze, education, VR)",
  "  Collaboration:   clusters 4, 20, 12, 15, 1 (LLM agents, robots, RL, crowdsourcing)",
  "  Explainability:  clusters 17, 2, 9 (XAI, interpretable ML, visualization)",
  "  Human-in-loop:   cluster 15 (+ 7, 17, 18) (labeling, decisions, clinical AI)",
  "",
  "OUTPUT FILES",
  paste0("  ", OUT_PLOT),
  paste0("  ", OUT_THEME_BAR),
  paste0("  ", OUT_CLUSTER),
  "================================================================="
)

writeLines(report_lines, OUT_REPORT)
cat(paste(report_lines, collapse = "\n"), "\n")
cat("\nSaved: ", OUT_REPORT, "\n", sep = "")

