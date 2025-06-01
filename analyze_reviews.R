library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(tm)          
library(textstem)    
library(tidytext)    
library(lubridate)   
library(broom)       
library(stargazer)   
library(syuzhet)     
library(quanteda)    
library(car)         
library(glmnet)


# Set the path to the folder that contains the review CSV files
base_dir <- "."  # Current folder (Emotion_Analysis)
data_dir <- file.path(base_dir, "datasets")
review_folder <- file.path(data_dir, "2_reviews_per_movie_raw")
budget_csv <- file.path(data_dir, "Mojo_budget_update.csv")
output_csv <- file.path(data_dir, "movie_level_data_8weeks_final.csv")

# Load budget data
data_budget <- read_csv(budget_csv)

# Load the NRC emotion lexicon
nrc_lexicon <- get_sentiments("nrc")

# Rename the 'title' column to 'movie_title' so it matches the review data
data_budget <- data_budget %>%
  rename(movie_title = title) %>%
  filter(year >= 2000 & year <= 2019)

movie_name_counts <- list()

# Function to read a single review file and standardize column names
read_review_file <- function(file) {
  movie_name <- str_remove(basename(file), "\\.csv$")        # Remove ".csv"
  movie_name <- str_remove(movie_name, "\\s\\d{4}$")         # Remove year at the end
  
  # Increment the count for this movie name
  if (!movie_name %in% names(movie_name_counts)) {
    movie_name_counts[[movie_name]] <<- 1
  } else {
    movie_name_counts[[movie_name]] <<- movie_name_counts[[movie_name]] + 1
    movie_name <- paste0(movie_name, " ", movie_name_counts[[movie_name]])
  }
  
  df <- read_csv(file) %>% 
    mutate(movie_title = movie_name) %>%
    rename(review_date = date, review_text = review)         # Make sure columns are named consistently
  return(df)
}



# Load all review CSVs and combine them into one dataset
review_files <- list.files(review_folder, pattern = "*.csv", full.names = TRUE)
review_data <- map_df(review_files, read_review_file)

# Merge the reviews with the budget data using movie title
merged_data <- inner_join(data_budget, review_data, by = "movie_title")

merged_data <- merged_data %>%
  mutate(
    review_date = dmy(review_date),
    release_date = mdy(paste(release_date, year)),
    days_since_release = as.numeric(review_date - release_date),
    review_week = case_when(
      days_since_release < 0                              ~ "0",
      days_since_release >= 0  & days_since_release <= 6  ~ "1",
      days_since_release >= 7  & days_since_release <= 13 ~ "2",
      days_since_release >= 14 & days_since_release <= 20 ~ "3",
      days_since_release >= 21 & days_since_release <= 27 ~ "4",
      days_since_release >= 28 & days_since_release <= 34 ~ "5",
      days_since_release >= 35 & days_since_release <= 41 ~ "6",
      days_since_release >= 42 & days_since_release <= 48 ~ "7",
      days_since_release >= 49 & days_since_release <= 55 ~ "8",
      days_since_release > 55                            ~ "9+",
      TRUE ~ NA_character_
    )
  )




# Fix formatting issue with "Sci-Fi" genre name
merged_data <- merged_data %>%
  mutate(across(starts_with("genre_"), ~ ifelse(.x == "Sci-Fi", "SciFi", .x)))

# Function to clean and lemmatize the review text
preprocess_text <- function(text) {
  text %>%
    tolower() %>%
    removePunctuation() %>%
    removeNumbers() %>%
    removeWords(stopwords("en")) %>%
    lemmatize_strings()
}

# Apply text cleaning to each review
merged_data <- merged_data %>% 
  select(movie_title, genre_1, genre_2, genre_3, genre_4, review_text, worldwide, release_date, review_week, budget) %>% 
  mutate(cleaned_review = map_chr(review_text, preprocess_text))

# Calculate emotion scores using NRC lexicon
nrc_sentiment <- get_nrc_sentiment(merged_data$cleaned_review)

# Remove the 'anticipation' and 'trust' emotions from the analysis
nrc_sentiment <- nrc_sentiment %>%
  select(-anticipation, -trust)

# Combine emotion scores back into the dataset
merged_data <- bind_cols(merged_data, nrc_sentiment)

# Count total unique words in the NRC lexicon (excluding 'trust' and 'anticipation')
total_words <- nrc_lexicon %>%
  filter(!sentiment %in% c("trust", "anticipation")) %>%
  distinct(word) %>%
  nrow()

# Count how many unique words belong to each remaining emotion
emotion_counts <- nrc_lexicon %>%
  filter(!sentiment %in% c("trust", "anticipation")) %>%
  group_by(sentiment) %>%
  summarise(word_count = n_distinct(word), .groups = 'drop')

# Calculate the proportion of each emotion in the lexicon
emotion_proportions <- emotion_counts %>%
  mutate(proportion = word_count / total_words)

# Display proportions of each emotion category
emotion_proportions

# Aggregate sentiment and review data at the movie level
movie_level_data <- merged_data %>%
  group_by(movie_title, genre_1, genre_2, genre_3, genre_4, worldwide, review_week, budget, release_date) %>%
  summarise(across(anger:positive, sum, na.rm = TRUE),           # Sum of each emotion for the movie
            n_reviews = n(),                                     # Total number of reviews
            n_review_words = sum(str_count(cleaned_review, "\\S+"), na.rm = TRUE)) %>%  # Total word count
  ungroup()

movie_level_data <- merged_data %>%
  filter(!is.na(review_week)) %>%
  group_by(movie_title, review_week) %>%
  summarise(
    genre_1 = first(genre_1),
    genre_2 = first(genre_2),
    genre_3 = first(genre_3),
    genre_4 = first(genre_4),
    worldwide = first(worldwide),
    budget = first(budget),
    across(anger:positive, sum, na.rm = TRUE),
    n_reviews = n(),
    n_review_words = sum(str_count(cleaned_review, "\\S+"), na.rm = TRUE),
    .groups = "drop"
  )



# Manually define the emotion proportions (based on NRC)
emotion_proportions <- c(fear = 0.246, 
                         anger = 0.208, 
                         sadness = 0.198, 
                         disgust = 0.177, 
                         joy = 0.115, 
                         surprise = 0.0889)

# Calculate emotion overrepresentation scores (relative to lexicon proportions)
movie_level_data <- movie_level_data %>%
  mutate(across(c(fear, anger, sadness, disgust, joy, surprise), 
                ~ ifelse(n_review_words > 0, 
                         (. / n_review_words) * 100 / emotion_proportions[cur_column()], 
                         NA)))  # Avoid division if word count is zero

# Divide worldwide revenue by 1 million
movie_level_data$worldwide <- movie_level_data$worldwide / 1e6
movie_level_data$budget <- movie_level_data$budget / 1e6
# Extract all unique genres from the four genre columns
all_genres <- unique(c(movie_level_data$genre_1, movie_level_data$genre_2, movie_level_data$genre_3, movie_level_data$genre_4))
all_genres <- all_genres[!is.na(all_genres)]  # Remove missing values

# Create dummy variables (0/1) for each genre
for (genre in all_genres) {
  movie_level_data[[genre]] <- apply(movie_level_data[, c("genre_1", "genre_2", "genre_3", "genre_4")], 1, function(x) as.integer(genre %in% x))
}

movie_level_data <- movie_level_data %>%
  filter(review_week %in% 0:8)

# Save final output
write_csv(movie_level_data, output_csv)

# List of genre dummy variables to include
genre_vars <- c("Action", "Adventure", "Animation", "Biography", "Comedy", "Crime", "Drama", 
                "Fantasy", "History", "Horror", "Music", "Mystery", "Romance", "SciFi", 
                "Sport", "Thriller", "War")

# Aggregate movie-level data including individual genres
movie_level_data_agg_genres <- movie_level_data %>%
  group_by(movie_title) %>%
  summarise(
    worldwide = first(worldwide),           # assumed constant per movie
    budget = first(budget),                 # assumed constant per movie
    joy = weighted.mean(joy, n_review_words, na.rm = TRUE),
    anger = weighted.mean(anger, n_review_words, na.rm = TRUE),
    fear = weighted.mean(fear, n_review_words, na.rm = TRUE),
    sadness = weighted.mean(sadness, n_review_words, na.rm = TRUE),
    disgust = weighted.mean(disgust, n_review_words, na.rm = TRUE),
    surprise = weighted.mean(surprise, n_review_words, na.rm = TRUE),
    across(all_of(genre_vars), ~ first(.))  # pick first occurrence of each genre dummy
  )


###### Elastic Net regression

# Create design matrix with interaction terms
x <- model.matrix(
  ~ joy * Comedy + fear * Horror + disgust * Horror + 
    sadness * Drama + anger * Crime + surprise * Thriller + budget, 
  data = movie_level_data_agg_genres
)[, -1]  # Remove intercept column

# Log-transformed outcome variable
y <- log(movie_level_data_agg_genres$worldwide)

set.seed(123)  # For reproducibility

# Cross-validate to find best lambda
cv_model <- cv.glmnet(x, y, alpha = 0.5)

# Best lambda value
best_lambda <- cv_model$lambda.min
best_lambda

# Fit final model at best lambda
final_model <- glmnet(x, y, alpha = 0.5, lambda = best_lambda)

# Coefficients
coef(final_model)


# Fit a combined model with interaction terms
final_regression_model <- lm(
  log(worldwide) ~ 
    joy + fear + sadness + Drama + 
    anger + Crime + surprise + Thriller + 
    budget +
    joy:Comedy + 
    Horror:disgust + 
    sadness:Drama,
  data = movie_level_data_agg_genres
)
summary(final_regression_model)




