###############################################################################
## Final Project Drafting Sheet

## Project Title: Recognizing the Development of Asian American Literature

## Track 1: Answer a Question/Tell a Story with an Existing Dataset

## Information about dataset: https://data.post45.org/posts/asian-american-literature/

###############################################################################

## Load libraries
library("stringr")
library("dplyr")
library("ggplot2")
library("plotly")
library("skimr")

## Open datasets and create dataframes
# Citations of Literary Works
AAL_df <- read.csv("https://raw.githubusercontent.com/Post45-Data-Collective/data/refs/heads/main/asian_american_literature/canon_asian_american_literature.csv", stringsAsFactors = FALSE)

# Unique Authors
AAL_author_df <- read.csv("https://raw.githubusercontent.com/Post45-Data-Collective/data/refs/heads/main/asian_american_literature/asian_american_authors.csv", stringsAsFactors = FALSE)

# Unique Titles
AAL_title_df <- read.csv("https://raw.githubusercontent.com/Post45-Data-Collective/data/refs/heads/main/asian_american_literature/asian_american_texts.csv", stringsAsFactors = FALSE)

## Skim Data
skim(AAL_df)
skim(AAL_author_df)
skim(AAL_title_df)

#######################################################################################
# 1. Which period of time was the most amount of Asian American literature published? How have publication patterns of Asian American literature changed over time? Which authors are productive with the most unique titles?

# 1.1 Years with the most titles
# Clean publication years. For a series across multiple years, extract the first publication year.
AAL_title_clean <- AAL_title_df %>%
  mutate(
    clean_year = str_extract(publication_year, "^\\d{4}"),
    clean_year = as.numeric(clean_year)
  ) %>% 
  filter(!is.na(clean_year))

# Calculate yearly publication counts
pub_by_year <- AAL_title_clean %>%
  group_by(clean_year) %>%
  summarize(num_titles = n()) %>%
  arrange(clean_year)

# Create an interactive line plot visualizing the Asian American literature by publication year.
num_pub_year <- 
  ggplot(data = pub_by_year, aes(x = clean_year, 
                                 y = num_titles, 
                                 group = 1,
                                 text = paste("Year:", clean_year, "\nTotal Number of Titles:", num_titles))) +
  geom_line() +
  labs(title = "Asian American Literature by Publication Year",
       x = "Publication Year",
       y = "Total Number of Titles")

ggplotly(num_pub_year, tooltip = "text") 

# 1.2 Authors with the most unique titles
# Calculate the top 10 authors. 
author_productivity <- AAL_title_df %>%
  group_by(author_name) %>%
  summarize(num_titles = n()) %>%
  arrange(desc(num_titles)) %>%
  slice_head(n = 10)

# Create an interactive bar plot visualizing the top 10 authors. 
num_author_title <- 
  ggplot(data = author_productivity) +
  geom_col(aes(x = num_titles, 
               y = reorder(author_name, num_titles), 
               text = paste("Author:", author_name, "\nTotal Number of Titles:", num_titles)), 
           fill = "steelblue") +
  labs(
    title = "Top 10 Authors in Asian American Literature",
    x = "Total Number of Titles",
    y = "Author"
  )

ggplotly(num_author_title, tooltip = "text") 

#######################################################################################
# 2. Among the authors represented in the AAL_author_df, which ethnic groups are the most prominent? What are the top 10 ethnic groups with the most authors? 

# Calculate the number of unique authors per ethnic group.
ethnic_grps <- AAL_author_df %>%
  group_by(ethnicity_national_origin_group) %>%
  summarize(number_of_authors = n())

# Convert the data frame to csv file to delete the clear empty cell. 
write.csv(ethnic_grps, "~/ethnic_grps.csv")

# Read and load the new data frame. Use your own file path. 
clean_ethnic_grps <- read.csv("clean_ethnic_grps.csv")

# Calculate the top 10 ethnic groups represented in the Unique Authors data set.
top_10_grps <- clean_ethnic_grps %>%
  slice_max(n = 10,
            order_by = number_of_authors)

# Create a data visualization of the top 10 ethnic groups and how many authors represent each group. (Made an additional visualization with Data Wrapper for this plot.) 
top_10_ethnic_grps_interactive <- 
  ggplot(data = top_10_grps) +
  geom_col(mapping = aes(x = number_of_authors, 
                         y = reorder(ethnicity_national_origin_group, +number_of_authors), 
                         fill = ethnicity_national_origin_group, 
                         text = paste("Ethnic Group:", ethnicity_national_origin_group, "\nTotal Number of Authors:", number_of_authors))) + 
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Top 10 Ethnicities of Authors in Asian American Literature", 
    x = "Total Number of Authors", 
    y = "Ethnic Group",
    fill = "Ethnic Group")

ggplotly(top_10_ethnic_grps_interactive, tooltip = "text") 

#######################################################################################
# 3. Which genres of Asian American literature are the most prominent? What are the top 5 genres with the most titles? What are the genres of Asian American literature published by Cambodian and Korean Americans?  

# 3.1 Top 5 Genres in Asian American Literature
# Calculate how many titles in each genre.  
genre_df <- AAL_title_clean %>%
  group_by(genre) %>% 
  summarize(Total_Titles = n())

# Slice the top 5 genres with the most number of titles. 
top_genre <- genre_df %>%  
  slice_max(n = 5, order_by = Total_Titles)

# Make a vector and filter only the top 5 genres.
top_genre_vector <- top_genre$genre

top_genre_year <- genre_df %>% 
  filter(genre %in% top_genre_vector) 

# Create an interactive bar plot visualizing the top 5 genres. 
top_genre_bar <- 
  ggplot(data = top_genre_year) +
  geom_col(mapping = aes(x = Total_Titles, 
                         y = reorder(genre, +Total_Titles), 
                         fill = genre, 
                         text = paste("Genre:", genre, "\nTotal Number of Titles:", Total_Titles))) + 
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Top 5 Genres in Asian American Literature", 
    x = "Total Number of Titles", 
    y = "Genre",
    fill = "Genre"
  ) 

ggplotly(top_genre_bar, tooltip = "text")

# 3.2 Genres of literature authored by Cambodian and Korean American Authors
# Group by genre, ethnic group, author, and title. 
ethnic_genre <- AAL_df %>%
  group_by(genre, ethnicity_national_origin_group, author_name, title) %>% 
  summarize(Total_Titles = n())

# Make a vector and filter only Cambodian and Korean American. 
kor_cam_vector <- c("Korean", "Cambodian")

kor_cam_df <- ethnic_genre %>% 
  filter(ethnicity_national_origin_group %in% kor_cam_vector)

# Download the "kor_cam_df" csv file. 
write.csv(kor_cam_df, "~/kor_cam_df_1.csv")

# Manually revise the dataset to remove duplicate rows caused by repeated authors or missing genre and title information. Use your own file path. 
kor_cam_df <- read.csv("kor_cam_df_1_edited.csv", stringsAsFactors = FALSE)

# Group by genre, and ethnic group
kor_cam_genre <- kor_cam_df %>%
  group_by(genre, ethnicity_national_origin_group) %>% 
  summarize(Total_Titles = sum(num_titles))

# Create an interactive bar plot visualizing the genres of literature authored by Cambodian and Korean. 
kor_cam_plot <- 
  ggplot(data = kor_cam_genre) +
  geom_bar(mapping = aes(x = ethnicity_national_origin_group, 
                         y = Total_Titles, 
                         fill = genre, 
                         text = paste("Genre:", genre, "\nTotal Number of Titles:", Total_Titles)),
           stat ="identity", position ="dodge") +
  labs(
    title = "Genres of Cambodian and Korean American Authors' Literature", 
    x = "Ethnic Group", 
    y = "Total Number of Titles",
    fill = "Genre"
  ) 

ggplotly(kor_cam_plot, tooltip = "text")
