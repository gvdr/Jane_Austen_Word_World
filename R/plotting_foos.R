plot_traits <- function(word_space_df,Book,N){
  library(gridExtra)
  library(ggrepel)
  library(viridis)
  
  word_space_df %>%
  filter(book %in% Book) %>%
    anti_join(stop_words_personal, by = c("label" = "word")) %>%
    arrange(-magnitude) %>%
    slice(seq.int(1,N)) %>%
    ggplot(aes(x = trait.1, y = trait.2, colour = trait.3, label = label)) +
    geom_point() +
    ggrepel::geom_label_repel() +
    theme_minimal() +
    ggtitle(paste0(Book,"'s word universe")) +
    labs(x="x",y="y") +
    scale_color_viridis(discrete=F) +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank()) +
    geom_point(aes(x = 0, y = 0), colour = "red")
}


plot_traits_allbooks <- function(word_space_df,N){
  library(gridExtra)
  library(ggrepel)
  library(viridis)
  
  word_space_df %>%
    anti_join(stop_words_personal, by = c("label" = "word")) %>%
    group_by(book) %>%
    top_n(N,magnitude) %>%
    ungroup() %>%
    ggplot(aes(x = trait.1, y = trait.2, colour = trait.3, label = label)) +
    facet_wrap( ~ book) +
    geom_point() +
    ggrepel::geom_label_repel() +
    theme_minimal() +
    ggtitle("Jane Austen's word worlds") +
    labs(x="x",y="y") +
    scale_color_viridis(discrete=F) +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank()) +
    geom_point(aes(x = 0, y = 0), colour = "red")
}

plot_traits_allchapters <- function(word_space_df,Book,N){
  library(gridExtra)
  library(ggrepel)
  library(viridis)
  
  word_space_df %>%
    filter(book %in% Book) %>%
    anti_join(stop_words_personal, by = c("label" = "word")) %>%
    group_by(chapter) %>%
    top_n(N,magnitude) %>%
    ungroup() %>%
    ggplot(aes(x = trait.1, y = trait.2, colour = trait.3, label = label)) +
    facet_wrap( ~ chapter) +
    geom_point() +
    ggrepel::geom_label_repel() +
    theme_minimal() +
    ggtitle(paste0(Book,"'s word world by chapter")) +
    labs(x="x",y="y") +
    scale_color_viridis(discrete=F) +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank()) +
    geom_point(aes(x = 0, y = 0), colour = "red")
}

plot_euclidean_distance_words <- function(word_space_df,Word1,Word2,Book){
  library(gridExtra)
  library(ggrepel)
  library(viridis)
  
  word_space_df %>%
    mutate(distance_euclid = sqrt((trait.1.w1 - trait.1.w2)^2 +
                                     (trait.2.w1 - trait.2.w2)^2 +
                                     (trait.3.w1 - trait.3.w2)^2)
            ) %>%
    ggplot(aes(chapter,distance_euclid)) +
    geom_bar(stat = "identity", position = "identity", na.rm = T) +
    theme_minimal() +
    ggtitle(paste0(Book,"'s word world"))+
    labs(x="chapter",y=paste0("Euclidean distance ",Word1," <-> ",Word2)) 
}

plot_cosine_similarity_words <- function(word_space_df,Word1,Word2,Book){
  library(gridExtra)
  library(ggrepel)
  library(viridis)
  
  word_space_df %>%
    filter(book %in% Book)  -> data_to_plot
  
  full_join(data_to_plot %>%
              filter(label %in% Word1) %>%
              select(-label,-magnitude)
            ,
            data_to_plot %>%
              filter(label %in% Word2) %>%
              select(-label,-magnitude)
            ,
            by = c("book","chapter"),
            suffix = c(".w1",".w2")
  ) %>%
    mutate(similarity_cosine = ((trait.1.w1 * trait.1.w2) +
                                  (trait.2.w1 * trait.2.w2)+
                                  (trait.3.w1 * trait.3.w2)) /
             ((trait.1.w1^2 + trait.2.w1^2 + trait.3.w1^2) *
                (trait.1.w2^2 + trait.2.w2^2 + trait.3.w2^2))
    ) %>%
    ggplot(aes(chapter,similarity_cosine)) +
    geom_bar(stat = "identity", position = "identity", na.rm = T) +
    theme_minimal() +
    ggtitle(paste0(Book,"'s word world"))+
    labs(x="chapter",y=paste0("Cosine similarity ",Word1," <-> ",Word2)) 
}