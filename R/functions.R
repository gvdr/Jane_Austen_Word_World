ccv_to_mat <- function(ccv,force_symmetric = T){
  nodes1 <- ccv %$% item1 %>% unique()
  nodes2 <- ccv %$% item2 %>% unique()
  nodes <- union(nodes1,nodes2)
  len <- nodes %>% length()
  Mat <- matrix(0,nrow = len, ncol = len,
                dimnames = list(nodes,
                                nodes))
  Mat[cbind(ccv %$% item1,ccv %$% item2)] <- ccv %$% n
  if(force_symmetric){
    Mat <- pmax(Mat,t(Mat))
  }
  return(Mat)
}

slicer <- function(data,slice_n, sampling = F){
  if(sampling & is.numeric(slice_n) & slice_n < nrow(data)) {
    data %>%
      sample_n(slice_n) %>%
      return()
  } else if(is.numeric(slice_n) & slice_n < nrow(data)) {
    data %>%
      slice(1:slice_n) %>%
      return()
  } else {
    data %>%
      return()
  }
}

vaniller <- function(dirty_talk){
  dirty_talk %>% 
    str_to_lower() %>%
    str_replace_all("chapter \\d+","") %>%
    str_replace_all("(?<=\\w)\\'s","") %>%
    str_replace_all("[#$%()*,:<=>@^_\\-`'|~{}\"]", " ") %>%
    str_replace_all("(?<=\\s)m[sr]*[rs].", "") %>%
    str_trim("both") %>%
    return()
}

phraser <- function(clean_text_df, text_to_clean){
  clean_text_df %>%
    do(.[[text_to_clean]] %>% 
         str_split("[\\?\\.\\!\\;]") %>%
         unlist %>% 
         data_frame(sentence = .)) %>%
    ungroup() %>%
    mutate(sentence_number = 1:length(sentence)) %>%
    nest(sentence) %>%
    return()
}

wordr <- function(clean_text_df, stemming = T){
    clean_text_df %>%
    mutate(split_word = map(data, ~.x$sentence %>%
                              wordr_internal(stemming = stemming)
                            )
    ) %>%
    select(-data) %>%
    unnest(split_word) %>%
    nest(sentence_number,split_word) %>%
    return()
}

wordr_internal <- function(sentence, stemming = T){
  if(stemming){
    sentence %>%
      str_split("\\s+") %>% 
      unlist() %>%
      SnowballC::wordStem() %>%
      data_frame(split_word = .) %>%
      filter(split_word != "") %>%
    return()
  } else {
    sentence %>%
      str_split("\\s+") %>% 
      unlist() %>%
      data_frame(split_word = .) %>%
      filter(split_word != "") %>%
      return()
  }
}


cooccurrence_counter <- function(nested_word_splitted_df, sort = T){
  nested_word_splitted_df %>% 
    mutate(pw_n = map(data, ~.x %>%
                        pairwise_count(split_word,
                                       sentence_number,
                                       diag = F, sort = sort))
    ) %>%
    select(-data) %>%
    return()
}

traits_ww <- function(cooccurences_df,rank = 2,
                      n = F, sample_data = F,
                      linearise = F,
                      mode_svd = "base"){
  source("../R/traits.R")
  
  Traits <- cooccurences_df %>%
    slicer(n, sample_data) %>%
    ccv_to_mat() %>%
    trait_matrix_sym(rank,mode_svd)
  if(linearise){
    Traits <- linearise_traits(Traits) %>% as.matrix()
  }
  data.frame(label = rownames(Traits),
             trait = Traits,
             row.names = NULL) %>%
    return()
}

align_traits_df <- function(data_df){
  
  data_unnested <- data_df
  
  temp_sour <- data_unnested %>% select(traits) %>% unnest(traits)
  temp_targ <- data_unnested %>% select(org_traits) %>% unnest(org_traits)
  
  print(temp_sour %>% dim)
  print(temp_targ %>% dim)
  
  
  temp_sour %>%
    semi_join(temp_targ, by = "label") %>%
    semi_join(tidytext::stop_words, by=c("label" = "word")) %$%
    label[1:30] %>%
    as.vector() ->
    quasi_invariants
  
  print(quasi_invariants)
  
  temp_sour %>%
    filter(label %in% quasi_invariants) %>%
    arrange(label) %>%
    select(-label) %>%
    as.matrix() ->
    proc_sour
  
  temp_targ %>%
    filter(label %in% quasi_invariants) %>%
    arrange(label) %>%
    select(-label) %>%
    as.matrix()  ->
    proc_targ
  
  print(proc_sour %>% dim)
  print(proc_targ %>% dim)
  
  proc_sour %>%
    MCMCpack::procrustes(proc_targ) %$%
    R -> rotation
  
  new_sour_traits <- data.frame(
    label = temp_sour %$% label,
    trait = temp_sour %>%
      select(-label) %>%
      as.matrix() %*% rotation
  ) %>% tbl_df
  
  return(new_sour_traits)
}

traits_by_chapter_2w <- function(word_space_df,Word1,Word2,Book){
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
  return()
}
