library(dplyr)
select_serovar_strains <- function(
    df,
    serovar_col = serovar,
    submitter_col = asm_submitter,
    contig_col = contig_count,
    assembly_id_col = accession,
    bioproject_col = bioproject,
    seq_rel_date_col = seq_rel_date,
    max_n = 200,
    contig_threshold = 10,
    force_include = NULL,
    force_exclude = NULL
) {
  
  id_col  <- rlang::as_name(rlang::ensym(assembly_id_col))
  sub_col <- rlang::as_name(rlang::ensym(submitter_col))
  bp_col  <- rlang::as_name(rlang::ensym(bioproject_col))
  date_col <- rlang::as_name(rlang::ensym(seq_rel_date_col))
  
  df %>%
    { 
      if (!is.null(force_exclude)) {
        filter(., !(.data[[id_col]] %in% force_exclude))
      } else .
    } %>%
    group_by({{ serovar_col }}) %>%
    group_modify(~ {
      df <- .x
      
      # -------------------------
      # 0. Обязательные
      forced <- if (!is.null(force_include)) {
        df %>% filter(.data[[id_col]] %in% force_include)
      } else df[0, ]
      
      if (nrow(forced) >= max_n) {
        return(forced %>% slice_head(n = max_n))
      }
      
      df_rest <- df %>%
        anti_join(forced, by = id_col)
      
      remaining_n <- max_n - nrow(forced)
      
      # используемые submitter и bioproject
      used_submitters  <- unique(forced[[sub_col]])
      used_bioprojects <- unique(forced[[bp_col]])
      
      # -------------------------
      # 1. Новые submitter
      step1 <- df_rest %>%
        filter(!(.data[[sub_col]] %in% used_submitters)) %>%
        arrange({{ contig_col }}) %>%
        group_by({{ submitter_col }}) %>%
        slice_head(n = 1) %>%
        ungroup()
      
      if (nrow(step1) >= remaining_n) {
        return(bind_rows(forced, step1 %>% slice_head(n = remaining_n)))
      }
      
      df_rest2 <- df_rest %>%
        anti_join(step1, by = id_col)
      
      remaining_n <- remaining_n - nrow(step1)
      
      # -------------------------
      # 2. Другие bioproject
      step2 <- df_rest2 %>%
        filter(!(.data[[bp_col]] %in% used_bioprojects)) %>%
        arrange({{ contig_col }})
      
      if (nrow(step2) >= remaining_n) {
        return(bind_rows(forced, step1, step2 %>% slice_head(n = remaining_n)))
      }
      
      df_rest3 <- df_rest2 %>%
        anti_join(step2, by = id_col)
      
      remaining_n <- remaining_n - nrow(step2)
      
      # -------------------------
      # 3. Всё остальное: bioproject + дата
      step3 <- df_rest3 %>%
        arrange(
          .data[[bp_col]],
          .data[[date_col]],
          {{ contig_col }}
        )
      
      bind_rows(forced, step1, step2, step3) %>%
        slice_head(n = max_n)
    }) %>%
    ungroup()
}
