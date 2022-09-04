bacon_orig <- function(formula,
                       data,
                       id_var,
                       time_var, 
                       quietly = F) {
  # Evaluate formula in data environment
  formula <- formula(terms(formula, data = data))
  
  # Unpack variable names and rename variables
  vars <- unpack_variable_names(formula)
  outcome_var <- vars$outcome_var
  treated_var <- vars$treated_var
  control_vars <- vars$control_vars
  data <- rename_vars(data, id_var, time_var, outcome_var, treated_var)
  
  # Check for a balanced panel
  bal <- aggregate(time ~ id,  data = data, FUN = length)
  balanced <- ifelse(all(bal$time == bal$time[1]), 1, 0)
  if (!balanced) {
    stop("Unbalanced Panel")
  }
  
  # Check for NA observations
  nas <- sum(is.na(data[, c("id", "time", "outcome", "treated")]))
  if (length(control_vars > 0)) {
    control_formula <- update(
      formula,
      paste0("treated ~ . - 1 - ", treated_var)
    )
    mm_control <- model.matrix(control_formula, data = data)
    nas_control <- 1 - (nrow(mm_control) == nrow(data))
    nas <- nas + nas_control
  }
  if (nas > 0) {
    stop("NA observations")
  }
  
  # Create 2x2 grid of treatment groups
  treatment_group_calc <- create_treatment_groups(data, control_vars,
                                                  return_merged_df = TRUE)
  two_by_twos <- treatment_group_calc$two_by_twos
  data <- treatment_group_calc$data
  
  # Uncontrolled 
  if (length(control_vars) == 0) {
    # Iterate through treatment group dyads
    for (i in 1:nrow(two_by_twos)) {
      treated_group <- two_by_twos[i, "treated"]
      untreated_group <- two_by_twos[i, "untreated"]
      
      data1 <- subset_data(data, treated_group, untreated_group)
      
      # Calculate estimate and weight
      weight <- calculate_weights(data = data1,
                                  treated_group = treated_group,
                                  untreated_group = untreated_group)
      estimate <- lm(outcome ~ treated + factor(time) + factor(id),
                     data = data1)$coefficients[2]
      
      two_by_twos[i, "estimate"] <- estimate
      two_by_twos[i, "weight"] <- weight
    }
    
    # Rescale weights to sum to 1
    two_by_twos <- scale_weights(two_by_twos)
    
    if (quietly == F) {
      print_summary(two_by_twos)
    }
    
    return(two_by_twos)
    
  } else if (length(control_vars) > 0) {
    # Controled 
    # Predict Treatment and calulate demeaned residuals
    control_formula <- update(
      formula,
      paste0("treated ~ . + factor(time) + factor(id) -", treated_var)
    )
    
    data <- run_fwl(data, control_formula)
    
    # Calculate within treatment group estimate and its weight
    Omega <- calculate_Omega(data)
    beta_hat_w <- calculate_beta_hat_w(data)
    
    # Collapse controls and predicted treatment to treatment group/year level
    r_collapse_x_p <- collapse_x_p(data, control_formula)
    data <- r_collapse_x_p$data
    g_control_formula <- r_collapse_x_p$g_control_formula
    
    # Iterate through treatment group dyads
    for (i in 1:nrow(two_by_twos)) {
      treated_group <- two_by_twos[i, "treated"]
      untreated_group <- two_by_twos[i, "untreated"]
      data1 <- data[data$treat_time %in% c(treated_group, untreated_group), ]
      
      # Calculate between group estimate and weight
      weight_est <- calc_controlled_beta_weights(data1, g_control_formula)
      s_kl <- weight_est$s_kl
      beta_hat_d_bkl <- weight_est$beta_hat_d_bkl
      
      two_by_twos[i, "weight"] <- s_kl
      two_by_twos[i, "estimate"] <- beta_hat_d_bkl
    }
    
    # Rescale weights to sum to 1
    two_by_twos <- scale_weights(two_by_twos)
    
    if (quietly == F) {
      print_summary(two_by_twos)
    }
    
    r_list <- list("beta_hat_w" = beta_hat_w,
                   "Omega" = Omega,
                   "two_by_twos" = two_by_twos)
    return(r_list)
  }
}

unpack_variable_names <- function(formula) {
  outcome_var <- base::as.character.default(formula)[2]
  right_side_vars <- base::as.character.default(formula)[3]
  right_side_vars <- strsplit(right_side_vars, " \\+ ")[[1]]
  treated_var <- right_side_vars[1]
  control_vars <- right_side_vars[-1]
  r_list <- list(outcome_var = outcome_var, treated_var = treated_var,
                 control_vars = control_vars)
  return(r_list)
}

lardon <- function(formula,
                   data,
                   id_var,
                   time_var, 
                   quietly = FALSE) {
  # Evaluate formula in data environment
  formula <- formula(terms(formula, data = data))
  
  # Unpack variable names and rename variables
  vars <- unpack_variable_names(formula)
  if(is.na(vars$outcome_var)) warning("Problem with formula!")
  outcome_var <- vars$outcome_var
  treated_var <- vars$treated_var
  control_vars <- vars$control_vars
  data <- bacondecomp:::rename_vars(data, id_var, time_var, outcome_var, treated_var)
  
  # Check for a balanced panel
  bal <- aggregate(time ~ id,  data = data, FUN = length)
  balanced <- ifelse(all(bal$time == bal$time[1]), 1, 0)
  if (!balanced) {
    stop("Unbalanced Panel")
  }
  
  # Check for NA observations
  nas <- sum(is.na(data[, c("id", "time", "outcome", "treated")]))
  if (length(control_vars > 0)) {
    control_formula <- update(
      formula,
      paste0("treated ~ . - 1 - ", treated_var)
    )
    mm_control <- model.matrix(control_formula, data = data)
    nas_control <- 1 - (nrow(mm_control) == nrow(data))
    nas <- nas + nas_control
  }
  if (nas > 0) {
    stop("NA observations")
  }
  
  # Create 2x2 grid of treatment groups
  treatment_group_calc <- bacondecomp:::create_treatment_groups(data, 
                                                                control_vars,
                                                                return_merged_df = TRUE)
  two_by_twos <- as_tibble(treatment_group_calc$two_by_twos) %>% 
    mutate(across(c(weight, estimate), as.numeric),
           means=list(tibble()))
  data <- treatment_group_calc$data
  
  # Uncontrolled
  if (length(control_vars) == 0) {
    # Iterate through treatment group dyads
    for (i in 1:nrow(two_by_twos)) {
      treated_group <- two_by_twos[i, "treated", drop=TRUE]
      untreated_group <- two_by_twos[i, "untreated", drop=TRUE]
      
      data1 <- bacondecomp:::subset_data(data, treated_group, untreated_group)
      
      # Calculate estimate and weight
      weight <- bacondecomp:::calculate_weights(data = data1,
                                                treated_group = treated_group,
                                                untreated_group = untreated_group)
      # multiDiff::DD_manu(data1,
      #                    y_var = "outcome",
      #                    time.index = "time",treat = "treated",unit.index = "id",
      #                    control_gr = seqs$seq[2],
      #                    treat_gr = seqs$seq[1])
      # seqs <- data1 %>% 
      #   distinct(treat_time, time, treated) %>% 
      #   arrange(treat_time, time) %>% 
      #   group_by(treat_time) %>%  
      #   summarise(seq=paste(treated, collapse = "_"))
      all_means_df <- all_means(data1, timing_treat=treated_group) 
      # all_means      
      # ## bacon means
      # all_means %>% 
      #   group_by(group, treatment_period) %>% 
      #   summarise(mean=mean(mean)) %>% 
      #   ungroup() %>% 
      #   spread(group, mean) %>% 
      #   mutate(diff_group=.[,2,drop=TRUE]-.[,3,drop=TRUE]) %>% 
      #   matPkg::mat_add_total_row(fun = diff)
      estimate <- lfe::felm(outcome ~ treated |time+ id,data = data1)
      
      two_by_twos[i, "estimate"] <- coef(estimate)
      two_by_twos[i, "weight"] <- weight
      two_by_twos[i, "means"] <- list(list(all_means_df))
      
    }
    
    # Rescale weights to sum to 1
    two_by_twos <- bacondecomp:::scale_weights(two_by_twos)
    
    if (quietly == FALSE) {
      bacondecomp:::print_summary(two_by_twos)
    }
    res <- two_by_twos
    
  } else if (length(control_vars) > 0) {
    # Controled
    # Predict Treatment and calulate demeaned residuals
    control_formula <- update(
      formula,
      paste0("treated ~ . + factor(time) + factor(id) -", treated_var)
    )
    
    data <- bacondecomp:::run_fwl(data, control_formula)
    
    # Calculate within treatment group estimate and its weight
    Omega <- bacondecomp:::calculate_Omega(data)
    beta_hat_w <- bacondecomp:::calculate_beta_hat_w(data)
    
    # Collapse controls and predicted treatment to treatment group/year level
    r_collapse_x_p <- bacondecomp:::collapse_x_p(data, control_formula)
    data <- r_collapse_x_p$data
    g_control_formula <- r_collapse_x_p$g_control_formula
    
    # Iterate through treatment group dyads
    for (i in 1:nrow(two_by_twos)) {
      treated_group <- two_by_twos[i, "treated", drop=TRUE]
      untreated_group <- two_by_twos[i, "untreated", drop=TRUE]
      data1 <- data[data$treat_time %in% c(treated_group, untreated_group), ]
      
      # Calculate between group estimate and weight
      weight_est <- bacondecomp:::calc_controlled_beta_weights(data1, g_control_formula)
      s_kl <- weight_est$s_kl
      beta_hat_d_bkl <- weight_est$beta_hat_d_bkl
      
      all_means_df <- all_means(data1, timing_treat=treated_group) 
      
      two_by_twos[i, "weight"] <- s_kl
      two_by_twos[i, "estimate"] <- beta_hat_d_bkl
      two_by_twos[i, "means"] <- list(list(all_means_df))
    }
    
    # Rescale weights to sum to 1
    two_by_twos <- bacondecomp:::scale_weights(two_by_twos)
    
    if (quietly == FALSE) {
      bacondecomp:::print_summary(two_by_twos)
    }
    
    res <- list("beta_hat_w" = beta_hat_w,
                "Omega" = Omega,
                "two_by_twos" = two_by_twos)
    
  }
  res
}

all_means <- function(df, timing_treat){
  sort(unique(df$time))
  df %>% 
    mutate(treatment_period=if_else(time<timing_treat, "pre", "post") %>% 
             factor() %>% 
             fct_relevel("pre"),
           treat_order=fct_relevel(factor(treat_time), as.character(timing_treat))) %>% 
    rename(group=treat_time) %>% 
    group_by(group, treat_order, time, treatment_period) %>% 
    summarise(mean=mean(outcome, na.rm=TRUE),
              # treatment_period=unique(treated),
              .groups="drop_last") %>% 
    ungroup()  %>% 
    arrange(treat_order, time)
  # group_by(time) %>%  
  # ungroup()  
}

# get_bacon <- function(df){
#   dat_DID <- df %>%
#     arrange(treat_order, time) %>% 
#     group_by(treat_order, treatment_period) %>%
#     summarise(mean=mean(mean), .groups="drop_last") %>%
#     ungroup() %>%
#     spread(treat_order, mean) %>%
#     mutate(diff_group=.[,2,drop=TRUE]-.[,3,drop=TRUE])
#   diff(dat_DID$diff_group)
# }

get_any <- function(df, after=0, before=1, instant=TRUE, print=FALSE, warn=TRUE){
  time_avail <- unique(df$time)
  range <- diff(range(time_avail))
  if(is.null(after)) after <- range
  if(is.null(before)) before <- range
  treat_time <- min(filter(df, treatment_period=="post")$time)
  time_keep <- (treat_time-before):(treat_time+after)
  if(!instant) time_keep <- time_keep[time_keep!=treat_time]
  time_keep_keep <- time_keep[time_keep%in%time_avail]
  if(!all(time_keep%in%time_avail) & warn) warning("Missing some periods")
  
  if(print) print(paste("Treatment at:", treat_time, "looking at: ", paste(time_keep_keep, collapse = ",")))
  dat_DID <- df %>%
    filter(time %in% time_keep_keep) %>% 
    arrange(treat_order, time) %>% 
    group_by(treat_order, treatment_period) %>%
    summarise(mean=mean(mean), .groups="drop_last") %>%
    ungroup() %>%
    spread(treat_order, mean) %>%
    mutate(diff_group=.[,2,drop=TRUE]-.[,3,drop=TRUE])
  
  ## check enough
  if(nrow(dat_DID)<2){
    if(warn) warning("Not enough values")
    return(NA_real_)
  }
  diff(dat_DID$diff_group)
}

get_chaise <- function(df, ...){ 
  get_any(df, after=0, before=1, instant=TRUE, ...)
}

get_jojo1 <- function(df, ...){ 
  get_any(df, after=1, before=1, instant=FALSE, ...)
}

get_bacon <- function(df, ...){ 
  get_any(df, after=NULL, before=NULL, instant=TRUE, ...)
}

get_7 <- function(df, ...){ 
  get_any(df, after=7, before=NULL, instant=TRUE, ...)
}


#library(bacondecomp)
#library(tidyverse)

if(FALSE){
  dat <- bacondecomp::castle[, c("l_homicide", "state", "year", "post", "l_pop", "l_income")]
  
  ## works
  coef(lfe::felm(l_homicide ~ post|state+year, data = dat))
  environment(bacon_orig) <- environment(bacon)
  df_bacon <- bacon_orig(l_homicide ~ post,
                         data = dat,
                         id_var = "state",
                         time_var = "year")%>% 
    arrange(treated, untreated) %>% 
    as_tibble()
  with(df_bacon, weighted.mean(estimate, weight))
  
  df_bacon %>% head()
  df_lardon <- lardon(formula=l_homicide ~ post,
                      data = dat,
                      id_var = "state",
                      time_var = "year") %>% 
    mutate(dynamic_average=map_dbl(means, get_bacon)) %>% 
    arrange(treated, untreated)
  
  df_lardon
  
  
  get_bacon(df=df_lardon$means[[2]], print=TRUE)
  get_chaise(df=df_lardon$means[[2]], print=TRUE)
  get_jojo1(df=df_lardon$means[[2]], print=TRUE)
  
  ##
  # Castle Doctrine (Controlled)
  lfe::felm(l_homicide ~ post + l_pop + l_income|state+year,
            data = dat)
  ret_bacon_full <- bacon(l_homicide ~ post + l_pop + l_income,
                          data = dat,
                          id_var = "state",
                          time_var = "year")
  ret_bacon <- ret_bacon_full$two_by_twos %>% 
    arrange(treated, untreated) %>% 
    as_tibble
  #  type                    weight avg_est
  #  1         Both Treated 0.09121 0.02724
  #  2 Treated vs Untreated 0.90879 0.09002
  
  ret_bacon
  ret_bacon %>% 
    group_by(type) %>% 
    summarise(estimate=weighted.mean(estimate, weight),
              weight=sum(weight)) %>% 
    select(type, weight, estimate)
  ret_bacon %>% 
    group_by(type) %>% 
    summarise(estimate=weighted.mean(estimate, weight),
              weight=sum(weight)) %>% 
    select(type, weight, estimate) %>% 
    ungroup() %>% 
    summarise(beta=weighted.mean(estimate, weight))
  
  out <- with(ret_bacon, weighted.mean(estimate, weight))
  
  ## full
  ret_bacon_full$beta_hat_w*ret_bacon_full$Omega+ (1-ret_bacon_full$Omega)*out
  
  ret_lardon <- lardon(formula=l_homicide ~ post + l_pop + l_income,
                       data = bacondecomp::castle,
                       id_var = "state",
                       time_var = "year")$two_by_twos %>% 
    arrange(treated, untreated) %>% 
    mutate(dynamic_average=map_dbl(means, get_bacon))
  ret_lardon
  ret_lardon$means[[1]] %>% 
    distinct(treatment_period)
  
}