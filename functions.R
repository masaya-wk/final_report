

DID <- function(outcome_var){
  estimatr::lm_robust(outcome_var ~  
                        Year:Treatment +
                        lag_death,
                      data = df, 
                      fixed_effects = ~ factor(year4) + factor(Prefecture)  + factor(week)+ factor(day_w),
                      se_type = "stata", 
                      clusters = factor(day_w), 
                      weights = population,
                      alpha = 0.1)}


dynamic_DID <- function(outcome_var){
estimatr::lm_robust(outcome_var ~    
                                     Year:weekbefore3 +
                                     Year:weekbefore2 +
                                     Year:weekbefore1 +
                                     Year:week0 +
                                     Year:weekafter1 +
                                     Year:weekafter2 +
                                     Year:weekafter3 +
                                     Year:weekafter4 +
                                     lag_death,
                                   　data = df, 
                                   fixed_effects = ~ factor(year4) + factor(Prefecture)  + factor(week)+ factor(day_w),
                                   se_type = "stata", 
                                   clusters = factor(day_w), 
                                   weights = population,
                                   alpha = 0.1)}



DID_coefficients_main <- function(DID_results){
  
  # make a data frame for graph
  data_for_graph <- data.frame(estimate = DID_results$coefficients, 
                               ci_lower = DID_results$conf.low,
                               ci_upper = DID_results$conf.high)
  
  # make lable (of date)  variable
  data_for_graph$label <- rownames(data_for_graph)
  # replace rownames to number
  rownames(data_for_graph) <- 1:nrow(data_for_graph)
  
  # generate month variable based on label
  data_for_graph <- data_for_graph %>% mutate(week = case_when(
    str_detect(data_for_graph$label, "Year:weekbefore4") ~ -4,
    str_detect(data_for_graph$label, "Year:weekbefore3") ~ -3,
    str_detect(data_for_graph$label, "Year:weekbefore2") ~ -2,
    str_detect(data_for_graph$label, "Year:weekbefore1") ~ -1,
    str_detect(data_for_graph$label, "Year:week0") ~ 0,
    str_detect(data_for_graph$label, "Year:weekafter1") ~ 1,
    str_detect(data_for_graph$label, "Year:weekafter2") ~ 2,
    str_detect(data_for_graph$label, "Year:weekafter3") ~ 3,
    str_detect(data_for_graph$label, "Year:weekafter4") ~ 4
  ))
  

  # bind reference month data
  reference_month <- c(0, 0, 0, "Year:weekbefore4", -4)
  data_for_graph <- rbind(data_for_graph, reference_month)
  
  # delete NA
  data_for_graph <- na.omit(data_for_graph)
  
  #put "treat" into label data and rename 
  data_for_graph <- data_for_graph %>% 
    rename(treatment = label)
  
  # bind reference month data
  #reference_month <- c(0, 0, 0, treat_var, "2020-01")
  #data_for_graph <- rbind(data_for_graph, reference_month)
  
  # turn into numeric (alread numeric?)
  data_for_graph <- data_for_graph %>% 
    mutate(estimate = as.numeric(estimate)) %>%
    mutate(ci_lower = as.numeric(ci_lower)) %>%
    mutate(ci_upper = as.numeric(ci_upper)) 
  
  # Gen estimation label
  # data_for_graph <- data_for_graph %>%
  #   mutate(estimation_type = estimation_label)
  #old:data_for_graph[,1:3] <- lapply(data_for_graph[,1:3], as.numeric)
  
}


event_study_graph <- function(data){
  
  ## dodge for overlapping graphs (not used)
  dodge <- position_dodge(0) # move graphs 0.05 to the left and right
  
  ## label (not used)
  label <- ""
  
  ## ggplot  
  data %>% ggplot(aes(x = week, y = estimate, group = 1)) +
    geom_line(size = 1.1, position = dodge, color = "blue") + # line
    geom_point(size = 2.5, position = dodge, fill = "blue", color = "blue") + # 21 is filled circle
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, width = 0.15, color = "blue"), position = dodge, color = "black") + # 90% CI error bar
    #geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, alpha = 0.1), position = dodge, color = "gray") + # 9t% CI shaded region
    #geom_line(aes(x = month, y = ci_lower), size = 0.8, color = "blue", linetype = "dashed") +
    #geom_line(aes(x = month, y = ci_upper), size = 0.8, color = "blue", linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray") + # line of y=0+
    geom_vline(xintercept = 0, linetype = "dotted", color = "gray") + # line of y=0+
    #scale_colour_discrete(name = " ") +  # Legend label
    #scale_shape_discrete(name = " ") +  # Legend label
    ylab("Estimates")+
    xlab("Weeks elapsed since the stay-aty-home order")+
    scale_x_continuous(breaks = seq(-4, 4, 1))+ # x axis break and label
    #                    labels=seq(1970, 1995, 5)) + 
    #scale_y_continuous(breaks = seq(-0.1:0.15, by = 0.05)) + # y axix breaks
    theme_classic(base_size = 15) + # white background
    theme(legend.position = 'none', axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), # Adjust x axis label
          panel.grid.minor = element_blank(),  # Delete minor grid lines
          legend.title=element_blank())   # Delete legend title
  
}