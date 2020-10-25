av_ggplot <- function(model){
  dat <- model.matrix(model) %>% as.data.frame()
  ind_vars <- names(dat)[-1]
  dep_name <- names(model$model)[1]
  dep_var <- model$model[,1] %>% as_vector()
  
  for(i in seq_along(ind_vars)){
    ind_y <- dat[ind_vars][,i]
    ind_df <- dat[ind_vars][-i]
    
    dep_mod <- residuals(lm(dep_var ~ ., data = ind_df))
    ind_mod <- residuals(lm(ind_y ~ ., data = ind_df))
    
    print(ggplot(data = NULL, aes(x = ind_mod, y = dep_mod)) +
            geom_point(alpha = 1/3) +
            geom_smooth(method = lm) +
            labs(x = paste(ind_vars[i], "| others", sep = " "), 
                 y = paste(dep_name, "| others", sep = " ")))
  }
}

pr_ggplot <- function(model){
  mm <- model.matrix(model) %>% as.data.frame() %>% dplyr::select(-1)
  e <- residuals(model)
  coefs <- coef(model)[-1]
  
  for(i in seq_along(coefs)){
    Xi <- mm[,i]
    `Partial Residuals` <- (coefs[i] * Xi) + e
    
    print(ggplot(data = NULL,
                 aes(x = Xi,
                     y = `Partial Residuals`)) +
            geom_point(alpha = 1/3) +
            geom_smooth(method = "lm", se = FALSE, colour = "darkred") +
            geom_smooth(se = FALSE) +
            labs(x = names(coefs)[i]))
  }
}
