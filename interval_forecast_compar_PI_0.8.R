#####################
# interval forecasts
#####################

# determining the optimal tuning parameter via standard deviation

tune_para_find_function <- function(tune_para, resi_mat, sd_val_input, alpha_level)
{
    n_age = nrow(resi_mat)
    ind = matrix(NA, n_age, ncol(resi_mat))
    for(iw in 1:ncol(resi_mat))
    {
        ind[,iw] = ifelse(between(resi_mat[,iw], -tune_para * sd_val_input, tune_para * sd_val_input), 1, 0)
        rm(iw)
    }
    ecp = sum(ind)/(n_age * ncol(resi_mat))
    rm(ind)
    return(abs(ecp - alpha_level))
}

# interval score

# holdout: holdout data
# lb: lower bound
# ub: upper bound
# alpha: level of significance

interval_score <- function(holdout, lb, ub, alpha)
{
    lb_ind = ifelse(holdout < lb, 1, 0)
    ub_ind = ifelse(holdout > ub, 1, 0)
    score = (ub - lb) + 2/alpha * ((lb - holdout) * lb_ind + (holdout - ub) * ub_ind)
    cover = 1 - (length(which(lb_ind == 1)) + length(which(ub_ind == 1)))/length(holdout)
    cpd = abs(cover - (1 - alpha))
    return(c(cover, cpd, mean(score)))
}


#########################
# train_set: 1:16
# validation_set: 17:32
# test_set: 33:48
#########################

# fdata: n by p data matrix
# horizon: forecast horizon
# fore_method: clr, ilr, cdf
# level_sig: level of significance

den_interval <- function(data, horizon, fore_method, ncomp_order, alpha_level)
{
    n_year = nrow(data)
    n_age = ncol(data)
    fore_validation = matrix(NA, ncol(data), (21 - horizon))
    if(fore_method == "clr")
    {
        for(ij in 1:(21 - horizon))
        {
            fore_validation[,ij] = compositions_clr_transformation(data = data[1:((n_year - 41) + ij),], 
                                                fh = horizon, fmethod = "ets", ncomp_selection = ncomp_order)
            rm(ij)
        }
    }
    else if(fore_method == "cdf")
    {
        for(ij in 1:(21 - horizon))
        {
            fore_validation[,ij] = cdf_transformation(data_mat = data[1:((n_year - 41) + ij),], fh = horizon, 
                                                  fmethod = "ets", ncomp_selection = ncomp_order)
            rm(ij)            
        }
    }
    else
    {
        warning("forecasting method must either be CLR, ILR or CDF.")
    }
    
    # holdout validation data
    
    holdout_validation_dum = t(matrix(data[((n_year - 40) + horizon):(n_year - 20),], 
                                      length(((n_year - 40) + horizon):(n_year - 20)), n_age))
    resi_mat = holdout_validation_dum - fore_validation
    
    # compute standard deviation of residuals
    
    sd_val_input = apply(resi_mat, 1, sd)
    
    # find the optimal tuning parameter
    
    tune_para_find_val_1 = optimise(f = tune_para_find_function, interval = c(0, 1), 
                                    resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                    alpha_level = alpha_level)
    
    tune_para_find_val_2 = optimise(f = tune_para_find_function, interval = c(0, 5), 
                                    resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                    alpha_level = alpha_level)
    
    tune_para_find_val_3 = optimise(f = tune_para_find_function, interval = c(0, 10), 
                                    resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                    alpha_level = alpha_level)
    
    tune_para_find_val_4 = optimise(f = tune_para_find_function, interval = c(0, 20), 
                                    resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                    alpha_level = alpha_level)
    
    tune_para_find_val_5 = optim(par = 1, fn = tune_para_find_function, lower = 0, method = "L-BFGS-B", 
                                 resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                 alpha_level = alpha_level)
    
    tune_para_find_val_6 = optim(par = 1, fn = tune_para_find_function, method = "Nelder-Mead",
                                 resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                 alpha_level = alpha_level)
    
    obj_val = c(tune_para_find_val_1$objective, 
                tune_para_find_val_2$objective, 
                tune_para_find_val_3$objective,
                tune_para_find_val_4$objective,
                tune_para_find_val_5$value,
                tune_para_find_val_6$value)
    obj_val_min = min(obj_val)
    
    tune_para_find = c(tune_para_find_val_1$minimum, 
                       tune_para_find_val_2$minimum,
                       tune_para_find_val_3$minimum,
                       tune_para_find_val_4$minimum,
                       tune_para_find_val_5$par,
                       tune_para_find_val_6$par)[which.min(obj_val)]
    
    fore_val = fore_val_lb = fore_val_ub = matrix(NA, ncol(data), (21 - horizon))
    if(fore_method == "clr")
    {
        for(ij in 1:(21 - horizon))
        {
            fore_val[,ij] = compositions_clr_transformation(data = data[1:((n_year - 21) + ij),], fh = horizon, 
                                               fmethod = "ets", ncomp_selection = ncomp_order)
            fore_val_lb[,ij] = fore_val[,ij] - tune_para_find * sd_val_input
            fore_val_ub[,ij] = fore_val[,ij] + tune_para_find * sd_val_input
            rm(ij)
        }
    }
    else if(fore_method == "cdf")
    {
        for(ij in 1:(21 - horizon))
        {
            fore_val[,ij] = cdf_transformation(data_mat = data[1:((n_year - 21) + ij),], fh = horizon, 
                                               fmethod = "ets", ncomp_selection = ncomp_order)
            fore_val_lb[,ij] = fore_val[,ij] - tune_para_find * sd_val_input
            fore_val_ub[,ij] = fore_val[,ij] + tune_para_find * sd_val_input
            rm(ij)            
        }
    }
    
    # holdout testing data
    
    holdout_val_dum = t(matrix(data[((n_year - 20) + horizon):n_year,], 
                               length(((n_year - 20) + horizon):n_year), n_age))
    
    int_err = interval_score(holdout = holdout_val_dum, lb = fore_val_lb, ub = fore_val_ub, alpha = (1 - alpha_level))
    return(list(int_err = int_err, tune_para_find = tune_para_find, tune_para_find_obj = obj_val_min))
}

#########
# female
#########

F_clr_EVR_int_err = F_cdf_EVR_int_err = F_clr_K6_int_err = F_cdf_K6_int_err = array(NA, dim = c(19, 3, n_country),
                                                            dimnames = list(1:19, c("ECP", "CPD", "score"), country))
F_EVR_tune_para = F_EVR_tune_obj = F_K6_tune_para = F_K6_tune_obj = array(NA, dim = c(19, 2, n_country),
                                 dimnames = list(1:19, c("CLR", "CDF"), country))
for(iwj in 1:n_country)
{
    for(iwk in 1:19)
    {
        dum = den_interval(data = get(overall_female_pop[iwk]), horizon = iwk, fore_method = "clr", 
                           ncomp_order = "EVR", alpha_level = 0.8)
        F_clr_EVR_int_err[iwk,,iwj] = dum$int_err
        F_EVR_tune_para[iwk,1,iwj] = dum$tune_para_find
        F_EVR_tune_obj[iwk,1,iwj] = dum$tune_para_find_obj
        rm(dum)
        
        dum = den_interval(data = get(overall_female_pop[iwk]), horizon = iwk, fore_method = "cdf", 
                           ncomp_order = "EVR", alpha_level = 0.8)
        F_cdf_EVR_int_err[iwk,,iwj] = dum$int_err
        F_EVR_tune_para[iwk,2,iwj] = dum$tune_para_find
        F_EVR_tune_obj[iwk,2,iwj] = dum$tune_para_find_obj
        
        
        dum = den_interval(data = get(overall_female_pop[iwk]), horizon = iwk, fore_method = "clr", 
                           ncomp_order = "provide", alpha_level = 0.8)
        F_clr_K6_int_err[iwk,,iwj] = dum$int_err
        F_K6_tune_para[iwk,1,iwj] = dum$tune_para_find
        F_K6_tune_obj[iwk,1,iwj] = dum$tune_para_find_obj
        rm(dum)
        
        dum = den_interval(data = get(overall_female_pop[iwk]), horizon = iwk, fore_method = "cdf", 
                           ncomp_order = "provide", alpha_level = 0.8)
        F_cdf_K6_int_err[iwk,,iwj] = dum$int_err
        F_K6_tune_para[iwk,2,iwj] = dum$tune_para_find
        F_K6_tune_obj[iwk,2,iwj] = dum$tune_para_find_obj
        print(iwk); rm(iwk); rm(dum)
    }
    print(iwj); rm(iwj)
}

CPD_F_EVR_array = CPD_M_EVR_array = array(NA, dim = c(19, 24, 4))
CPD_F_EVR_array[,,1] = F_clr_EVR_int_err[,2,]
CPD_F_EVR_array[,,2] = F_cdf_EVR_int_err[,2,]
CPD_F_EVR_array[,,3] = F_clr_K6_int_err[,2,]
CPD_F_EVR_array[,,4] = F_cdf_K6_int_err[,2,]

CPD_M_EVR_array[,,1] = M_clr_EVR_int_err[,2,]
CPD_M_EVR_array[,,2] = M_cdf_EVR_int_err[,2,]
CPD_M_EVR_array[,,3] = M_clr_K6_int_err[,2,]
CPD_M_EVR_array[,,4] = M_cdf_K6_int_err[,2,]

# CPD

CPD_F_EVR_array_mean = apply(CPD_F_EVR_array, c(1, 3), mean)
CPD_M_EVR_array_mean = apply(CPD_M_EVR_array, c(1, 3), mean)
colnames(CPD_F_EVR_array_mean) = c("CLR (EVR)", "CDF (EVR)", "CLR (K=6)", "CDF (K=6)")
rownames(CPD_F_EVR_array_mean) = 1:19

# score

score_F_EVR_array = score_M_EVR_array = array(NA, dim = c(19, 24, 4))
score_F_EVR_array[,,1] = F_clr_EVR_int_err[,3,]
score_F_EVR_array[,,2] = F_cdf_EVR_int_err[,3,]
score_F_EVR_array[,,3] = F_clr_K6_int_err[,3,]
score_F_EVR_array[,,4] = F_cdf_K6_int_err[,3,]

score_M_EVR_array[,,1] = M_clr_EVR_int_err[,3,]
score_M_EVR_array[,,2] = M_cdf_EVR_int_err[,3,]
score_M_EVR_array[,,3] = M_clr_K6_int_err[,3,]
score_M_EVR_array[,,4] = M_cdf_K6_int_err[,3,]

score_F_EVR_array_mean = apply(score_F_EVR_array, c(1, 3), mean)
score_M_EVR_array_mean = apply(score_M_EVR_array, c(1, 3), mean)

require(xtable)
xtable(cbind(rbind(CPD_F_EVR_array_mean, colMeans(CPD_F_EVR_array_mean)),
             rbind(CPD_M_EVR_array_mean, colMeans(CPD_M_EVR_array_mean))), digits = 3)

xtable(cbind(rbind(score_F_EVR_array_mean, colMeans(score_F_EVR_array_mean)),
             rbind(score_M_EVR_array_mean, colMeans(score_M_EVR_array_mean))), digits = 0)


###########
# CPD
# rankings
###########

n_methods = 4
CPD_F_EVR_array = array(NA, dim = c(19, 24, n_methods), 
                    dimnames = list(1:19, 1:24, c("CLR (EVR)", "CDF (EVR)", "CLR (K=6)", "CDF (K=6)")))
CPD_F_EVR_array[,,1] = apply(F_clr_EVR_int_err[,2,], 1, mean)
CPD_F_EVR_array[,,2] = apply(F_cdf_EVR_int_err[,2,], 1, mean)
CPD_F_EVR_array[,,3] = apply(F_clr_K6_int_err[,2,], 1, mean)
CPD_F_EVR_array[,,4] = apply(F_cdf_K6_int_err[,2,], 1, mean)

CPD_F_ranking_size_mat = matrix(0, nrow = 19, ncol = n_methods)
colnames(CPD_F_ranking_size_mat) = paste("M", 1:n_methods, sep = "")
rownames(CPD_F_ranking_size_mat) = 1:19
for(ih in 1:19)
{
    temp_rank = table(apply(CPD_F_EVR_array[ih,,],1, which.min))
    if(length(temp_rank) == n_methods)
    {
        CPD_F_ranking_size_mat[ih,] = temp_rank
    }
    else
    {
        temp_rank_extend = rep(0, n_methods)
        temp_rank_extend[as.numeric(names(temp_rank))] = as.numeric(temp_rank)
        CPD_F_ranking_size_mat[ih,] = temp_rank_extend
        rm(temp_rank_extend)
    }
    rm(temp_rank)
}

###################
# create a heatmap
###################

CPD_F_ranking_size_mat = data.frame(Horizon = 1:19, CPD_F_ranking_size_mat)

CPD_F_days_size = CPD_F_ranking_size_mat %>% 
  as_tibble() %>%
  pivot_longer(!Horizon, names_to = "Model", values_to = "count") %>%
  mutate(
    Horizon = factor(Horizon, ordered = TRUE, levels = 1:19),
    Model = factor(Model, ordered = TRUE, levels = paste("M", 1:n_methods, sep = ""))
  )

# save figure

ggsave("Fig_2a.png")
ggplot(CPD_F_days_size, aes(Model, ordered(Horizon, levels = 19:1))) +
  geom_tile(aes(fill = count)) +
  geom_text(aes(label = count)) +
  scale_x_discrete(position = "top") +
  theme(legend.position = "none") + 
  scale_fill_gradient2(high="darkgray",mid="lightgray",low="white", 
                       na.value="yellow", midpoint = 24/n_methods) + 
  ylab("Horizon") +
  xlab("") + 
  scale_x_discrete(labels = c("CLR (EVR)", "CDF (EVR)", "CLR (K=6)", "CDF (K=6)")) + 
  theme_bw()
dev.off()

###########
# score
# rankings
###########

n_methods = 4
score_F_EVR_array = array(NA, dim = c(19, 24, n_methods), 
                        dimnames = list(1:19, 1:24, c("CLR (EVR)", "CDF (EVR)", "CLR (K=6)", "CDF (K=6)")))
score_F_EVR_array[,,1] = apply(F_clr_EVR_int_err[,3,], 1, mean)
score_F_EVR_array[,,2] = apply(F_cdf_EVR_int_err[,3,], 1, mean)
score_F_EVR_array[,,3] = apply(F_clr_K6_int_err[,3,], 1, mean)
score_F_EVR_array[,,4] = apply(F_cdf_K6_int_err[,3,], 1, mean)

score_F_ranking_size_mat = matrix(0, nrow = 19, ncol = n_methods)
colnames(score_F_ranking_size_mat) = paste("M", 1:n_methods, sep = "")
rownames(score_F_ranking_size_mat) = 1:19
for(ih in 1:19)
{
    temp_rank = table(apply(score_F_EVR_array[ih,,],1, which.min))
    if(length(temp_rank) == n_methods)
    {
        score_F_ranking_size_mat[ih,] = temp_rank
    }
    else
    {
        temp_rank_extend = rep(0, n_methods)
        temp_rank_extend[as.numeric(names(temp_rank))] = as.numeric(temp_rank)
        score_F_ranking_size_mat[ih,] = temp_rank_extend
        rm(temp_rank_extend)
    }
    rm(temp_rank)
}

###################
# create a heatmap
###################

score_F_ranking_size_mat = data.frame(Horizon = 1:19, score_F_ranking_size_mat)

score_F_days_size = score_F_ranking_size_mat %>% 
  as_tibble() %>%
  pivot_longer(!Horizon, names_to = "Model", values_to = "count") %>%
  mutate(
    Horizon = factor(Horizon, ordered = TRUE, levels = 1:19),
    Model = factor(Model, ordered = TRUE, levels = paste("M", 1:n_methods, sep = ""))
  )

# save figure

ggsave("Fig_2b.png")
ggplot(score_F_days_size, aes(Model, ordered(Horizon, levels = 19:1))) +
  geom_tile(aes(fill = count)) +
  geom_text(aes(label = count)) +
  scale_x_discrete(position = "top") +
  theme(legend.position = "none") + 
  scale_fill_gradient2(high="darkgray",mid="lightgray",low="white", 
                       na.value="yellow", midpoint = 24/n_methods) + 
  ylab("") +
  xlab("") + 
  scale_x_discrete(labels = c("CLR (EVR)", "CDF (EVR)", "CLR (K=6)", "CDF (K=6)")) + 
  theme_bw()
dev.off()


#######
# male
#######

M_clr_EVR_int_err = M_cdf_EVR_int_err = M_clr_K6_int_err = M_cdf_K6_int_err = array(NA, dim = c(19, 3, n_country),
                                                                                    dimnames = list(1:19, c("ECP", "CPD", "score"), country))
M_EVR_tune_para = M_EVR_tune_obj = M_K6_tune_para = M_K6_tune_obj = array(NA, dim = c(19, 2, n_country),
                                                                          dimnames = list(1:19, c("CLR", "CDF"), country))
for(iwj in 1:n_country)
{
    for(iwk in 1:19)
    {
        dum = den_interval(data = get(overall_male_pop[iwk]), horizon = iwk, fore_method = "clr", 
                           ncomp_order = "EVR", alpha_level = 0.8)
        M_clr_EVR_int_err[iwk,,iwj] = dum$int_err
        M_EVR_tune_para[iwk,1,iwj] = dum$tune_para_find
        M_EVR_tune_obj[iwk,1,iwj] = dum$tune_para_find_obj
        rm(dum)
        
        dum = den_interval(data = get(overall_male_pop[iwk]), horizon = iwk, fore_method = "cdf", 
                           ncomp_order = "EVR", alpha_level = 0.8)
        M_cdf_EVR_int_err[iwk,,iwj] = dum$int_err
        M_EVR_tune_para[iwk,2,iwj] = dum$tune_para_find
        M_EVR_tune_obj[iwk,2,iwj] = dum$tune_para_find_obj
        
        
        dum = den_interval(data = get(overall_male_pop[iwk]), horizon = iwk, fore_method = "clr", 
                           ncomp_order = "provide", alpha_level = 0.8)
        M_clr_K6_int_err[iwk,,iwj] = dum$int_err
        M_K6_tune_para[iwk,1,iwj] = dum$tune_para_find
        M_K6_tune_obj[iwk,1,iwj] = dum$tune_para_find_obj
        rm(dum)
        
        dum = den_interval(data = get(overall_male_pop[iwk]), horizon = iwk, fore_method = "cdf", 
                           ncomp_order = "provide", alpha_level = 0.8)
        M_cdf_K6_int_err[iwk,,iwj] = dum$int_err
        M_K6_tune_para[iwk,2,iwj] = dum$tune_para_find
        M_K6_tune_obj[iwk,2,iwj] = dum$tune_para_find_obj
        print(iwk); rm(iwk); rm(dum)
    }
    print(iwj); rm(iwj)
}

###########
# CPD
# rankings
###########

n_methods = 4
CPD_M_EVR_array = array(NA, dim = c(19, 24, n_methods), 
                        dimnames = list(1:19, 1:24, c("CLR (EVR)", "CDF (EVR)", "CLR (K=6)", "CDF (K=6)")))
CPD_M_EVR_array[,,1] = apply(M_clr_EVR_int_err[,2,], 1, mean)
CPD_M_EVR_array[,,2] = apply(M_cdf_EVR_int_err[,2,], 1, mean)
CPD_M_EVR_array[,,3] = apply(M_clr_K6_int_err[,2,], 1, mean)
CPD_M_EVR_array[,,4] = apply(M_cdf_K6_int_err[,2,], 1, mean)

CPD_M_ranking_size_mat = matrix(0, nrow = 19, ncol = n_methods)
colnames(CPD_M_ranking_size_mat) = paste("M", 1:n_methods, sep = "")
rownames(CPD_M_ranking_size_mat) = 1:19
for(ih in 1:19)
{
    temp_rank = table(apply(CPD_M_EVR_array[ih,,],1, which.min))
    if(length(temp_rank) == n_methods)
    {
        CPD_M_ranking_size_mat[ih,] = temp_rank
    }
    else
    {
        temp_rank_extend = rep(0, n_methods)
        temp_rank_extend[as.numeric(names(temp_rank))] = as.numeric(temp_rank)
        CPD_M_ranking_size_mat[ih,] = temp_rank_extend
        rm(temp_rank_extend)
    }
    rm(temp_rank)
}

###################
# create a heatmap
###################

CPD_M_ranking_size_mat = data.frame(Horizon = 1:19, CPD_M_ranking_size_mat)

CPD_M_days_size = CPD_M_ranking_size_mat %>% 
  as_tibble() %>%
  pivot_longer(!Horizon, names_to = "Model", values_to = "count") %>%
  mutate(
    Horizon = factor(Horizon, ordered = TRUE, levels = 1:19),
    Model = factor(Model, ordered = TRUE, levels = paste("M", 1:n_methods, sep = ""))
  )

# save figure

ggsave("Fig_2c.png")
ggplot(CPD_M_days_size, aes(Model, ordered(Horizon, levels = 19:1))) +
  geom_tile(aes(fill = count)) +
  geom_text(aes(label = count)) +
  scale_x_discrete(position = "top") +
  theme(legend.position = "none") + 
  scale_fill_gradient2(high="darkgray",mid="lightgray",low="white", 
                       na.value="yellow", midpoint = 24/n_methods) + 
  ylab("Horizon") +
  scale_x_discrete(labels = c("CLR (EVR)", "CDF (EVR)", "CLR (K=6)", "CDF (K=6)")) + 
  theme_bw()
dev.off()

###########
# score
# rankings
###########

n_methods = 4
score_M_EVR_array = array(NA, dim = c(19, 24, n_methods), 
                          dimnames = list(1:19, 1:24, c("CLR (EVR)", "CDF (EVR)", "CLR (K=6)", "CDF (K=6)")))
score_M_EVR_array[,,1] = apply(M_clr_EVR_int_err[,3,], 1, mean)
score_M_EVR_array[,,2] = apply(M_cdf_EVR_int_err[,3,], 1, mean)
score_M_EVR_array[,,3] = apply(M_clr_K6_int_err[,3,], 1, mean)
score_M_EVR_array[,,4] = apply(M_cdf_K6_int_err[,3,], 1, mean)

score_M_ranking_size_mat = matrix(0, nrow = 19, ncol = n_methods)
colnames(score_M_ranking_size_mat) = paste("M", 1:n_methods, sep = "")
rownames(score_M_ranking_size_mat) = 1:19
for(ih in 1:19)
{
    temp_rank = table(apply(score_M_EVR_array[ih,,],1, which.min))
    if(length(temp_rank) == n_methods)
    {
        score_M_ranking_size_mat[ih,] = temp_rank
    }
    else
    {
        temp_rank_extend = rep(0, n_methods)
        temp_rank_extend[as.numeric(names(temp_rank))] = as.numeric(temp_rank)
        score_M_ranking_size_mat[ih,] = temp_rank_extend
        rm(temp_rank_extend)
    }
    rm(temp_rank)
}

###################
# create a heatmap
###################

score_M_ranking_size_mat = data.frame(Horizon = 1:19, score_M_ranking_size_mat)

score_M_days_size = score_M_ranking_size_mat %>% 
  as_tibble() %>%
  pivot_longer(!Horizon, names_to = "Model", values_to = "count") %>%
  mutate(
    Horizon = factor(Horizon, ordered = TRUE, levels = 1:19),
    Model = factor(Model, ordered = TRUE, levels = paste("M", 1:n_methods, sep = ""))
  )

# save figure

ggsave("Fig_2d.png")
ggplot(score_M_days_size, aes(Model, ordered(Horizon, levels = 19:1))) +
  geom_tile(aes(fill = count)) +
  geom_text(aes(label = count)) +
  scale_x_discrete(position = "top") +
  theme(legend.position = "none") + 
  scale_fill_gradient2(high="darkgray",mid="lightgray",low="white", 
                       na.value="yellow", midpoint = 24/n_methods) + 
  ylab("") +
  xlab("Model") + 
  scale_x_discrete(labels = c("CLR (EVR)", "CDF (EVR)", "CLR (K=6)", "CDF (K=6)")) + 
  theme_bw()
dev.off()

