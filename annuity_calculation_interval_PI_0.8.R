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

den_interval_out <- function(data, horizon, ncomp_order, alpha_level)
{
    n_year = nrow(data)
    n_age = ncol(data)
    fore_validation = matrix(NA, ncol(data), ((n_year - 6) - horizon))
    for(ij in 1:((n_year - 6) - horizon))
    {
        fore_validation[,ij] = cdf_transformation(data_mat = data[1:(6 + ij),], fh = horizon, 
                                                  fmethod = "ets", ncomp_selection = ncomp_order)
        rm(ij)            
    }
    
    # holdout validation data
    
    holdout_validation_dum = t(matrix(data[(n_year - ((n_year - 6) - horizon - 1)):n_year,], 
                                      length((n_year - ((n_year - 6) - horizon - 1)):n_year), n_age))
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
    
    fore_val = cdf_transformation(data_mat = data, fh = horizon, fmethod = "ets", ncomp_selection = ncomp_order)
    fore_val_lb = fore_val - tune_para_find * sd_val_input
    fore_val_ub = fore_val + tune_para_find * sd_val_input
    
    fore_val_boot = matrix(NA, n_age, 1000)
    for(iw in 1:n_age)
    {
        fore_val_boot[iw,] = rnorm(1000, mean = fore_val[iw], sd = tune_para_find * sd_val_input[iw])
        rm(iw)
    }
    
    return(list(fore_val_lb = fore_val_lb, fore_val_ub = fore_val_ub, 
                tune_para_find = tune_para_find, tune_para_find_obj = obj_val_min))
}

## Female

# Work out dx

F_cdf_K6_int_lb = F_cdf_K6_int_ub = array(NA, dim = c(111, 50, n_country), dimnames = list(0:110, 1:50, country))
for(iwj in 1:n_country)
{
    for(iwk in 1:50)
    {
        dum = den_interval_out(data = get(overall_female_pop[iwj]), horizon = iwk, ncomp_order = "provide", 
                               alpha_level = 0.8)
        F_cdf_K6_int_lb[,iwk,iwj] = dum$fore_val_lb                               
        F_cdf_K6_int_ub[,iwk,iwj] = dum$fore_val_ub
        rm(dum); rm(iwk)
    }
    print(iwj); rm(iwj)
}

F_cdf_K6_int_lb_new = replace(F_cdf_K6_int_lb, which(F_cdf_K6_int_lb < 0), 0)

# Work out survival probability px

fore_out_px_F_lb = fore_out_px_F_ub = array(NA, dim = c(111, 50, n_country), dimnames = list(0:110, 1:50, country))
for(iw in 1:n_country)
{
    for(ij in 1:50)
    {
        fore_out_px_F_lb[,ij,iw] = 1 - LifeTable(0:110, dx = F_cdf_K6_int_lb_new[,ij,iw])$lt$qx
        fore_out_px_F_ub[,ij,iw] = 1 - LifeTable(0:110, dx = F_cdf_K6_int_ub[,ij,iw])$lt$qx
        rm(ij)
    }
    rm(iw)
}

# annuities

annuities_price_F_lb = annuities_price_F_ub = array(NA, dim = c(length(ages), length(maturities), n_country), 
                                                    dimnames = list(ages, maturities, country))
for(ik in 1:n_country)
{
    for(iw in 1:length(ages))
    {
        for(ij in 1:length(maturities))
        {
            dum_1 = try(AnnuityPrice_point(y.predict = diag(fore_out_px_F_lb[61:111,,ik]), age = ages[iw],
                                          maturity = maturities[ij], inRate = interest_rates[ik]/100), silent = TRUE)
            
            dum_2 = try(AnnuityPrice_point(y.predict = diag(fore_out_px_F_ub[61:111,,ik]), age = ages[iw],
                                          maturity = maturities[ij], inRate = interest_rates[ik]/100), silent = TRUE)
            
            annuities_price_F_lb[iw,ij,ik] = pmin(dum_1, dum_2)
            annuities_price_F_ub[iw,ij,ik] = pmax(dum_1, dum_2)
            rm(ij); rm(dum_1); rm(dum_2)
        }
        rm(iw)
    }
    rm(ik)
}

## Male

# Work out dx

M_cdf_K6_int_lb = M_cdf_K6_int_ub = array(NA, dim = c(111, 50, n_country), dimnames = list(0:110, 1:50, country))
for(iwj in 1:n_country)
{
    for(iwk in 1:50)
    {
        dum = den_interval_out(data = get(overall_male_pop[iwj]), horizon = iwk, ncomp_order = "provide", 
                               alpha_level = 0.8)
        M_cdf_K6_int_lb[,iwk,iwj] = dum$fore_val_lb                               
        M_cdf_K6_int_ub[,iwk,iwj] = dum$fore_val_ub
        rm(dum); rm(iwk)
    }
    print(iwj); rm(iwj)
}

M_cdf_K6_int_lb_new = replace(M_cdf_K6_int_lb, which(M_cdf_K6_int_lb < 0), 0)

# Work out survival probability px

fore_out_px_M_lb = fore_out_px_M_ub = array(NA, dim = c(111, 50, n_country), dimnames = list(0:110, 1:50, country))
for(iw in 1:n_country)
{
    for(ij in 1:50)
    {
        fore_out_px_M_lb[,ij,iw] = 1 - LifeTable(0:110, dx = M_cdf_K6_int_lb_new[,ij,iw])$lt$qx
        fore_out_px_M_ub[,ij,iw] = 1 - LifeTable(0:110, dx = M_cdf_K6_int_ub[,ij,iw])$lt$qx
        rm(ij)
    }
    rm(iw)
}

# annuities

annuities_price_M_lb = annuities_price_M_ub = array(NA, dim = c(length(ages), length(maturities), n_country), 
                                                    dimnames = list(ages, maturities, country))
for(ik in 1:n_country)
{
    for(iw in 1:length(ages))
    {
        for(ij in 1:length(maturities))
        {
            dum_1 = try(AnnuityPrice_point(y.predict = diag(fore_out_px_M_lb[61:111,,ik]), age = ages[iw],
                                           maturity = maturities[ij], inRate = interest_rates[ik]/100), silent = TRUE)
            
            dum_2 = try(AnnuityPrice_point(y.predict = diag(fore_out_px_M_ub[61:111,,ik]), age = ages[iw],
                                           maturity = maturities[ij], inRate = interest_rates[ik]/100), silent = TRUE)
            
            annuities_price_M_lb[iw,ij,ik] = pmin(dum_1, dum_2)
            annuities_price_M_ub[iw,ij,ik] = pmax(dum_1, dum_2)
            rm(ij); rm(dum_1); rm(dum_2)
        }
        rm(iw)
    }
    rm(ik)
}


#############
# an example
#############

# 80% nominal

xtable(annuities_price_F_lb[,,1], digits = 4)
xtable(annuities_price_F_ub[,,1], digits = 4)

xtable(annuities_price_M_lb[,,1], digits = 4)
xtable(annuities_price_M_ub[,,1], digits = 4)

# 95% nominal

xtable(annuities_price_F_lb_0.95[,,1], digits = 4)
xtable(annuities_price_F_ub_0.95[,,1], digits = 4)

xtable(annuities_price_M_lb_0.95[,,1], digits = 4)
xtable(annuities_price_M_ub_0.95[,,1], digits = 4)

