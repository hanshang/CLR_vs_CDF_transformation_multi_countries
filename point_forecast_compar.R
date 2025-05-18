source("transformation.R")
source("load_package.R")

##################
# point forecasts
##################

# data: data matrix
# horizon: forecast horizon
# transformation: type of transformation, clr, ilr, cdf
# ncomp_order: way of selecting the number of components

den_point <- function(data, horizon, transformation, ncomp_order)
{
    n_year = nrow(data)
    n_age = ncol(data)
    den_fore = matrix(NA, n_age, (21 - horizon))
    for(iw in 1:(21 - horizon))
    {
        if(transformation == "compositions_clr")
        {
            den_fore[,iw] = as.numeric(compositions_clr_transformation(data = data[1:((n_year - 21) + iw),], fh = horizon, 
                                               fmethod = "ets", ncomp_selection = ncomp_order))
        }
        else if(transformation == "easyCoDa_clr")
        {
            den_fore[,iw] = as.numeric(easyCoDa_clr_transformation(data = data[1:((n_year - 21) + iw),], fh = horizon, 
                                                fmethod = "ets", ncomp_selection = ncomp_order))
        }
        else if(transformation == "cdf")
        {
            den_fore[,iw] = cdf_transformation(data_mat = data[1:((n_year - 21) + iw),], fh = horizon, 
                                               fmethod = "ets", ncomp_selection = ncomp_order)
        }
        else
        {
            warning("transformation can be either CLR or CDF.")
        }
        rm(iw)
    }
    
    # compute the KLD and JSD
    
    holdout_val_dum = t(matrix(data[((n_year - 20) + horizon):n_year,], length(((n_year - 20) + horizon):n_year), n_age))
    if(any(holdout_val_dum == 0))
    {
        holdout_val = replace(x = holdout_val_dum, list = which(holdout_val_dum == 0), values = 10^-5)
    }
    else
    {
        holdout_val = holdout_val_dum
    }
    rm(holdout_val_dum)
    
    if(all(dim(den_fore) == dim(holdout_val)))
    {
        KL_div_val = JS_div_val = vector("numeric", (21 - horizon))
        for(ij in 1:(21 - horizon))
        {
            KL_div_val[ij] = mean(KLdiv(cbind(den_fore[,ij], holdout_val[,ij]))[2:3])
            JS_div_val[ij] = mean(KLdiv(cbind(den_fore[,ij], 
                                      apply(cbind(den_fore[,ij], holdout_val[,ij]), 1, geometric.mean)))[2:3])
        }
        err = c(mean(KL_div_val), mean(JS_div_val))
    }
    else
    {
        warning("check the dimension.")
    }
    return(list(den_fore = den_fore, err = err))
}

#########
# female
#########

F_compositions_clr_EVR_point_err = F_cdf_EVR_point_err = 
F_compositions_clr_K6_point_err = F_cdf_K6_point_err = array(NA, dim = c(20, 2, 24), dimnames = list(1:20, c("KLD", "JSD"), country))
for(iwj in 2:n_country)
{
    for(iwk in 1:20)
    {
        # EVR
      
        F_compositions_clr_EVR_point_err[iwk,,iwj] = den_point(data = get(overall_female_pop[iwj]), horizon = iwk, transformation = "compositions_clr", ncomp_order = "EVR")$err
        F_cdf_EVR_point_err[iwk,,iwj] = den_point(data = get(overall_female_pop[iwj]), horizon = iwk, transformation = "cdf", ncomp_order = "EVR")$err
        
        # K = 6
        
        F_compositions_clr_K6_point_err[iwk,,iwj] = den_point(data = get(overall_female_pop[iwj]), horizon = iwk, transformation = "compositions_clr", ncomp_order = "provide")$err
        F_cdf_K6_point_err[iwk,,iwj] = den_point(data = get(overall_female_pop[iwj]), horizon = iwk, transformation = "cdf", ncomp_order = "provide")$err
        print(iwk); rm(iwk)
    }
    print(iwj); rm(iwj)
}

# KLD

savefig("F_EVR_KLD", width = 12, height = 10, toplines = 0.8, type = "png")
plot(1:20, apply(F_compositions_clr_EVR_point_err[,1,], 1, mean), type = "l", col = 1, lty = 1, 
     ylim = c(0, 0.11), xlab = "", ylab = "KLD", main = "Female data")
lines(1:20, apply(F_cdf_EVR_point_err[,1,], 1, mean), col = 2, lty = 2)
lines(1:20, apply(F_compositions_clr_K6_point_err[,1,], 1, mean), col = 3, lty = 3)
lines(1:20, apply(F_cdf_K6_point_err[,1,], 1, mean), col = 4, lty = 4)
legend("topleft", c("CLR (EVR)", "CDF (EVR)", "CLR (K=6)", "CDF (K=6)"), col = 1:4, lty = 1:4, cex = 0.8, ncol = 2)
dev.off()

# JSD

savefig("F_EVR_JSD", width = 12, height = 10, toplines = 0.8, type = "png")
plot(1:20, apply(F_compositions_clr_EVR_point_err[,2,], 1, mean), type = "l", col = 1, lty = 1, 
     ylim = c(0, 0.03), xlab = "Forecast horizon", ylab = "JSD", main = "")
lines(1:20, apply(F_cdf_EVR_point_err[,2,], 1, mean), col = 2, lty = 2)
lines(1:20, apply(F_compositions_clr_K6_point_err[,2,], 1, mean), col = 3, lty = 3)
lines(1:20, apply(F_cdf_K6_point_err[,2,], 1, mean), col = 4, lty = 4)
dev.off()

#######
# male
#######

M_compositions_clr_EVR_point_err = M_cdM_EVR_point_err = 
M_compositions_clr_K6_point_err = M_cdM_K6_point_err = array(NA, dim = c(20, 2, 24), dimnames = list(1:20, c("KLD", "JSD"), country))
for(iwj in 1:n_country)
{
    for(iwk in 1:20)
    {
        # EVR
        
        M_compositions_clr_EVR_point_err[iwk,,iwj] = den_point(data = get(overall_male_pop[iwj]), horizon = iwk, transformation = "compositions_clr", ncomp_order = "EVR")$err
        M_cdM_EVR_point_err[iwk,,iwj] = den_point(data = get(overall_male_pop[iwj]), horizon = iwk, transformation = "cdf", ncomp_order = "EVR")$err
        
        # K = 6
        
        M_compositions_clr_K6_point_err[iwk,,iwj] = den_point(data = get(overall_male_pop[iwj]), horizon = iwk, transformation = "compositions_clr", ncomp_order = "provide")$err
        M_cdM_K6_point_err[iwk,,iwj] = den_point(data = get(overall_male_pop[iwj]), horizon = iwk, transformation = "cdf", ncomp_order = "provide")$err
        print(iwk); rm(iwk)
    }
    print(iwj); rm(iwj)
}

# KLD

savefig("M_EVR_KLD", width = 12, height = 10, toplines = 0.8, type = "png")
plot(1:20, apply(M_compositions_clr_EVR_point_err[,1,], 1, mean), type = "l", col = 1, lty = 1, 
     ylim = c(0, 0.11), xlab = "", ylab = "KLD", main = "Male data")
lines(1:20, apply(M_cdM_EVR_point_err[,1,], 1, mean), col = 2, lty = 2)
lines(1:20, apply(M_compositions_clr_K6_point_err[,1,], 1, mean), col = 3, lty = 3)
lines(1:20, apply(M_cdM_K6_point_err[,1,], 1, mean), col = 4, lty = 4)
dev.off()

# JSD

savefig("M_EVR_JSD", width = 12, height = 10, toplines = 0.8, type = "png")
plot(1:20, apply(M_compositions_clr_EVR_point_err[,2,], 1, mean), type = "l", col = 1, lty = 1, 
     ylim = c(0, 0.03), xlab = "Forecast horizon", ylab = "JSD", main = "")
lines(1:20, apply(M_cdM_EVR_point_err[,2,], 1, mean), col = 2, lty = 2)
lines(1:20, apply(M_compositions_clr_K6_point_err[,2,], 1, mean), col = 3, lty = 3)
lines(1:20, apply(M_cdM_K6_point_err[,2,], 1, mean), col = 4, lty = 4)
dev.off()

