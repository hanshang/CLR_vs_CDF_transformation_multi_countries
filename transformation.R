######
# clr
######

# data: n by p data matrix
# fh: forecast horizon
# fmethod: univariate time-series forecasting method
# ncomp_selection: EVR or K=6

compositions_clr_transformation <- function(data, fh, fmethod, ncomp_selection)
{
    n_age = ncol(data)
    clr_data = matrix(clr(data), nrow(data), ncol(data))
    
    SVD_decomp = svd(clr_data)
    if(ncomp_selection == "EVR")
    {
        ncomp = select_K(0.001, eigenvalue = SVD_decomp$d^2)
    }
    else if(ncomp_selection == "provide")
    {
        ncomp = 6
    }
    basis = SVD_decomp$v[,1:ncomp]
    score = t(basis) %*% t(clr_data)
    
    score_fore = matrix(NA, ncomp, 1)
    for(ik in 1:ncomp)
    {
        score_fore[ik,] = forecast(ets(as.numeric(score[ik,])), h = fh)$mean[fh]
    }
    fore_val = basis %*% score_fore
    
    clr_inv_data = as.numeric(clrInv(as.numeric(fore_val))) * 10^5
    return(clr_inv_data)
}

easyCoDa_clr_transformation <- function(data, fh, fmethod, ncomp_selection)
{
    n_age = ncol(data)
    if(any(data == 0))
    {
        data_set = replace(data, which(data == 0), 10^-5)
    }
    else
    {
        data_set = data
    }
    h_x_t = CLR(data_set)$LR
    
    SVD_decomp = svd(h_x_t)
    if(ncomp_selection == "EVR")
    {
        ncomp = select_K(tau = 0.001, eigenvalue = SVD_decomp$d^2)
    }
    else if(ncomp_selection == "provide")
    {
        ncomp = 6
    }
    basis = SVD_decomp$v[,1:ncomp]
    score = t(basis) %*% t(h_x_t)
    
    score_fore = matrix(NA, ncomp, 1)
    for(ik in 1:ncomp)
    {
      score_fore[ik,] = forecast(ets(as.numeric(score[ik,])), h = fh)$mean[fh]
    }
    fore_val = basis %*% score_fore
    clr_inv_data = invCLR(t(fore_val)) * 10^5
    return(clr_inv_data)
}

######
# CDF
######

# data: n by p data matrix
# fh: forecast horizon
# fmethod: univariate time series method
# ncomp_method: way for selecting the number of components

cdf_transformation <- function(data_mat, fh, fmethod, ncomp_selection)
{
    if(any(rowSums(data_mat) > 1))
    {
        data = data_mat/10^5
    }
    else
    {
        data = data_mat
    }
    n_age = ncol(data)
    data_cumsum_dum = matrix(NA, nrow(data), ncol(data))
    for(ij in 1:nrow(data))
    {
        data_cumsum_dum[ij,] = cumsum(data[ij,])
        rm(ij)
    }
    
    # check if any cumsum values equal to 0
    
    if(any(data_cumsum_dum == 0))
    {
        data_cumsum = replace(data_cumsum_dum, which(data_cumsum_dum == 0), 10^-5)
    }
    else
    {
        data_cumsum = data_cumsum_dum
    }
    rm(data_cumsum_dum)
    
    # logit transformation
    
    data_cumsum_logit = matrix(NA, nrow(data), (ncol(data) - 1))
    for(ij in 1:nrow(data))
    {
        data_cumsum_logit[ij,] = logit(data_cumsum[ij, 1:(ncol(data) - 1)])
        rm(ij)
    }
    rm(data_cumsum)

    # fitting a functional time series forecasting method
    
    if(ncomp_selection == "EVR")
    {
        ncomp = select_K(tau = 10^-3, eigenvalue = (svd(data_cumsum_logit)$d)^2)
    }
    else if(ncomp_selection == "provide")
    {
        ncomp = 6
    }
    data_cumsum_logit_fore = forecast(ftsm(fts(1:(n_age - 1), t(data_cumsum_logit)), order = ncomp), 
                                      h = fh, method = fmethod)
    
    data_cumsum_logit_fore_add = c(invlogit(data_cumsum_logit_fore$mean$y[,fh]), 1)
    data_cumsum_logit_fore_add_diff = c(data_cumsum_logit_fore_add[1], diff(data_cumsum_logit_fore_add))
    return(data_cumsum_logit_fore_add_diff * 10^5)
}

