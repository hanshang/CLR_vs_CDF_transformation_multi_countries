# data_mat: data matrix
# fh: forecast horizon
# fmethod: forecasting method
# ncomp_selection: EVR or K = 6

cdf_transformation_out <- function(data_mat, horizons, fmethod, ncomp_selection)
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
                                      h = horizons, method = fmethod)
    
    data_cumsum_logit_fore_add_diff = matrix(NA, n_age, horizons)
    for(iw in 1:horizons)
    {
        data_cumsum_logit_fore_add = c(invlogit(data_cumsum_logit_fore$mean$y[,iw]), 1)
        data_cumsum_logit_fore_add_diff[,iw] = c(data_cumsum_logit_fore_add[1], diff(data_cumsum_logit_fore_add))
        rm(data_cumsum_logit_fore_add); rm(iw)
    }
    return(data_cumsum_logit_fore_add_diff * 10^5)
}

# Work out dx

fore_out_dx_F = fore_out_dx_M = array(NA, dim = c(111, 50, n_country), dimnames = list(0:110, 1:50, country))
for(iwj in 1:n_country)
{
    fore_out_dx_F[,,iwj] = cdf_transformation_out(data_mat = get(overall_female_pop[iwj]), horizons = 50, fmethod = "ets", 
                                 ncomp_selection = "provide")

    fore_out_dx_M[,,iwj] = cdf_transformation_out(data_mat = get(overall_male_pop[iwj]), horizons = 50, fmethod = "ets", 
                                                  ncomp_selection = "provide")
    print(iwj); rm(iwj)
}

# Work out lx

fore_out_lx_F = fore_out_lx_M = array(NA, dim = c(111, 50, n_country), dimnames = list(0:110, 1:50, country))
for(iw in 1:n_country)
{
    for(ij in 1:50)
    {
        for(ik in 1:111)
        {
            fore_out_lx_F[ik,ij,iw] = 10^5 - sum(fore_out_dx_F[1:ik, ij, iw])
            fore_out_lx_M[ik,ij,iw] = 10^5 - sum(fore_out_dx_M[1:ik, ij, iw])
        }
    }
}

# Work out survival probability px

fore_out_px_F = fore_out_px_M = array(NA, dim = c(111, 50, n_country), dimnames = list(0:110, 1:50, country))
for(iw in 1:n_country)
{
    for(ij in 1:50)
    {
        for(ik in 1:111)
        {
            fore_out_px_F[,ij,iw] = 1 - round(fore_out_dx_F[,ij,iw]/c(10^5, fore_out_lx_F[1:110,ij,iw]), 4)
            fore_out_px_M[,ij,iw] = 1 - round(fore_out_dx_M[,ij,iw]/c(10^5, fore_out_lx_M[1:110,ij,iw]), 4)
        }
    }
}

AnnuityPrice_point <- function(y.predict, age, maturity, inRate)
{
    if(age < 60 || age > 110) return(print("age is outside range"))
    if(age + maturity > 111) return(print("NA"))
    
    surv_prob = vector("numeric", maturity)
    for(iw in 1:maturity)
    {
        surv_prob[iw] = y.predict[age - 60 + iw]
    }
    surv_curve = cumprod(surv_prob)
    
    annuity.price = 0
    for(iw in 1:maturity)
    {
        annuity.price = annuity.price + exp(-inRate * iw) * surv_curve[iw]
    }
    return(round(annuity.price, 4))
}


# define ages and maturities

ages = seq(60, 105, by = 5)
maturities = seq(5, 30, by = 5)

interest_rates <- c(
  Australia = 4.10,
  Austria = 2.25,
  Belgium = 2.25,
  Bulgaria = 2.24,
  Canada = 4.50,
  Czech_Republic = 3.50,
  Denmark = 2.25,
  Finland = 2.25,
  France = 2.25,
  Hungary = 6.50,
  Iceland = 7.75,
  Ireland = 2.25,
  Italy = 2.25,
  Japan = 0.50,
  Netherlands = 2.25,
  Norway = 4.50,
  New_Zealand = 5.50,
  Portugal = 2.25,
  Slovakia = 2.25,
  Spain = 2.25,
  Sweden = 4.00,
  Switzerland = 1.75,
  United_Kingdom = 5.25,
  United_States = 5.25
)

annuities_price_F = annuities_price_M = array(NA, dim = c(length(ages), length(maturities), n_country), dimnames = list(ages, maturities, country))
for(ik in 1:n_country)
{
    for(iw in 1:length(ages))
    {
        for(ij in 1:length(maturities))
        {
            annuities_price_F[iw,ij,ik] = try(AnnuityPrice_point(y.predict = diag(fore_out_px_F[61:111,,ik]), age = ages[iw],
                                            maturity = maturities[ij], inRate = interest_rates[ik]/100), silent = TRUE)
            
            annuities_price_M[iw,ij,ik] = try(AnnuityPrice_point(y.predict = diag(fore_out_px_M[61:111,,ik]), age = ages[iw],
                                            maturity = maturities[ij], inRate = interest_rates[ik]/100), silent = TRUE)
        }
    }
}

###############
# save figures
###############

# entry age 60, maturity 5 years

savefig("annuity_price_F", width = 12, height = 10, toplines = 0.5, type = "png")
dotchart(as.numeric(annuities_price_F[1,1,]), labels = country, main = "Female", cex = 0.7, 
         xlim = c(3.86, 4.88))
dev.off()

savefig("annuity_price_M", width = 12, height = 10, toplines = 0.5, type = "png")
dotchart(as.numeric(annuities_price_M[1,1,]), labels = country, main = "Male", cex = 0.7,
         xlim = c(3.86, 4.88))
dev.off()

# entry age 70, maturity 20 years

savefig("annuity_price_F_2", width = 12, height = 10, toplines = 0.5, type = "png")
dotchart(as.numeric(annuities_price_F[3,4,]), labels = country, main = "Female", cex = 0.7)
dev.off()

savefig("annuity_price_M_2", width = 12, height = 10, toplines = 0.5, type = "png")
dotchart(as.numeric(annuities_price_M[3,4,]), labels = country, main = "Male", cex = 0.7)
dev.off()

