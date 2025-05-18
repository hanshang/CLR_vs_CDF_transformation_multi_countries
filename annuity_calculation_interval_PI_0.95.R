#################
### alpha = 0.95
#################

## Female

# Work out age distribution of death count dx

F_cdf_K6_int_lb_0.95 = F_cdf_K6_int_ub_0.95 = array(NA, dim = c(111, 50, n_country), 
                                                    dimnames = list(0:110, 1:50, country))
for(iwj in 1:n_country)
{
    for(iwk in 1:50)
    {
        dum = den_interval_out(data = get(overall_female_pop[iwj]), horizon = iwk, ncomp_order = "provide", 
                               alpha_level = 0.95)
        F_cdf_K6_int_lb_0.95[,iwk,iwj] = dum$fore_val_lb                               
        F_cdf_K6_int_ub_0.95[,iwk,iwj] = dum$fore_val_ub
        rm(dum); rm(iwk)
    }
    print(iwj); rm(iwj)
}

F_cdf_K6_int_lb_0.95_new = replace(F_cdf_K6_int_lb_0.95, which(F_cdf_K6_int_lb_0.95 < 0), 0)

# Work out survival probability px 

fore_out_px_F_lb_0.95 = fore_out_px_F_ub_0.95 = array(NA, dim = c(111, 50, n_country), 
                                                      dimnames = list(0:110, 1:50, country))
for(iw in 1:n_country)
{
    for(ij in 1:50)
    {
        fore_out_px_F_lb_0.95[,ij,iw] = 1 - LifeTable(0:110, dx = F_cdf_K6_int_lb_0.95_new[,ij,iw])$lt$qx
        fore_out_px_F_ub_0.95[,ij,iw] = 1 - LifeTable(0:110, dx = F_cdf_K6_int_ub_0.95[,ij,iw])$lt$qx
        rm(ij)
    }
    rm(iw)
}

# annuities

annuities_price_F_lb_0.95 = annuities_price_F_ub_0.95 = array(NA, dim = c(length(ages), length(maturities), n_country), 
                                                    dimnames = list(ages, maturities, country))
for(ik in 1:n_country)
{
    for(iw in 1:length(ages))
    {
        for(ij in 1:length(maturities))
        {
            dum_1 = try(AnnuityPrice_point(y.predict = diag(fore_out_px_F_lb_0.95[61:111,,ik]), age = ages[iw],
                                          maturity = maturities[ij], inRate = interest_rates[ik]/100), silent = TRUE)
            
            dum_2 = try(AnnuityPrice_point(y.predict = diag(fore_out_px_F_ub_0.95[61:111,,ik]), age = ages[iw],
                                          maturity = maturities[ij], inRate = interest_rates[ik]/100), silent = TRUE)
            
            annuities_price_F_lb_0.95[iw,ij,ik] = pmin(dum_1, dum_2)
            annuities_price_F_ub_0.95[iw,ij,ik] = pmax(dum_1, dum_2)
            rm(ij); rm(dum_1); rm(dum_2)
        }
        rm(iw)
    }
    rm(ik)
}

## Male

# Work out age distribution of death count dx

M_cdf_K6_int_lb_0.95 = M_cdf_K6_int_ub_0.95 = array(NA, dim = c(111, 50, n_country), 
                                                    dimnames = list(0:110, 1:50, country))
for(iwj in 1:n_country)
{
    for(iwk in 1:50)
    {
        dum = den_interval_out(data = get(overall_male_pop[iwj]), horizon = iwk, ncomp_order = "provide", 
                               alpha_level = 0.95)
        M_cdf_K6_int_lb_0.95[,iwk,iwj] = dum$fore_val_lb                               
        M_cdf_K6_int_ub_0.95[,iwk,iwj] = dum$fore_val_ub
        rm(dum); rm(iwk)
    }
    print(iwj); rm(iwj)
}

M_cdf_K6_int_lb_0.95_new = replace(M_cdf_K6_int_lb_0.95, which(M_cdf_K6_int_lb_0.95 < 0), 0)

# Work out survival probability px

fore_out_px_M_lb_0.95 = fore_out_px_M_ub_0.95 = array(NA, dim = c(111, 50, n_country), dimnames = list(0:110, 1:50, country))
for(iw in 1:n_country)
{
    for(ij in 1:50)
    {
        fore_out_px_M_lb_0.95[,ij,iw] = 1 - LifeTable(0:110, dx = M_cdf_K6_int_lb_0.95_new[,ij,iw])$lt$qx
        fore_out_px_M_ub_0.95[,ij,iw] = 1 - LifeTable(0:110, dx = M_cdf_K6_int_ub_0.95[,ij,iw])$lt$qx
        rm(ij)
    }
    rm(iw)
}

# annuities

annuities_price_M_lb_0.95 = annuities_price_M_ub_0.95 = array(NA, dim = c(length(ages), length(maturities), n_country), 
                                                    dimnames = list(ages, maturities, country))
for(ik in 1:n_country)
{
    for(iw in 1:length(ages))
    {
        for(ij in 1:length(maturities))
        {
            dum_1 = try(AnnuityPrice_point(y.predict = diag(fore_out_px_M_lb_0.95[61:111,,ik]), age = ages[iw],
                                           maturity = maturities[ij], inRate = interest_rates[ik]/100), silent = TRUE)
            
            dum_2 = try(AnnuityPrice_point(y.predict = diag(fore_out_px_M_ub_0.95[61:111,,ik]), age = ages[iw],
                                           maturity = maturities[ij], inRate = interest_rates[ik]/100), silent = TRUE)
            
            annuities_price_M_lb_0.95[iw,ij,ik] = pmin(dum_1, dum_2)
            annuities_price_M_ub_0.95[iw,ij,ik] = pmax(dum_1, dum_2)
            rm(ij); rm(dum_1); rm(dum_2)
        }
        rm(iw)
    }
    rm(ik)
}

################################
# entry age 60, 5 year maturity
################################

# Female

savefig("Fig_4a", width = 12, height = 10, toplines = 0.8, type = "png")
dotchart(as.numeric(annuities_price_F[1,1,]), labels = country, main = "Female", cex = 0.7, 
         xlim = c(3.86, 4.88))
points(as.numeric(annuities_price_F_lb[1,1,]), 1:24, col = "red", pch = "(")
points(as.numeric(annuities_price_F_ub[1,1,]), 1:24, col = "red", pch = ")")

points(as.numeric(annuities_price_F_lb_0.95[1,1,]), 1:24, col = "blue", pch = "(")
points(as.numeric(annuities_price_F_ub_0.95[1,1,]), 1:24, col = "blue", pch = ")")
legend("topleft", c("Point forecast", "80% lower bound", "80% upper bound", "95% lower bound", "95% upper bound"), 
       col = c(1, 2, 2, 4, 4), pch = c("o", "(", ")", "(", ")"), cex = 0.6)
dev.off()

# Male

savefig("Fig_4b", width = 12, height = 10, toplines = 0.8, type = "png")
dotchart(as.numeric(annuities_price_M[1,1,]), labels = country, main = "Male", cex = 0.7, 
         xlim = c(3.86, 4.88))
points(as.numeric(annuities_price_M_lb[1,1,]), 1:24, col = "red", pch = "(")
points(as.numeric(annuities_price_M_ub[1,1,]), 1:24, col = "red", pch = ")")

points(as.numeric(annuities_price_M_lb_0.95[1,1,]), 1:24, col = "blue", pch = "(")
points(as.numeric(annuities_price_M_ub_0.95[1,1,]), 1:24, col = "blue", pch = ")")
dev.off()

################################
# entry age 70, 20 year maturity
################################

# Female

savefig("Fig_4c", width = 12, height = 10, toplines = 0.8, type = "png")
dotchart(as.numeric(annuities_price_F[3,4,]), labels = country, main = "", cex = 0.7,
         xlim = c(7, 17))
points(as.numeric(annuities_price_F_lb[3,4,]), 1:24, col = "red", pch = "(")
points(as.numeric(annuities_price_F_ub[3,4,]), 1:24, col = "red", pch = ")")

points(as.numeric(annuities_price_F_lb_0.95[3,4,]), 1:24, col = "blue", pch = "(")
points(as.numeric(annuities_price_F_ub_0.95[3,4,]), 1:24, col = "blue", pch = ")")
dev.off()

# Male

savefig("Fig_4d", width = 12, height = 10, toplines = 0.8, type = "png")
dotchart(as.numeric(annuities_price_M[3,4,]), labels = country, main = "", cex = 0.7,
         xlim = c(7, 17))
points(as.numeric(annuities_price_M_lb[3,4,]), 1:24, col = "red", pch = "(")
points(as.numeric(annuities_price_M_ub[3,4,]), 1:24, col = "red", pch = ")")

points(as.numeric(annuities_price_M_lb_0.95[3,4,]), 1:24, col = "blue", pch = "(")
points(as.numeric(annuities_price_M_ub_0.95[3,4,]), 1:24, col = "blue", pch = ")")
dev.off()

