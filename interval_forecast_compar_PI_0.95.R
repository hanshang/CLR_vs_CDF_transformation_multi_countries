#########
# female
#########

F_clr_EVR_int_err_alpha_0.95 = F_cdf_EVR_int_err_alpha_0.95 = F_clr_K6_int_err_alpha_0.95 = F_cdf_K6_int_err_alpha_0.95 = array(NA, dim = c(19, 3, n_country),
                                                                                    dimnames = list(1:19, c("ECP", "CPD", "score"), country))
F_EVR_tune_para_alpha_0.95 = F_EVR_tune_obj_alpha_0.95 = F_K6_tune_para_alpha_0.95 = F_K6_tune_obj_alpha_0.95 = array(NA, dim = c(19, 2, n_country),
                                                                          dimnames = list(1:19, c("CLR", "CDF"), country))
for(iwj in 1:n_country)
{
    for(iwk in 1:19)
    {
        dum = den_interval(data = get(overall_female_pop[iwk]), horizon = iwk, fore_method = "clr", 
                           ncomp_order = "EVR", alpha_level = 0.95)
        F_clr_EVR_int_err_alpha_0.95[iwk,,iwj] = dum$int_err
        F_EVR_tune_para_alpha_0.95[iwk,1,iwj] = dum$tune_para_find
        F_EVR_tune_obj_alpha_0.95[iwk,1,iwj] = dum$tune_para_find_obj
        rm(dum)
        
        dum = den_interval(data = get(overall_female_pop[iwk]), horizon = iwk, fore_method = "cdf", 
                           ncomp_order = "EVR", alpha_level = 0.95)
        F_cdf_EVR_int_err_alpha_0.95[iwk,,iwj] = dum$int_err
        F_EVR_tune_para_alpha_0.95[iwk,2,iwj] = dum$tune_para_find
        F_EVR_tune_obj_alpha_0.95[iwk,2,iwj] = dum$tune_para_find_obj
        
        
        dum = den_interval(data = get(overall_female_pop[iwk]), horizon = iwk, fore_method = "clr", 
                           ncomp_order = "provide", alpha_level = 0.95)
        F_clr_K6_int_err_alpha_0.95[iwk,,iwj] = dum$int_err
        F_K6_tune_para_alpha_0.95[iwk,1,iwj] = dum$tune_para_find
        F_K6_tune_obj_alpha_0.95[iwk,1,iwj] = dum$tune_para_find_obj
        rm(dum)
        
        dum = den_interval(data = get(overall_female_pop[iwk]), horizon = iwk, fore_method = "cdf", 
                           ncomp_order = "provide", alpha_level = 0.95)
        F_cdf_K6_int_err_alpha_0.95[iwk,,iwj] = dum$int_err
        F_K6_tune_para_alpha_0.95[iwk,2,iwj] = dum$tune_para_find
        F_K6_tune_obj_alpha_0.95[iwk,2,iwj] = dum$tune_para_find_obj
        print(iwk); rm(iwk); rm(dum)
    }
    print(iwj); rm(iwj)
}

# CPD

CPD_F_EVR_array_alpha_0.95 = CPD_M_EVR_array_alpha_0.95 = array(NA, dim = c(19, 24, 4))
CPD_F_EVR_array_alpha_0.95[,,1] = F_clr_EVR_int_err_alpha_0.95[,2,]
CPD_F_EVR_array_alpha_0.95[,,2] = F_cdf_EVR_int_err_alpha_0.95[,2,]
CPD_F_EVR_array_alpha_0.95[,,3] = F_clr_K6_int_err_alpha_0.95[,2,]
CPD_F_EVR_array_alpha_0.95[,,4] = F_cdf_K6_int_err_alpha_0.95[,2,]

CPD_M_EVR_array_alpha_0.95[,,1] = M_clr_EVR_int_err_alpha_0.95[,2,]
CPD_M_EVR_array_alpha_0.95[,,2] = M_cdf_EVR_int_err_alpha_0.95[,2,]
CPD_M_EVR_array_alpha_0.95[,,3] = M_clr_K6_int_err_alpha_0.95[,2,]
CPD_M_EVR_array_alpha_0.95[,,4] = M_cdf_K6_int_err_alpha_0.95[,2,]

CPD_F_EVR_array_alpha_0.95_mean = apply(CPD_F_EVR_array_alpha_0.95, c(1, 3), mean)
CPD_M_EVR_array_alpha_0.95_mean = apply(CPD_M_EVR_array_alpha_0.95, c(1, 3), mean)
colnames(CPD_F_EVR_array_alpha_0.95_mean) = c("CLR (EVR)", "CDF (EVR)", "CLR (K=6)", "CDF (K=6)")
rownames(CPD_F_EVR_array_alpha_0.95_mean) = 1:19

# score

score_F_EVR_array_alpha_0.95 = score_M_EVR_array_alpha_0.95 = array(NA, dim = c(19, 24, 4))
score_F_EVR_array_alpha_0.95[,,1] = F_clr_EVR_int_err_alpha_0.95[,3,]
score_F_EVR_array_alpha_0.95[,,2] = F_cdf_EVR_int_err_alpha_0.95[,3,]
score_F_EVR_array_alpha_0.95[,,3] = F_clr_K6_int_err_alpha_0.95[,3,]
score_F_EVR_array_alpha_0.95[,,4] = F_cdf_K6_int_err_alpha_0.95[,3,]

score_M_EVR_array_alpha_0.95[,,1] = M_clr_EVR_int_err_alpha_0.95[,3,]
score_M_EVR_array_alpha_0.95[,,2] = M_cdf_EVR_int_err_alpha_0.95[,3,]
score_M_EVR_array_alpha_0.95[,,3] = M_clr_K6_int_err_alpha_0.95[,3,]
score_M_EVR_array_alpha_0.95[,,4] = M_cdf_K6_int_err_alpha_0.95[,3,]

score_F_EVR_array_alpha_0.95_mean = apply(score_F_EVR_array_alpha_0.95, c(1, 3), mean)
score_M_EVR_array_alpha_0.95_mean = apply(score_M_EVR_array_alpha_0.95, c(1, 3), mean)

#######
# male
#######

M_clr_EVR_int_err_alpha_0.95 = M_cdM_EVR_int_err_alpha_0.95 = M_clr_K6_int_err_alpha_0.95 = M_cdM_K6_int_err_alpha_0.95 = array(NA, dim = c(19, 3, n_country),
                                                                                    dimnames = list(1:19, c("ECP", "CPD", "score"), country))
M_EVR_tune_para_alpha_0.95 = M_EVR_tune_obj_alpha_0.95 = M_K6_tune_para_alpha_0.95 = M_K6_tune_obj_alpha_0.95 = array(NA, dim = c(19, 2, n_country),
                                                                          dimnames = list(1:19, c("CLR", "CDF"), country))
for(iwj in 1:n_country)
{
    for(iwk in 1:19)
    {
        dum = den_interval(data = get(overall_male_pop[iwk]), horizon = iwk, fore_method = "clr", 
                           ncomp_order = "EVR", alpha_level = 0.95)
        M_clr_EVR_int_err_alpha_0.95[iwk,,iwj] = dum$int_err
        M_EVR_tune_para_alpha_0.95[iwk,1,iwj] = dum$tune_para_find
        M_EVR_tune_obj_alpha_0.95[iwk,1,iwj] = dum$tune_para_find_obj
        rm(dum)
        
        dum = den_interval(data = get(overall_male_pop[iwk]), horizon = iwk, fore_method = "cdf", 
                           ncomp_order = "EVR", alpha_level = 0.95)
        M_cdM_EVR_int_err_alpha_0.95[iwk,,iwj] = dum$int_err
        M_EVR_tune_para_alpha_0.95[iwk,2,iwj] = dum$tune_para_find
        M_EVR_tune_obj_alpha_0.95[iwk,2,iwj] = dum$tune_para_find_obj
        
        
        dum = den_interval(data = get(overall_male_pop[iwk]), horizon = iwk, fore_method = "clr", 
                           ncomp_order = "provide", alpha_level = 0.95)
        M_clr_K6_int_err_alpha_0.95[iwk,,iwj] = dum$int_err
        M_K6_tune_para_alpha_0.95[iwk,1,iwj] = dum$tune_para_find
        M_K6_tune_obj_alpha_0.95[iwk,1,iwj] = dum$tune_para_find_obj
        rm(dum)
        
        dum = den_interval(data = get(overall_male_pop[iwk]), horizon = iwk, fore_method = "cdf", 
                           ncomp_order = "provide", alpha_level = 0.95)
        M_cdM_K6_int_err_alpha_0.95[iwk,,iwj] = dum$int_err
        M_K6_tune_para_alpha_0.95[iwk,2,iwj] = dum$tune_para_find
        M_K6_tune_obj_alpha_0.95[iwk,2,iwj] = dum$tune_para_find_obj
        print(iwk); rm(iwk); rm(dum)
    }
    print(iwj); rm(iwj)
}

