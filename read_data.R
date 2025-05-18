##########################
# set a working directory
##########################

setwd("~/Dropbox/Todos/LTDC_transformation/data")

age = 0:110

country = c("AUS", "AUT", "BEL", "BGR", "CAN", "CZE", "DEN", "FIN", "FRA", "HUN", "ICE", "IRE", "ITA", 
            "JPN", "NLD", "NOR", "NZ", "PRT", "SLO", "SPA", "SWE", "SWI", "UK", "USA")
n_country = length(country)

overall_female_pop = overall_male_pop = c()
for(iwk in 1:n_country)
{
    overall_female_pop[iwk] = paste(country[iwk], "_female_pop", sep="")
    overall_male_pop[iwk]   = paste(country[iwk], "_male_pop",   sep="")
    rm(iwk)
}

# AUS

AUS_year = 1921:2021
AUS_n_year = length(AUS_year)
AUS_female_qx = t(matrix(read.table("lt_AUS_F.txt", header = TRUE, skip = 2)[,"qx"], 111, AUS_n_year))
AUS_male_qx   = t(matrix(read.table("lt_AUS_M.txt", header = TRUE, skip = 2)[,"qx"], 111, AUS_n_year))

AUS_n_col = ncol(AUS_female_qx)
AUS_n_row = nrow(AUS_female_qx)
AUS_female_pop = AUS_male_pop = matrix(NA, AUS_n_row, AUS_n_col)
for(ij in 1:AUS_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:AUS_n_col)
    {
        AUS_female_pop[ij,ik] = AUS_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - AUS_female_pop[ij,ik]

        AUS_male_pop[ij,ik] = AUS_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - AUS_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(AUS_female_pop) = rownames(AUS_male_pop) = AUS_year
colnames(AUS_female_pop) = colnames(AUS_male_pop) = age

# AUT

AUT_year = 1947:2023
AUT_n_year = length(AUT_year)
AUT_female_qx = t(matrix(read.table("lt_AUT_F.txt", header = TRUE, skip = 2)[,"qx"], 111, AUT_n_year))
AUT_male_qx   = t(matrix(read.table("lt_AUT_M.txt", header = TRUE, skip = 2)[,"qx"], 111, AUT_n_year))

AUT_n_col = ncol(AUT_female_qx)
AUT_n_row = nrow(AUT_female_qx)
AUT_female_pop = AUT_male_pop = matrix(NA, AUT_n_row, AUT_n_col)
for(ij in 1:AUT_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:AUT_n_col)
    {
        AUT_female_pop[ij,ik] = AUT_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - AUT_female_pop[ij,ik]
        
        AUT_male_pop[ij,ik] = AUT_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - AUT_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(AUT_female_pop) = rownames(AUT_male_pop) = AUT_year
colnames(AUT_female_pop) = colnames(AUT_male_pop) = age

# BEL

BEL_year = 1919:2023
BEL_n_year = length(BEL_year)
BEL_female_qx = t(matrix(as.numeric(read.table("lt_BEL_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, BEL_n_year))
BEL_male_qx   = t(matrix(as.numeric(read.table("lt_BEL_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, BEL_n_year))

BEL_n_col = ncol(BEL_female_qx)
BEL_n_row = nrow(BEL_female_qx)
BEL_female_pop = BEL_male_pop = matrix(NA, BEL_n_row, BEL_n_col)
for(ij in 1:BEL_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:BEL_n_col)
    {
        BEL_female_pop[ij,ik] = BEL_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - BEL_female_pop[ij,ik]
        
        BEL_male_pop[ij,ik] = BEL_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - BEL_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(BEL_female_pop) = rownames(BEL_male_pop) = BEL_year
colnames(BEL_female_pop) = colnames(BEL_male_pop) = age

# BGR

BGR_year = 1947:2021
BGR_n_year = length(BGR_year)
BGR_female_qx = t(matrix(as.numeric(read.table("lt_BGR_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, BGR_n_year))
BGR_male_qx   = t(matrix(as.numeric(read.table("lt_BGR_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, BGR_n_year))

BGR_n_col = ncol(BGR_female_qx)
BGR_n_row = nrow(BGR_female_qx)
BGR_female_pop = BGR_male_pop = matrix(NA, BGR_n_row, BGR_n_col)
for(ij in 1:BGR_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:BGR_n_col)
    {
        BGR_female_pop[ij,ik] = BGR_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - BGR_female_pop[ij,ik]
        
        BGR_male_pop[ij,ik] = BGR_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - BGR_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(BGR_female_pop) = rownames(BGR_male_pop) = BGR_year
colnames(BGR_female_pop) = colnames(BGR_male_pop) = age

# CAN

CAN_year = 1921:2022
CAN_n_year = length(CAN_year)
CAN_female_qx = t(matrix(as.numeric(read.table("lt_CAN_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, CAN_n_year))
CAN_male_qx   = t(matrix(as.numeric(read.table("lt_CAN_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, CAN_n_year))

CAN_n_col = ncol(CAN_female_qx)
CAN_n_row = nrow(CAN_female_qx)
CAN_female_pop = CAN_male_pop = matrix(NA, CAN_n_row, CAN_n_col)
for(ij in 1:CAN_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:CAN_n_col)
    {
        CAN_female_pop[ij,ik] = CAN_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - CAN_female_pop[ij,ik]
        
        CAN_male_pop[ij,ik] = CAN_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - CAN_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(CAN_female_pop) = rownames(CAN_male_pop) = CAN_year
colnames(CAN_female_pop) = colnames(CAN_male_pop) = age

# CZE

CZE_year = 1950:2021
CZE_n_year = length(CZE_year)
CZE_female_qx = t(matrix(as.numeric(read.table("lt_CZE_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, CZE_n_year))
CZE_male_qx   = t(matrix(as.numeric(read.table("lt_CZE_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, CZE_n_year))

CZE_n_col = ncol(CZE_female_qx)
CZE_n_row = nrow(CZE_female_qx)
CZE_female_pop = CZE_male_pop = matrix(NA, CZE_n_row, CZE_n_col)
for(ij in 1:CZE_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:CZE_n_col)
    {
        CZE_female_pop[ij,ik] = CZE_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - CZE_female_pop[ij,ik]
        
        CZE_male_pop[ij,ik] = CZE_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - CZE_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(CZE_female_pop) = rownames(CZE_male_pop) = CZE_year
colnames(CZE_female_pop) = colnames(CZE_male_pop) = age

# DEN

DEN_year = 1835:2024
DEN_n_year = length(DEN_year)
DEN_female_qx = t(matrix(as.numeric(read.table("lt_DEN_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, DEN_n_year))
DEN_male_qx   = t(matrix(as.numeric(read.table("lt_DEN_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, DEN_n_year))

DEN_n_col = ncol(DEN_female_qx)
DEN_n_row = nrow(DEN_female_qx)
DEN_female_pop = DEN_male_pop = matrix(NA, DEN_n_row, DEN_n_col)
for(ij in 1:DEN_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:DEN_n_col)
    {
        DEN_female_pop[ij,ik] = DEN_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - DEN_female_pop[ij,ik]
        
        DEN_male_pop[ij,ik] = DEN_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - DEN_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(DEN_female_pop) = rownames(DEN_male_pop) = DEN_year
colnames(DEN_female_pop) = colnames(DEN_male_pop) = age

# FIN

FIN_year = 1878:2023
FIN_n_year = length(FIN_year)
FIN_female_qx = t(matrix(as.numeric(read.table("lt_FIN_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, FIN_n_year))
FIN_male_qx   = t(matrix(as.numeric(read.table("lt_FIN_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, FIN_n_year))

FIN_n_col = ncol(FIN_female_qx)
FIN_n_row = nrow(FIN_female_qx)
FIN_female_pop = FIN_male_pop = matrix(NA, FIN_n_row, FIN_n_col)
for(ij in 1:FIN_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:FIN_n_col)
    {
        FIN_female_pop[ij,ik] = FIN_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - FIN_female_pop[ij,ik]
        
        FIN_male_pop[ij,ik] = FIN_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - FIN_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(FIN_female_pop) = rownames(FIN_male_pop) = FIN_year
colnames(FIN_female_pop) = colnames(FIN_male_pop) = age

# FRA

FRA_year = 1816:2022
FRA_n_year = length(FRA_year)
FRA_female_qx = t(matrix(as.numeric(read.table("lt_FRA_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, FRA_n_year))
FRA_male_qx   = t(matrix(as.numeric(read.table("lt_FRA_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, FRA_n_year))

FRA_n_col = ncol(FRA_female_qx)
FRA_n_row = nrow(FRA_female_qx)
FRA_female_pop = FRA_male_pop = matrix(NA, FRA_n_row, FRA_n_col)
for(ij in 1:FRA_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:FRA_n_col)
    {
        FRA_female_pop[ij,ik] = FRA_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - FRA_female_pop[ij,ik]
        
        FRA_male_pop[ij,ik] = FRA_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - FRA_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(FRA_female_pop) = rownames(FRA_male_pop) = FRA_year
colnames(FRA_female_pop) = colnames(FRA_male_pop) = age

# HUN

HUN_year = 1950:2020
HUN_n_year = length(HUN_year)
HUN_female_qx = t(matrix(as.numeric(read.table("lt_HUN_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, HUN_n_year))
HUN_male_qx   = t(matrix(as.numeric(read.table("lt_HUN_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, HUN_n_year))

HUN_n_col = ncol(HUN_female_qx)
HUN_n_row = nrow(HUN_female_qx)
HUN_female_pop = HUN_male_pop = matrix(NA, HUN_n_row, HUN_n_col)
for(ij in 1:HUN_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:HUN_n_col)
    {
        HUN_female_pop[ij,ik] = HUN_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - HUN_female_pop[ij,ik]
        
        HUN_male_pop[ij,ik] = HUN_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - HUN_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(HUN_female_pop) = rownames(HUN_male_pop) = HUN_year
colnames(HUN_female_pop) = colnames(HUN_male_pop) = age

# ICE

ICE_year = 1838:2023
ICE_n_year = length(ICE_year)
ICE_female_qx = t(matrix(as.numeric(read.table("lt_ICE_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, ICE_n_year))
ICE_male_qx   = t(matrix(as.numeric(read.table("lt_ICE_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, ICE_n_year))

ICE_n_col = ncol(ICE_female_qx)
ICE_n_row = nrow(ICE_female_qx)
ICE_female_pop = ICE_male_pop = matrix(NA, ICE_n_row, ICE_n_col)
for(ij in 1:ICE_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:ICE_n_col)
    {
        ICE_female_pop[ij,ik] = ICE_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - ICE_female_pop[ij,ik]
        
        ICE_male_pop[ij,ik] = ICE_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - ICE_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(ICE_female_pop) = rownames(ICE_male_pop) = ICE_year
colnames(ICE_female_pop) = colnames(ICE_male_pop) = age

# IRE

IRE_year = 1950:2022
IRE_n_year = length(IRE_year)
IRE_female_qx = t(matrix(as.numeric(read.table("lt_IRE_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, IRE_n_year))
IRE_male_qx   = t(matrix(as.numeric(read.table("lt_IRE_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, IRE_n_year))

IRE_n_col = ncol(IRE_female_qx)
IRE_n_row = nrow(IRE_female_qx)
IRE_female_pop = IRE_male_pop = matrix(NA, IRE_n_row, IRE_n_col)
for(ij in 1:IRE_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:IRE_n_col)
    {
        IRE_female_pop[ij,ik] = IRE_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - IRE_female_pop[ij,ik]
        
        IRE_male_pop[ij,ik] = IRE_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - IRE_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(IRE_female_pop) = rownames(IRE_male_pop) = IRE_year
colnames(IRE_female_pop) = colnames(IRE_male_pop) = age

# ITA

ITA_year = 1872:2022
ITA_n_year = length(ITA_year)
ITA_female_qx = t(matrix(as.numeric(read.table("lt_ITA_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, ITA_n_year))
ITA_male_qx   = t(matrix(as.numeric(read.table("lt_ITA_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, ITA_n_year))

ITA_n_col = ncol(ITA_female_qx)
ITA_n_row = nrow(ITA_female_qx)
ITA_female_pop = ITA_male_pop = matrix(NA, ITA_n_row, ITA_n_col)
for(ij in 1:ITA_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:ITA_n_col)
    {
        ITA_female_pop[ij,ik] = ITA_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - ITA_female_pop[ij,ik]
        
        ITA_male_pop[ij,ik] = ITA_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - ITA_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(ITA_female_pop) = rownames(ITA_male_pop) = ITA_year
colnames(ITA_female_pop) = colnames(ITA_male_pop) = age

# JPN

JPN_year = 1947:2023
JPN_n_year = length(JPN_year)
JPN_female_qx = t(matrix(as.numeric(read.table("lt_JPN_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, JPN_n_year))
JPN_male_qx   = t(matrix(as.numeric(read.table("lt_JPN_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, JPN_n_year))

JPN_n_col = ncol(JPN_female_qx)
JPN_n_row = nrow(JPN_female_qx)
JPN_female_pop = JPN_male_pop = matrix(NA, JPN_n_row, JPN_n_col)
for(ij in 1:JPN_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:JPN_n_col)
    {
        JPN_female_pop[ij,ik] = JPN_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - JPN_female_pop[ij,ik]
        
        JPN_male_pop[ij,ik] = JPN_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - JPN_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(JPN_female_pop) = rownames(JPN_male_pop) = JPN_year
colnames(JPN_female_pop) = colnames(JPN_male_pop) = age

# NLD

NLD_year = 1850:2022
NLD_n_year = length(NLD_year)
NLD_female_qx = t(matrix(as.numeric(read.table("lt_NLD_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, NLD_n_year))
NLD_male_qx   = t(matrix(as.numeric(read.table("lt_NLD_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, NLD_n_year))

NLD_n_col = ncol(NLD_female_qx)
NLD_n_row = nrow(NLD_female_qx)
NLD_female_pop = NLD_male_pop = matrix(NA, NLD_n_row, NLD_n_col)
for(ij in 1:NLD_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:NLD_n_col)
    {
        NLD_female_pop[ij,ik] = NLD_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - NLD_female_pop[ij,ik]
        
        NLD_male_pop[ij,ik] = NLD_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - NLD_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(NLD_female_pop) = rownames(NLD_male_pop) = NLD_year
colnames(NLD_female_pop) = colnames(NLD_male_pop) = age

# NOR

NOR_year = 1846:2023
NOR_n_year = length(NOR_year)
NOR_female_qx = t(matrix(as.numeric(read.table("lt_NOR_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, NOR_n_year))
NOR_male_qx   = t(matrix(as.numeric(read.table("lt_NOR_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, NOR_n_year))

NOR_n_col = ncol(NOR_female_qx)
NOR_n_row = nrow(NOR_female_qx)
NOR_female_pop = NOR_male_pop = matrix(NA, NOR_n_row, NOR_n_col)
for(ij in 1:NOR_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:NOR_n_col)
    {
        NOR_female_pop[ij,ik] = NOR_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - NOR_female_pop[ij,ik]
        
        NOR_male_pop[ij,ik] = NOR_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - NOR_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(NOR_female_pop) = rownames(NOR_male_pop) = NOR_year
colnames(NOR_female_pop) = colnames(NOR_male_pop) = age

# NZ

NZ_year = 1948:2021
NZ_n_year = length(NZ_year)
NZ_female_qx = t(matrix(as.numeric(read.table("lt_NZ_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, NZ_n_year))
NZ_male_qx   = t(matrix(as.numeric(read.table("lt_NZ_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, NZ_n_year))

NZ_n_col = ncol(NZ_female_qx)
NZ_n_row = nrow(NZ_female_qx)
NZ_female_pop = NZ_male_pop = matrix(NA, NZ_n_row, NZ_n_col)
for(ij in 1:NZ_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:NZ_n_col)
    {
        NZ_female_pop[ij,ik] = NZ_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - NZ_female_pop[ij,ik]
        
        NZ_male_pop[ij,ik] = NZ_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - NZ_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(NZ_female_pop) = rownames(NZ_male_pop) = NZ_year
colnames(NZ_female_pop) = colnames(NZ_male_pop) = age

# PRT

PRT_year = 1940:2023
PRT_n_year = length(PRT_year)
PRT_female_qx = t(matrix(as.numeric(read.table("lt_PRT_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, PRT_n_year))
PRT_male_qx   = t(matrix(as.numeric(read.table("lt_PRT_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, PRT_n_year))

PRT_n_col = ncol(PRT_female_qx)
PRT_n_row = nrow(PRT_female_qx)
PRT_female_pop = PRT_male_pop = matrix(NA, PRT_n_row, PRT_n_col)
for(ij in 1:PRT_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:PRT_n_col)
    {
        PRT_female_pop[ij,ik] = PRT_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - PRT_female_pop[ij,ik]
        
        PRT_male_pop[ij,ik] = PRT_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - PRT_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(PRT_female_pop) = rownames(PRT_male_pop) = PRT_year
colnames(PRT_female_pop) = colnames(PRT_male_pop) = age

# SLO

SLO_year = 1950:2019
SLO_n_year = length(SLO_year)
SLO_female_qx = t(matrix(as.numeric(read.table("lt_SLO_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, SLO_n_year))
SLO_male_qx   = t(matrix(as.numeric(read.table("lt_SLO_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, SLO_n_year))

SLO_n_col = ncol(SLO_female_qx)
SLO_n_row = nrow(SLO_female_qx)
SLO_female_pop = SLO_male_pop = matrix(NA, SLO_n_row, SLO_n_col)
for(ij in 1:SLO_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:SLO_n_col)
    {
        SLO_female_pop[ij,ik] = SLO_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - SLO_female_pop[ij,ik]
        
        SLO_male_pop[ij,ik] = SLO_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - SLO_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(SLO_female_pop) = rownames(SLO_male_pop) = SLO_year
colnames(SLO_female_pop) = colnames(SLO_male_pop) = age

# SPA

SPA_year = 1908:2023
SPA_n_year = length(SPA_year)
SPA_female_qx = t(matrix(as.numeric(read.table("lt_SPA_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, SPA_n_year))
SPA_male_qx   = t(matrix(as.numeric(read.table("lt_SPA_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, SPA_n_year))

SPA_n_col = ncol(SPA_female_qx)
SPA_n_row = nrow(SPA_female_qx)
SPA_female_pop = SPA_male_pop = matrix(NA, SPA_n_row, SPA_n_col)
for(ij in 1:SPA_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:SPA_n_col)
    {
        SPA_female_pop[ij,ik] = SPA_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - SPA_female_pop[ij,ik]
        
        SPA_male_pop[ij,ik] = SPA_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - SPA_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(SPA_female_pop) = rownames(SPA_male_pop) = SPA_year
colnames(SPA_female_pop) = colnames(SPA_male_pop) = age

# SWE

SWE_year = 1751:2023
SWE_n_year = length(SWE_year)
SWE_female_qx = t(matrix(as.numeric(read.table("lt_SWE_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, SWE_n_year))
SWE_male_qx   = t(matrix(as.numeric(read.table("lt_SWE_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, SWE_n_year))

SWE_n_col = ncol(SWE_female_qx)
SWE_n_row = nrow(SWE_female_qx)
SWE_female_pop = SWE_male_pop = matrix(NA, SWE_n_row, SWE_n_col)
for(ij in 1:SWE_n_row)
{
  start_pop_female = start_pop_male = 10^5
  # start_pop_female = start_pop_male = 1
  for(ik in 1:SWE_n_col)
  {
    SWE_female_pop[ij,ik] = SWE_female_qx[ij,ik] * start_pop_female
    start_pop_female  = start_pop_female - SWE_female_pop[ij,ik]
    
    SWE_male_pop[ij,ik] = SWE_male_qx[ij,ik] * start_pop_male
    start_pop_male  = start_pop_male - SWE_male_pop[ij,ik]
    rm(ik)
  }
  print(ij); rm(ij)
}
rownames(SWE_female_pop) = rownames(SWE_male_pop) = SWE_year
colnames(SWE_female_pop) = colnames(SWE_male_pop) = age

# SWI

SWI_year = 1876:2023
SWI_n_year = length(SWI_year)
SWI_female_qx = t(matrix(as.numeric(read.table("lt_SWI_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, SWI_n_year))
SWI_male_qx   = t(matrix(as.numeric(read.table("lt_SWI_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, SWI_n_year))

SWI_n_col = ncol(SWI_female_qx)
SWI_n_row = nrow(SWI_female_qx)
SWI_female_pop = SWI_male_pop = matrix(NA, SWI_n_row, SWI_n_col)
for(ij in 1:SWI_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:SWI_n_col)
    {
        SWI_female_pop[ij,ik] = SWI_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - SWI_female_pop[ij,ik]
        
        SWI_male_pop[ij,ik] = SWI_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - SWI_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(SWI_female_pop) = rownames(SWI_male_pop) = SWI_year
colnames(SWI_female_pop) = colnames(SWI_male_pop) = age

# UK

UK_year = 1922:2022
UK_n_year = length(UK_year)
UK_female_qx = t(matrix(as.numeric(read.table("lt_UK_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, UK_n_year))
UK_male_qx   = t(matrix(as.numeric(read.table("lt_UK_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, UK_n_year))

UK_n_col = ncol(UK_female_qx)
UK_n_row = nrow(UK_female_qx)
UK_female_pop = UK_male_pop = matrix(NA, UK_n_row, UK_n_col)
for(ij in 1:UK_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:UK_n_col)
    {
        UK_female_pop[ij,ik] = UK_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - UK_female_pop[ij,ik]
        
        UK_male_pop[ij,ik] = UK_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - UK_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(UK_female_pop) = rownames(UK_male_pop) = UK_year
colnames(UK_female_pop) = colnames(UK_male_pop) = age

# USA

USA_year = 1933:2023
USA_n_year = length(USA_year)
USA_female_qx = t(matrix(as.numeric(read.table("lt_USA_F.txt", header = TRUE, skip = 2)[,"qx"]), 111, USA_n_year))
USA_male_qx   = t(matrix(as.numeric(read.table("lt_USA_M.txt", header = TRUE, skip = 2)[,"qx"]), 111, USA_n_year))

USA_n_col = ncol(USA_female_qx)
USA_n_row = nrow(USA_female_qx)
USA_female_pop = USA_male_pop = matrix(NA, USA_n_row, USA_n_col)
for(ij in 1:USA_n_row)
{
    start_pop_female = start_pop_male = 10^5
    # start_pop_female = start_pop_male = 1
    for(ik in 1:USA_n_col)
    {
        USA_female_pop[ij,ik] = USA_female_qx[ij,ik] * start_pop_female
        start_pop_female  = start_pop_female - USA_female_pop[ij,ik]
        
        USA_male_pop[ij,ik] = USA_male_qx[ij,ik] * start_pop_male
        start_pop_male  = start_pop_male - USA_male_pop[ij,ik]
        rm(ik)
    }
    print(ij); rm(ij)
}
rownames(USA_female_pop) = rownames(USA_male_pop) = USA_year
colnames(USA_female_pop) = colnames(USA_male_pop) = age


###############
# plot figures
###############

par(mfrow = c(1, 2))
plot(fts(age, t(female_pop)), xlab = "Age", ylab = "Life-table death counts",
     main = "Australian female data\n (1921-2021)", ylim = c(0, 0.08))
plot(fts(age, t(male_pop)),   xlab = "Age", ylab = "", main = "Australian male data\n (1921-2020)",
     ylim = c(0, 0.08))

# save figures

savefig("Fig_1a", width = 12, height = 10, type = "png", toplines = 1.5, res = 600)
plot(fts(age, t(female_pop)), xlab = "Age", ylab = "Life-table death counts",
     main = "Australia: female data\n (1921-2021)", ylim = c(0, 0.08 * 10^5))
dev.off()

savefig("Fig_1b", width = 12, height = 10, type = "png", toplines = 1.5, res = 600)
plot(fts(age, t(male_pop)),   xlab = "Age", ylab = "", main = "Australia: male data\n (1921-2020)",
     ylim = c(0, 0.08* 10^5))
dev.off()
