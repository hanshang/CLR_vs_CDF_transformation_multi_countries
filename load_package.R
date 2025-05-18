################
# Package names
################

#devtools::install_github("mpascariu/MortalityForecast")

packages <- c("Compositional", "psych", "ftsa", "meboot", "pracma", "reldist", "flexmix", "demography",
              "MortalityLaws", "HMDHFDplus", "LaplacesDemon", "dplyr", "compositions", "easyCODA")

# Install packages not yet installed

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading

invisible(lapply(packages, library, character.only = TRUE))

# selecting the number of components via EVR

select_K <- function(tau, eigenvalue)
{
    k_max = length(eigenvalue)
    k_all = rep(0, k_max-1)
    for(k in 1:(k_max-1))
    {
        k_all[k] = (eigenvalue[k+1]/eigenvalue[k])*ifelse(eigenvalue[k]/eigenvalue[1] > tau, 1, 0) + ifelse(eigenvalue[k]/eigenvalue[1] < tau, 1, 0)
    }
    K_hat = which.min(k_all)
    return(K_hat)
}
