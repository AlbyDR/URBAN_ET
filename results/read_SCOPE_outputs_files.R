#########################################################################
#### Read the results from the SCOPE output files
#########################################################################

########################################################################
# SCOPE results LEtotal/LEsoil/LEcanopy
########################################################################
# list the name of all with the word ROTH
Files.ROTH <- list.files(path=grep("ROTH",
                                   list.dirs(path = "D:/SCOPE-2.0N/SCOPE-2.0/output", 
                                             full.names = TRUE,
                                             recursive = F), 
                                             value = TRUE),
                         pattern= "fluxes.csv", full.names = T)

SCOPE_ROTH <- NULL
# read all files of ROTH
for (i in 1:length(Files.ROTH)) {
 SCOPE_ROTH[[i]] <- read_csv(Files.ROTH[i],
                             col_names =T,
                             cols(.default = col_double()))[-1,]                                               
}

SCOPE_ROTH[[1]]
summary(Input_ROTH)
summary(SCOPE_ROTH[[11]])
hist(SCOPE_ROTH[[1]]$nu_iterations)

SCOPE_ROTH[[1]][c(6,11,16)]

ggplot(SCOPE_ROTH[[1]], aes(x=1:10225))+
  geom_point(aes(y=lEtot))+
  geom_point(aes(y=lECtot), color="blue")+
  geom_point(aes(y=lEstot), color="red")

ggplot(SCOPE_ROTH[[11]], aes(x=1:10225))+
  geom_point(aes(y=lEtot))+
  geom_point(aes(y=lEctot), color="blue")+
  geom_point(aes(y=lEstot), color="red")

ggplot(SCOPE_ROTH[[60]], aes(x=1:10225))+
  geom_point(aes(y=lEtot))+
  geom_point(aes(y=lEctot), color="blue")+
  geom_point(aes(y=lEstot), color="red")


####################
# SCOPE model inputs
####################
Input.ROTH <- list.files(paste0(path=grep("ROTH",
                                   list.dirs(path="D:/SCOPE-2.0N/SCOPE-2.0/output", 
                                             full.names = TRUE,
                                             recursive = F), 
                                             value = TRUE), "/Parameters",
                                             collapse = NULL, recycle0 = FALSE),
                                             pattern= "filenames",
                                             full.names = T)

Models_ROTH <- NULL
for (i in 1:length(Input.ROTH)) {
  Models_ROTH[[i]] <- read_csv(Input.ROTH[i],
                                 col_names =T)                              
}  

print(as_tibble(Models_ROTH[[1]]), n=71)

Parameters_ROTH <- NULL
for (i in 1:length(Models_ROTH)) {
  Parameters_ROTH[[i]] <- filter(Models_ROTH[[i]],!is.na(X2))$X2
}

####################
# SCOPE model settings
####################
Settings.ROTH <- list.files(paste0(path=grep("ROTH",
                                           list.dirs(path="D:/SCOPE-2.0N/SCOPE-2.0/output", 
                                                     full.names = TRUE,
                                                     recursive = F), 
                                                     value = TRUE), "/Parameters",
                                                     collapse = NULL, recycle0 = FALSE),
                                                     pattern= "setoptions",
                                                     full.names = T)
Settings_ROTH <- NULL
for (i in 1:length(Settings.ROTH)) {
  Settings_ROTH[[i]] <- read_csv(Settings.ROTH[i],
                                 col_names =F)
}  

print(as_tibble(Settings_ROTH[[1]]), n=20)
filter(Settings_ROTH[[1]],X1>0)$X2

####################
# SCOPE model settings
####################
Constants.ROTH <- list.files(paste0(path=grep("ROTH",
                                              list.dirs(path="D:/SCOPE-2.0N/SCOPE-2.0/output", 
                                                        full.names = TRUE,
                                                        recursive = F), 
                                                        value = TRUE), "/Parameters",
                                                        collapse = NULL, recycle0 = FALSE),
                                                        pattern= "input_data",
                                                        full.names = T)

Constants_ROTH <- NULL
for (i in 1:length(Constants.ROTH)) {
  Constants_ROTH[[i]] <- read_csv(Constants.ROTH[i],
                                 col_names =T)
}  

print(as_tibble(Constants_ROTH[[1]]), n=84)
filter(Constants_ROTH[[1]],!is.na(PROSPECT))

#########################################################################
#### TUCC ###############################################################
#########################################################################
# SCOPE results LEtotal/LEsoil/LEcanopy
####################
File.TUCC <- list.files(path=grep("TUCC",
                                   list.dirs(path = "D:/SCOPE-2.0N/SCOPE-2.0/output", 
                                             full.names = TRUE,
                                             recursive = F), 
                                   value = TRUE),
                         pattern= "fluxes.csv",
                         full.names = T)

SCOPE_TUCC <- NULL
for (i in 1:length(File.TUCC)) {
  SCOPE_TUCC[[i]] <- read_csv(File.TUCC[i],
                              col_names =T,
                              cols(.default = col_double()))[-1,]    
}

####################
# SCOPE model inputs
####################
Input.TUCC <- list.files(paste0(path=grep("TUCC",
                                           list.dirs(path="D:/SCOPE-2.0N/SCOPE-2.0/output", 
                                                     full.names = TRUE,
                                                     recursive = F), 
                                           value = TRUE), "/Parameters",
                                 collapse = NULL, recycle0 = FALSE),
                          pattern= "filenames",
                          full.names = T)

Models_TUCC <- NULL
for (i in 1:length(Input.TUCC)) {
  Models_TUCC[[i]] <- read_csv(Input.TUCC[i],
                                 col_names =T)
}  

print(as_tibble(Models_TUCC[[1]]), n=71)

Parameters_TUCC <- NULL
for (i in 1:length(Models_TUCC)) {
  Parameters_TUCC[[i]] <- filter(Models_TUCC[[i]],!is.na(X2))$X2
}

####################
# SCOPE model settings
####################
Settings.TUCC <- list.files(paste0(path=grep("TUCC",
                                              list.dirs(path="D:/SCOPE-2.0N/SCOPE-2.0/output", 
                                                        full.names = TRUE,
                                                        recursive = F), 
                                              value = TRUE), "/Parameters",
                                    collapse = NULL, recycle0 = FALSE),
                             pattern= "setoptions",
                             full.names = T)

Settings_TUCC <- NULL
for (i in 1:length(Settings.TUCC)) {
  Settings_TUCC[[i]] <- read_csv(Settings.TUCC[i],
                                 col_names =F)
}  

print(as_tibble(Settings_TUCC[[1]]), n=20)
filter(Settings_TUCC[[1]],X1>0)$X2

####################
# SCOPE model settings
####################
Constant.TUCC <- list.files(paste0(path=grep("TUCC",
                                              list.dirs(path="D:/SCOPE-2.0N/SCOPE-2.0/output", 
                                                        full.names = TRUE,
                                                        recursive = F), 
                                              value = TRUE), "/Parameters",
                                    collapse = NULL, recycle0 = FALSE),
                             pattern= "input_data",
                             full.names = T)

Constants_TUCC <- NULL
for (i in 1:length(Constant.TUCC)) {
  Constants_TUCC[[i]] <- read_csv(Constant.TUCC[i],
                                 col_names =T)
}  

print(as_tibble(Constants_TUCC[[1]]), n=84)
filter(Constants_TUCC[[1]],!is.na(PROSPECT))

#############################################################################
#############################################################################
