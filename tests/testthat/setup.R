Sys.setenv("USER_APP_FILE_HOME_DIR" = "../../data/auxiliary/user-files")
Sys.setenv("APP_ARTIFACTS_DIR" = paste0(tempdir(), "/")) # citation_metadata.csv written by rFunction(); appArtifactPath() adds no separator
remotes::install_github("movestore/moveapps-sdk-r-package")
moveapps::logger.init()

moveapps::clearRecentOutput()
# the system under test (sut)
source(file.path("..", "..", "./RFunction.R"))