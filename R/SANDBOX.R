## SANDBOX ##

source("C:/Users/skati/Documents/runhydrus/R/create_hydrus_project.R")

hydrus_project <- create_hydrus_project(project_name = "my_test_model",
                                       parent_dir = paste0(getwd(), "/tests")
                                       )
parameterize_hydrus_model(hydrus_project)
