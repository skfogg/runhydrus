
library(runhydrus)
model_test <- read_hydrus_model("C:/Users/skati/Documents/runhydrus/examples/fairfield_dual_perm")

model_test$main_processes
model_test$hydrus_project$hydrus_version
model_test$solute_transport <- solute_transport(number_solutes = 1,
                                                )


run_hydrus_1D(model_test)
