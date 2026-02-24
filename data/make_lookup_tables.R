

top_bc_opts <- data.frame(top_bc_name = c("constant_pressure_head", "constant_flux", "atm_bc_with_surface_layer", "atm_bc_with_surface_runoff",
                                          "variable_pressure_head", "variable_pressure_head/flux"),
                          TopInf = c("f","f","t","t","t","t"),
                          WLayer = c("f","f","t","f","f","f"),
                          KodTop = c("1","-1","-1","-1","1","0"),
                          InitCond = c("f","f","f","f","f","f"))
# saveRDS(top_bc_opts, "data/top_bc_opts.rds")

## BotInf qGWLF FreeD SeepF KodBot DrainF hSeep

bottom_bc_opts <- data.frame(bottom_bc_name = c("contant_pressure_head", "contant_flux", "variable_pressure_head",
                                                "variable_flux", "free_drainage", "deep_drainage",
                                                "seepage_face", "horizontal_drains"),
                             BotInf = c("f", "f", "t", "t", "f", "f", "f", "f"),
                             qGWLF = c("f","f","f","f","f","t","f","f"),
                             FreeD = c("f","f","f","f","t","f","f","f"),
                             SeepF = c("f","f","f","f","f","f","t","f"),
                             KodBot = c(" ","-"," ","-","-","-","-","-"),
                             DrainF = c("f","f","f","f","f","f","f","t"),
                             hSeep = c("0","0","0","0","0","0","0","0"))
# saveRDS(bottom_bc_opts, "data/bottom_bc_opts.rds")


# "constant_pressure_head" = c(f,f,f,f,1,f,0)
# "constant_flux" = c(f,f,f,f,-1,f,0); added parameters: rTop rBot rRoot
# "variable_pressure_head" = c(t,f,f,f,1,f,0)
# "variable_flux"  = c(t,f,f,f,-1,f,0)
# "free_drainage" (default) = c(f,f,t,f,-1,f,0)
# "deep_drainage" = c(f,t,f,f,-1,f,0); added parameters: GWL0L Aqh Bqh
# "seepage_face" = c(f,f,f,t,-1,f,0); last param (hSeep) is the user specified seepage face height in lunit [cm]
# "horizontal_drains" = c(f,f,f,f,-1,t,0); 3 added parameter lines: Drain System; Drain Depth, Drain Spacing, Entrance
# Resistance; Drain Parameters (HAVE THIS BE NOT AVIALABLE RN BECAUSE TOO MANY
# PARAMETERS)
