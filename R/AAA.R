# Package environment will be used for many convienience things. During development
# there will be a need to add new positions there
pkg_env <- new.env()

#### deeps classes prefic ####
pkg_env$cls_prfx <- "dgs4"
pkg_env$object_types <- c("Obj", "Req", "Resp", "Data")
