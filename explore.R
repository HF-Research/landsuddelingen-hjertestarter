library(ProjectTemplate)
load.project()

tmp <- tempfile(tmpdir = "data")
write_json(iris, tmp)
read_json(tmp, simplifyVector = TRUE)
