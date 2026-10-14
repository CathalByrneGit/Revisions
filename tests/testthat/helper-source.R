root <- normalizePath(file.path("..", ".."))
for (f in list.files(file.path(root, "R"), full.names = TRUE)) source(f)
fixture <- function(release) file.path(root, "tests", "fixtures", release, "MUM01.json")
