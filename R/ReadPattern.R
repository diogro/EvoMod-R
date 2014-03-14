#'Read Pattern from directory
#'
#'Read many populations from a folder using a pattern
#'@export
ReadPattern <- function(pattern,
                        n.traits = 10,
                        sel.type,
                        direct.sel = T){
  folders  <- dir("output/", pattern)
  main.data = llply(folders, function(x) ReadFolder(x, n.traits, sel.type, direct.sel))
  names(main.data) = folders
  return(main.data)
}