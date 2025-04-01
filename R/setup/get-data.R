
## Check path
if (!exists("usr")) stop("Object 'usr' missing")
  
if (is.null(usr$data_file)) stop("Path to data file missing")
 
if (!usr$data_file %in% list.files(".", recursive = T, full.names = T)) stop("Path to data in 'usr$data_file' not in directory")

## get data
data_init$plot <- read_xlsx(usr$data_file, sheet = "01-info", na = "NA")
data_init$tree <- read_xlsx(usr$data_file, sheet = "03-tree", na = "NA") 
data_init$stem <- read_xlsx(usr$data_file, sheet = "04-stem", na = "NA", guess_max = 10000)