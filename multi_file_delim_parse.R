# create sample data
sample_data1 <- data.frame(a = c(1,2,3), b = c('x', 'y', 'z'), c = c('444-44-1111', 32, '111-22-3456'))
sample_data2 <- data.frame(a = c(4,5,6), b = c('x', 'y', 'z'), c = c(11, 42, '111-22-3456'), d = c(4,3,2))

# test - regex detection mechanism - run through all columns per row, return boolean
# apply(sample_data1,1,function(x) sum(grepl("[0-9]{3}-[0-9]{2}-[0-9]{4}",x))>0)

# write files with different delims
## this is the file path to folder on MY computer
write.table(sample_data1, file = '/Users/ludumipsum/GitHub/msearch/data/file1', sep = '|')
write.table(sample_data2, file = '/Users/ludumipsum/GitHub/msearch/data/file2', sep = '~')
write.table(sample_data2, file = '/Users/ludumipsum/GitHub/msearch/data/file3', sep = '\t')

# list files in a directory
file_list <- list.files('/Users/ludumipsum/GitHub/msearch/data/', full.names = TRUE)

# 1. read file
# 2. determine delim
# 3. read file based on delim
# 4. binary check with regex
# 5. write files back out

check_ssn <- function(files, output_dir, write=FALSE){

  lapply(files, function(input_file){
    #browser()
    
    # 2. determine delim
    if (grepl('\\|', readLines(input_file, n=2)[2])){
        parse_file <- read.table(input_file, sep = '|')
        parse_file$ssn_bool <-  apply(parse_file,1,function(x) sum(grepl("[0-9]{3}-[0-9]{2}-[0-9]{4}",x))>0)
        write.table(x = parse_file, file =  paste0(input_file, '_checked'))
    } else if (grepl('~', readLines(input_file, n=2)[2])){
        parse_file <- read.table(input_file, sep = '~')
        parse_file$ssn_bool <-  apply(parse_file,1,function(x) sum(grepl("[0-9]{3}-[0-9]{2}-[0-9]{4}",x))>0)
        write.table(x = parse_file, file =  paste0(input_file, '_checked'))
    } else if (grepl('\t', readLines(input_file, n=2)[2])){
        parse_file <- read.table(input_file, sep = '\t')
        parse_file$ssn_bool <-  apply(parse_file,1,function(x) sum(grepl("[0-9]{3}-[0-9]{2}-[0-9]{4}",x))>0)
        write.table(x = parse_file, file =  paste0(input_file, '_checked'))
    }
    })
}

# i didn't set up the last two arguments inside the function, output_dir and write. 
# you can mess with that if you like
processed_list <- check_ssn(file_list, output_dir = '/Users/ludumipsum/GitHub/msearch/data_checked/', write = TRUE)



