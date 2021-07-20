unzip_LPJ_GUESS_output <- function (sourceDir,
                                    destDir) {
    
    
    file_list <- list.files(path=sourceDir, pattern = ".tar.gz")
    
    out_list <- gsub(".tar.gz", "", file_list)
    
    ### unzip
    for (i in 1:length(file_list)) {
        
        ## mk directory
        if(!dir.exists(paste0(sourceDir, "/", out_list[i]))) {
            dir.create(paste0(sourceDir, "/", out_list[i]), showWarnings = FALSE)
        }
        
        ### command texts
        text1 <- paste0(sourceDir, "/", file_list[i])
        text2 <- paste0("tar xvzf ", text1, " -C ", sourceDir, "/", out_list[i])
        
        ### system
        system(command=text2)
        
    }
    
    for (i in 1:length(out_list)) {
        
        currentDir <- paste0(sourceDir, "/", out_list[i])
        
        myFiles1 <- list.files(path = currentDir, pattern = "_euc_ter.csv")
        myFiles2 <- list.files(path = currentDir, "_D.csv")
        
        for (j in 1:length(myFiles1)) {
            test1 <- substr(myFiles1[j], start=5, stop=9)
            
            if (test1 == "LPJGN") {
                t1 <- paste0(currentDir, "/", myFiles1[j])
                t2 <- paste0(currentDir, "/", myFiles2[j])
                
                c1 <- paste0("mv ", t1, " ", destDir, "/LPJGN/new_soil/euc_ter/")
                c2 <- paste0("mv ", t2, " ", destDir, "/LPJGN/new_soil/all_pft/")
                
                system(command=c1)
                system(command=c2)
                
            } else if (test1 == "LPJGP") {
                t1 <- paste0(currentDir, "/", myFiles1[j])
                t2 <- paste0(currentDir, "/", myFiles2[j])
                
                c1 <- paste0("mv ", t1, " ", destDir, "/LPJGP/new_soil/euc_ter/")
                c2 <- paste0("mv ", t2, " ", destDir, "/LPJGP/new_soil/all_pft/")
                
                system(command=c1)
                system(command=c2)
            } else {
                print("model abbreviation incorrect...")
            }
        } # j
        
            
    } # i
    
}