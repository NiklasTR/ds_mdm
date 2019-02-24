parse_args <- function(args){
  # Assigning variables
  # if(grepl(args[2], pattern = "c(")){
    args = list(
      path = args[1],
      ctrl = eval(parse(text = args[2])),
      disease = args[3],
      chr_n = args[4]
    )
    return(args)
  # }
  # 
  # args = list(
  #   path = args[1],
  #   ctrl = args[2],
  #   disease = args[3],
  #   chr_n = args[4])
  # 
  # return(args)
}