parse_args <- function(args){
  # Assigning variables
  args = list(
    path = args[1],
    ctrl = args[2],
    disease = args[3],
    chr_n = args[4]
  )
  
  return(args)
}