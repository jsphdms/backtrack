# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

kt_valid_move <- function(i, j, solution) {

  valid <- all(0 < i,
               0 < j,
               i < nrow(solution) + 1,
               j < nrow(solution) + 1,
               is.na(solution[i, j]))

  return(valid)

}
