#' Does the next position help make a knight's tour?
#'
#' Can the proposed next position be appended to the given partial knight's tour
#'   to create a larger knight's tour (which may or may not be a complete
#'   knight's tour)?
#'
#' @param position A numeric vector of length 2 describing the coordinates of
#'   the proposed next position on a chess board.
#' @param solution A numeric 8x8 matrix describing the current partial solution
#'   on a chess board.
#' @return \code{TRUE} if the proposed next position can be appended to the
#'   given partial knight's tour to form another knight's tour (which may or may
#'   not be a complete knight's tour). Otherwise \code{FALSE}.
kt_valid_move <- function(position = position, solution) {

  i <- position["i"]
  j <- position["j"]

  on_the_board <- all(0 < i,
                      0 < j,
                      i <= nrow(solution),
                      j <= nrow(solution))

  # tidy up this logic
  if (on_the_board) {
    if (is.na(solution[i, j])) {
      return(TRUE)
    } else (
      return(FALSE)
    )
  } else {
    return(FALSE)
  }
}

#' Find a knight's tour recursively
#'
#' Recursively search for a knight's tour on a NxN chess board. Currently
#' returns no more than one solution. The knight begins on a corner square.
#'
#' @param N The size of the NxN chess board.
#' @param position A numeric vector of length 2 describing the coordinates of
#'   the proposed next position on a chess board.
#' @param moves List of numeric vectors describing the valid moves a kight can
#'   make in a game of chess.
#' @param solution Numeric NxN matrix describing the (possibly partial) knight's
#'   tour.
#' @param last_move Numeric vector of length 2 describing the last move in the
#'   (possibly partial) knight's tour.
#' @param pb Text progress bar to view progress in constructing a knight's tour.
#' @return A NxN matrix describing a knight's tour if a solution exists.
#'   Otherwise \code{FALSE}.
#'
#' @examples
#' kt_solve_recursively(N = 5)
#' @export
kt_solve_recursively <- function(N = 8, position = c(i = 1, j = 1),
                                 moves = list(c(2, 1),
                                              c(1, 2),
                                              c(-1, 2),
                                              c(-2, 1),
                                              c(-2, -1),
                                              c(-1, -2),
                                              c(1, -2),
                                              c(2, -1)),
                                 solution = matrix(c(1, rep(NA_integer_, N ^ 2 - 1)),
                                                   nrow = N),
                                 last_move = c(i = 0, j = 0),
                                 pb = txtProgressBar(style = 3)) {

  moves <- lapply(moves, `names<-`, c("i", "j"))

  if (!any(is.na(solution))) return (solution)

  for (move in moves) {

    next_position <- position + move

    if (kt_valid_move(position = next_position, solution = solution)) {

      last_move <- move
      position <- next_position
      solution[position["i"], position["j"]] <- sum(!is.na(solution)) + 1


      setTxtProgressBar(pb, max(solution, na.rm = TRUE) / N ^ 2)

      recurse <- kt_solve_recursively(N = N, position = position, moves = moves,
                                      solution = solution,
                                      last_move = last_move,
                                      pb = pb)

      if (is.matrix(recurse)) {
        close(pb)
        return(recurse)
      } else {
        solution[position["i"], position["j"]] <- NA_integer_
        position <- position - last_move
        }
    }

  }

  return(FALSE)
  close(pb)

}
