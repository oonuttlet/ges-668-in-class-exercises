compare_nums <- function(x, y) {
  abs(y-x)
}

compare_nums(15,20)

compare_nums(c(2,10), c(8, 20))

compare_nums(c(2,10), 1)

library(spData)

us_states

filter_states <- function(data, 
                          region = NULL,
                          name = NULL,
                          ...) {
  stopifnot(
    "Both `region` and `name` cannot be provided." = is.null(name) || is.null(region),
    "`data` must be a dataframe!" = is.data.frame(data)
  )
  if (!is.null(region) & is.null(name)) {
    return(data |>
      dplyr::filter(REGION == region))
  }
  else if (is.null(region) && is.null(name)){
    return(data)
  }
  data |>
    dplyr::filter(NAME == name)
}

filter_states(us_states)

data |>
  dplyr::filter

# Good
is_it_wednesday <- function(x) {
  wday <- wday(x, label = TRUE)
  
  if (identical(as.character(wday), "Wed")) {
    return("Yes!")
  }
  
  "No..."
}

is_it_wednesday(Sys.Date())

# Bad
is_it_wednesday <- function(x) {
  wday <- wday(x, label = TRUE)
  
  if (identical(wday, "Wed")) {
    return("Yes!")
  } else {
    return("No...")
  }
}


### IS EVEN 2024-10-26 ###

# Function that determines whether a number is even or odd
# Take one or more numbers
# Return a value indicating whether the value is even or odd (must be the same length as the input vector)
# Name, arguments, body

is_even <- function(num){
  if (!is.numeric(num)){
    cli::cli_abort("`num` must be numeric!")
  }
  ifelse(num%%2 == 0, "Even!", "Odd!")
}

is_even_josh <- function(num) {
  dplyr::if_else(num %% 2 == 0, TRUE, FALSE)
}

