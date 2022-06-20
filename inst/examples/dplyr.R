#' Title: Simple asciicast example
#' Depends: processx (>= 3.4.0), dplyr

# Comments are simply "typed in"
# Commands are typed in as well. They are executed and their output
# is printed:

library(dplyr)

iris %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise_all(mean)

# Here is another command:

starwars %>%
  group_by(species) %>%
  summarise(
    n = n(),
    mass = mean(mass, na.rm = TRUE)
  ) %>%
  filter(n > 1,
         mass > 50)


