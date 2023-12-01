# 0 Load data and dependencies --------------------------------------------

library(dplyr)
library(stringr)
library(magrittr)
library(tidyr)

day1 <- read.table("inputs/day1.txt")
colnames(day1) <- c("code")

# 1 Part 1, just the numbers --------------------------

# solution 1
day1 %>% 
  mutate(numbers = as.numeric(gsub('[^0-9]+', "", day1$code))) %>% # filter to numbers
  rowwise() %>% # rowwise operation
  mutate(numbers = glue::glue(numbers, numbers)) %>% # duplicate everything because first/last order is maintained
  ungroup() %>% # remove rowwise
  mutate(first_digit = str_extract(numbers, "^\\d"), # get first digit
         last_digit = str_extract(numbers, "\\d$")) %>% # second digit
  rowwise() %>% # need rowwise again
  mutate(total = as.numeric(glue(first_digit, last_digit))) %>% # combine and make numeric
  ungroup() %>% 
  summarize(
    sum = sum(total)
  )

# solution 2
day1 %>% 
  extract(code, "first", "(\\d)", remove = F) %>% 
  extract(code, "last", ".*(\\d)", remove = F) %>% 
  mutate(total = as.numeric(paste0(first, last))) %>% 
  summarize(
    sum = sum(total)
  )

# Part 2, convert words to numbers and repeat -----------------------------

day1 %>% 
  mutate(code = gsub("one", "1", code),
         code = gsub("two", "2", code),
         code = gsub("three", "3", code),
         code = gsub("four", "4", code),
         code = gsub("five", "5", code),
         code = gsub("six", "6", code),
         code = gsub("seven", "7", code),
         code = gsub("eight", "8", code),
         code = gsub("nine", "9", code)) %>% # substitute all numbers
  extract(code, "first", "(\\d)", remove = F) %>% # then repeat extraction
  extract(code, "last", ".*(\\d)", remove = F) %>% 
  mutate(total = as.numeric(paste0(first, last))) %>% 
  summarize(
    sum = sum(total)
  )

# alternative option to substitute numbers
for (i in 1:9) {
  day1 <- day1 %>% 
    mutate(code = gsub(numbers[i], i, code))
}

# the problem with those solutions is that it goes numerically, so replaces all the ones first
# even if eight appears in the word first, it might be superseded by two, e.g., eightwo would translate to 2 because 2 is coded first
# so I need a solution that goes by the word

number <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

day1 %>% 
  extract(code, "first", "(\\d|one|two|three|four|five|six|seven|eight|nine)", remove = F) %>% 
  extract(code, "last", ".*(\\d|one|two|three|four|five|six|seven|eight|nine)", remove = F) %>% 
  mutate(first = coalesce(as.numeric(first), match(first, number)), # match(first, number) takes advantage of the order of the number vector to replace the word with the number place in the vector (i.e., num has to be in order), and then coalesce introduces NAs and the number from match replaces them
         last = coalesce(as.numeric(last), match(last, number))) %>% 
  mutate(n = as.numeric(paste0(first, last))) %>% 
  summarize(
    sum = sum(n)
  )


# 9999 Experimentation Zone -----------------------------------------------

# how does extract function work?

df <- tibble(x = c(NA, "a-b", "a-d", "b-c", "d-e"))
df %>% extract(x, "A")
df %>% extract(x, c("A", "B"), "([[:alnum:]]+)-([[:alnum:]]+)")

# so you list the column you're extracting from, x
# and what columns you want to sort into, c("A", "B") sorts into two columns
# and then what you're sorting, which somehow gets divided evenly between the two columns

# how does coalesce work?

x <- sample(c(1:5, NA, NA, NA))
coalesce(x, 0L)

# replaces NA variables with provided option, 0L

y <- c(1, 2, NA, NA, 5)
z <- c(NA, NA, 3, 4, 5)
coalesce(y, z)

# replaces NA with non-NA option from other df
# what if other df also has NA there?

z2 <- c(NA, NA, NA, 4, 5)
coalesce(y, z2) # then keeps the NA
