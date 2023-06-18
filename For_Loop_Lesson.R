this_letters <- LETTERS[1:26]

this_letters[2]

# Initialization
empty_list <- vector("list", length(this_letters))
empty_list[[2]]
# for loop
for (iter in 1:length(this_letters)) {
  empty_list[[iter]] <- this_letters[iter]
}