input = readLines("input3")

input = lapply(input, function(x) strsplit(x, "")[[1]])
input = matrix(unlist(input), nrow = length(input), byrow = T)

traverse = function(input, right=3, down=1){
  dims = dim(input)
  counter = 0
  i=1; j=1;
  repeat{
    i = i+down
    j = ((j+right-1) %% dims[2]) + 1
    if (i > dims[1]){
      break
    }
    if (input[i,j] == "#")
      counter = counter+1
  }
  counter
}  

#
traverse(input, 3,1)

#
prod(
traverse(input, 1, 1),
traverse(input, 3, 1),
traverse(input, 5, 1),
traverse(input, 7, 1),
traverse(input, 1, 2)
)
