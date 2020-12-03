input = readLines('input')

split = lapply(input, function(x) strsplit(gsub(":","",x), " ")[[1]])

is_valid = function(s){
  bounds = strsplit(s, "-")[[1]]
  lower = as.numeric(bounds[1])
  upper = as.numeric(bounds[2])
  
  pattern = sprintf("[^%s]", s[2])
  occ = nchar(gsub(pattern, "", s[3]))
  occ >= lower & occ <= upper
}

sum(sapply(split, function(x) is_valid(x)))


is_valid2 = function(s){
  bounds = strsplit(s, "-")[[1]]
  lower = as.numeric(bounds[1])
  upper = as.numeric(bounds[2])

  chars = strsplit(s[3],"")[[1]]
  
  (chars[lower] == s[2]) + (chars[upper] == s[2]) == 1
}
sum(sapply(split, function(x) is_valid2(x)))
