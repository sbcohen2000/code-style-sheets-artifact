isEven n = if n == 0
  then 1
  else isOdd

isOdd n = if n == 0
  then 0
  else isEven (n - 1)

main = print (isEven 28)
