// Set nth bit to x in number
(number &&& ~~~(1UL <<< n)) ||| (x <<< n)
