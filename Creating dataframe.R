#Create a dataframe with variables having random values

N  <- 100
df  <- data.frame(id = 1:N, x1 = rnorm(N),
                  x2 = sample(1:10, size = N, replace = TRUE), 
                  x3 = sample(letters[1:10], size = N, replace = TRUE), 
                  x4 = 1*(runif(n = N) < .75))



#Randomly insert NAs into dataframe proportionately - 1
as.data.frame(lapply(df, function(cc) cc[ sample(c(TRUE, NA), 
                                                 prob = c(0.85, 0.15), size = length(cc), replace = TRUE) ]))

#Randomly insert NAs into dataframe proportionately - 2
miss_add_random =  function(df, prop = .1){
  n = nrow(df)
  m = ncol(df)
  num.to.na = ceiling(prop*n*m)
  id = sample(0:(m*n-1), num.to.na, replace = FALSE)
  rows = id %/% m + 1
  cols = id %% m + 1
  sapply(seq(num.to.na), function(x){
    df[rows[x], cols[x]] <<- NA
  }
  )
  return(df)
}

df_na <- miss_add_random(df, prop = 0.1)