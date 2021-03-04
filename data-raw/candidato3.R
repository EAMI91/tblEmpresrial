## code to prepare `candidato3` dataset goes here
token <- create_token(
  app = "R_Mining_chemi",
  consumer_key = "egBPz5BVerF974Nsk4rYuozne",
  consumer_secret = "aXO7swCGnH4biykQNAt5gochFsbMFnm6bOwilPIMRqLKJ2nt3Z", 
  access_token = "101707868-zV8saQyrQU5TyQ8M4z5ZPNEo6KvS7svxMekJRJ6n",
  access_secret = "CMlK4cX499SpaPyg3gwOxxVNF6TRsAM5YeAY3AOaN7yWp",
  set_renv = F
)

candidato3 <- get_timeline("TonoIxtlahuac", n = 3200, token = token)

usethis::use_data(candidato3, overwrite = TRUE)
