## code to prepare `candidato2` dataset goes here
token <- create_token(
  app = "R_Mining_chemi",
  consumer_key = "egBPz5BVerF974Nsk4rYuozne",
  consumer_secret = "aXO7swCGnH4biykQNAt5gochFsbMFnm6bOwilPIMRqLKJ2nt3Z", 
  access_token = "101707868-zV8saQyrQU5TyQ8M4z5ZPNEo6KvS7svxMekJRJ6n",
  access_secret = "CMlK4cX499SpaPyg3gwOxxVNF6TRsAM5YeAY3AOaN7yWp",
  set_renv = F
)

candidato2 <- get_timeline("CarlosHerreraSi", n = 3200, token = token)

usethis::use_data(candidato2, overwrite = TRUE)
