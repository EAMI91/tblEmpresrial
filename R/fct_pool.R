pool <- pool::dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = "db_gp",
  host = "mysql.cduzrgqgkfht.us-east-2.rds.amazonaws.com",
  username = "root",
  password = "9Blw33caY",
  port = 3306
)

onStop(function() {
  pool::poolClose(pool)
})

entidadesbd <- "p_suscriptoresIII"
pruebabd <- "tw_pruebaIII"
entrenamientobd <- "tw_entrenamientoIII"
noticiasbd <- "p_entrenamientoIII"
opcionesbd <- "p_opcionesIII"
candidatosbd <- "p_temasIII"
