#‘{
#  “db_type”: “mysql”,
#  “uri_cli_1": “mysql -u admin -pDISQUEXCJHWVPNQE --host sl-eu-fra-2-portal.3.dblayer.com --port 23323 --ssl-mode=REQUIRED”,
#  “maps”: [],
#  “instance_administration_api”: {
#    “instance_id”: “be06e056-1d80-4eac-a04d-2e724c6b453c”,
#    “root”: “https://composebroker-dashboard-public.eu-de.mybluemix.net/api“,
#    “deployment_id”: “60e70d1db078f6000c77ea16"
#},
#“name”: “bmix-eude-yp-be06e056-1d80-4eac-a04d-2e724c6b453c”,
#“uri_cli”: “mysql -u admin -pDISQUEXCJHWVPNQE --host sl-eu-fra-2-portal.4.dblayer.com --port 23323 --ssl-mode=REQUIRED”,
#“uri_direct_1”: “mysql://admin:DISQUEXCJHWVPNQE@sl-eu-fra-2-portal.3.dblayer.com:23323/compose”,
#“ca_certificate_base64”: “LS0tLS1CRUdJTiBDRVJUSUZJQ0FURS0tLS0tCk1JSURwVENDQW8yZ0F3SUJBZ0lFWU9jTkhqQU5CZ2txaGtpRzl3MEJBUTBGQURCVU1WSXdVQVlEVlFRRERFa3gKTkdJMlptVXdPREEyWm1aaU16SmtZVGhrTWpjM09XUmtZV0UxTmpRNE15dGxkV05zYjNWa0xXSmhNekpoWmpObQpObU0wTjJRNE9UUTJOVFZrTVRJMU5tSXdPVFV6WW1ZNU1CNFhEVEl4TURjd09ERTBNelV4TUZvWERUUXhNRGN3Ck9ERTBNREF3TUZvd1ZERlNNRkFHQTFVRUF3eEpNVFJpTm1abE1EZ3dObVptWWpNeVpHRTRaREkzTnpsa1pHRmgKTlRZME9ETXJaWFZqYkc5MVpDMWlZVE15WVdZelpqWmpORGRrT0RrME5qVTFaREV5TlRaaU1EazFNMkptT1RDQwpBU0l3RFFZSktvWklodmNOQVFFQkJRQURnZ0VQQURDQ0FRb0NnZ0VCQU5tVXN1eGZtMGVLc0ZycGlMT21vbm40CmdVczVaeVcyaVhjUldXTXc4MzRwVUgrRFpSR244bmdkZmttVkthQ2MrTUFIdlkxZ1QwNk9sTnh4aXlobW4vRUQKMWZ5RzJ1cTFROFhjODcxRWY3ZWFCb1NQTnZZRk1zbU04aWFTYXQzT24xT1U4YWF5eC9yRndnZmhzcWxSRjRnVwpQNzA5RkV4MWZ3dExkbUIvSi9TUk9HR1JMR1MzYTRsdERaUjkyczJvSDFOVFZkRkY3MWswMlNIdEppd3JrWWJJCjhiZmhzRW9GOUUyWHhRZ0xxNEZ3bzYvTGZMakNCSXNrOG5Ja2ZOUG0vN3lnbzM3UHlzSGlQNTlTNmV5c1o4TVoKdi9YN2NkaklVSkNUS0dodm55VVVJQk9LNnRBQS9vblhQaWtTbUZEcHZYNS81NllmVE1MR2N0eUQwUWZIeEJFQwpBd0VBQWFOL01IMHdIUVlEVlIwT0JCWUVGRG4zcnRZcmxRR0taQ0NweStDeGZ3MmpGWXB5TUE0R0ExVWREd0VCCi93UUVBd0lDQkRBZEJnTlZIU1VFRmpBVUJnZ3JCZ0VGQlFjREFRWUlLd1lCQlFVSEF3SXdEQVlEVlIwVEJBVXcKQXdFQi96QWZCZ05WSFNNRUdEQVdnQlE1OTY3V0s1VUJpbVFncWN2Z3NYOE5veFdLY2pBTkJna3Foa2lHOXcwQgpBUTBGQUFPQ0FRRUFKSkFXaFhzMitucXN1Q1VKK0FZWGlYcEZoa0NZa0RVZXltdGNLYmhqZFdCQ1VtcXM1RHN6Ck9HSUlRYlVYZmFZYzI5RGdnQVZYNTZCWitOMjBMZGh6QWJnbllDZEZacTNxMWJIUzFQRHJpb2ZwMERvZWJ2Z3cKcWNzeWM5TWdOZ2tnL0M1Q3FUei8zcGZXMmFrRWZqWVl1WnpMOTZXV0xTYlhJQlp1WTR2VjNuSitDNi9ucldlawpBQ0ZNQkhUdnRIL25oSWYzd0RVUWtZSHc1RmJId2o0Mm5MU1J1ZW9yTzFsZEhOVFN5M1BqcWRRbWZkN3EyTWkxClQrcTZ5Z3FodnNrQytTWmVoQ0lXbGNDYzJKZDJsM1JSMzZicmdHS3BMQkZINk9hWDNhQjl1SDFKZnY3QnlXTHEKVWtWY2hVTXl4dGlJckltREQ0SlowV3NzcEhWZlhxV0krZz09Ci0tLS0tRU5EIENFUlRJRklDQVRFLS0tLS0K”,
#“deployment_id”: “60e70d1db078f6000c77ea16",
#  “uri”: “mysql://admin:DISQUEXCJHWVPNQE@sl-eu-fra-2-portal.4.dblayer.com:23323/compose”
#}’

library(odbc)
library(RMariaDB)
library(DBI)
library(dbplyr)


# mariadb credentials and connectionstring
# ----------------------------------------------------------------------------------------------------------
repDBConnect <- function(Server=mariadb_hostname, UID=mariadb_username, PWD=mariadb_password) {

  mariadb_database="RepMariaDB"
  mariadb_username="RepMariaDBUser"
  mariadb_password="RepMariaDBUserPassword"
  mariadb_hostname="127.0.0.1"
  #mariadb_hostname="localhost"  # MUST NOT USE -> connect through socket, not ip
  mariadb_port=3306
  mariadb_driver="odbcinstMariaDB" # from /etc/odbcinst.ini

  if (Sys.getenv("REP_CRISPML_ENV") != "")
    mariadb_hostname <- "rep-mariadb-service"

  logMsg(paste("Connecting to", mariadb_database, "at", mariadb_hostname, "as",mariadb_username))

  # Method with packages odbc and DBI. TODO: this is for SQL server -> adapt for DB2
  conDB <- DBI::dbConnect( drv = RMariaDB::MariaDB(),
                           host      = mariadb_hostname,
                           port      = mariadb_port,
                           dbname    = mariadb_database,
                           username  = mariadb_username,
                           password  = mariadb_password)
  # via /etc/odbc.ini
  # conDB <- DBI::dbConnect(RMariaDB::MariaDB(), groups="odbcMariaDB")
  # via ~/.my.cnf
  # conDB <- DBI::dbConnect(RMariaDB::MariaDB(), host="127.0.0.1", port=3306, groups="rep-mariadb")

  return(conDB)
}


repDBListTables <- function () {
  #logMsg("repDBListTables entry")
  #dfEntry <- perfLogEntry(method="repDBListTables", dataSet="method", mlModel="RepMariaDB")
  
  # List tables in REP_DBSCHEMA
  dbCon <- repDBConnect()
  tbls <- DBI::dbListTables(dbCon)
  dbDisconnect(dbCon)
  
  #perfLogExit(dfEntry=dfEntry, db="CrispmlDB", measurement="Crispml")
  #logMsg("repDBListTables exit")
  return (tbls)
}



