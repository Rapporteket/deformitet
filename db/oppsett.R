devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)

# dekoding av database-dump
sship::dec("c://Users/ibo600/Downloads/deformitet15eb2789.sql.gz__20250506_150122.tar.gz", keyfile = "c://Users/ibo600/Documents/.ssh/id_rsa", target_dir = "c://Users/ibo600/Downloads/.")

Sys.setlocale(locale = 'nb_NO.UTF-8')
source("db/sysSetenv.R")

Sys.setenv(MYSQL_HOST = "localhost") # for mobilt kontor
Sys.setenv(MYSQL_DB_DATA = "deformitet")

Sys.setenv(R_RAP_CONFIG_PATH = "c://Users/ibo600/repo/config")
