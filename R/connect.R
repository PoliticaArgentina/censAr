censo_path <- function() {
  sys_censo_path <- Sys.getenv("CENSAR_DIR")
  sys_censo_path <- gsub("\\\\", "/", sys_censo_path)
  if (sys_censo_path == "") {
    return(gsub("\\\\", "/", tools::R_user_dir("censAr")))
  } else {
    return(gsub("\\\\", "/", sys_censo_path))
  }
}

censo_check_status <- function() {
  if (!censo_status(FALSE)) {
    stop("La base de datos local del Censo 2010 esta vacia o daniada.
         Descargala con censo_descargar().")
  }
}

#' Conexion a la Base de Datos del Censo
#'
#' Devuelve una conexion a la base de datos local. Esto corresponde a una
#' conexion a una base DuckDB compatible con DBI. A diferencia de
#' [censAr::censo_tabla()], esta funcion es mas flexible y se puede usar con
#' dbplyr para leer unicamente lo que se necesita o directamente con DBI para
#' usar comandos SQL.
#'
#' @param dir La ubicacion de la base de datos en el disco. Por defecto es
#' `censAr` en la carpeta de datos del usuario de R o la variable de entorno
#' `CENSAR_DIR` si el usuario la especifica.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  DBI::dbListTables(censo_conectar())
#'
#'  DBI::dbGetQuery(
#'   censo_conectar(),
#'   'SELECT * FROM comunas WHERE provincia_ref_id = 1'
#'  )
#' }
censo_conectar <- function(dir = censo_path()) {
  duckdb_version <- utils::packageVersion("duckdb")
  db_file <- paste0(dir, "/censAr_duckdb_v", gsub("\\.", "", duckdb_version), ".sql")

  db <- mget("censo_conectar", envir = censo_cache, ifnotfound = NA)[[1]]

  if (inherits(db, "DBIConnection")) {
    if (DBI::dbIsValid(db)) {
      return(db)
    }
  }

  try(dir.create(dir, showWarnings = FALSE, recursive = TRUE))

  drv <- duckdb::duckdb(db_file, read_only = FALSE)

  tryCatch({
    con <- DBI::dbConnect(drv)
  },
  error = function(e) {
    if (grepl("Failed to open database", e)) {
      stop(
        "La base de datos local del Censo esta siendo usada por otro proceso.
        Intenta cerrar otras sesiones de R o desconectar la base usando
        censo_desconectar() en las demas sesiones.",
        call. = FALSE
      )
    } else {
      stop(e)
    }
  },
  finally = NULL
  )

  assign("censo_conectar", con, envir = censo_cache)
  con
}

#' Tablas Completas de la Base de Datos del Censo
#'
#' Devuelve una tabla completa de la base de datos. Para entregar datos
#' filtrados previamente se debe usar [censAr::censo_conectar()].
#'
#' @param tabla Una cadena de texto indicando la tabla a extraer
#' @return Un tibble
#' @export
#'
#' @examples
#' \dontrun{ censo_tabla("dpto") }
censo_tabla <- function(tabla) {
  df <- tryCatch(
    tibble::as_tibble(DBI::dbReadTable(censo_conectar(), tabla)),
    error = function(e) { read_table_error(e) }
  )
  return(df)
}

#' Desconecta la Base de Datos del Censo
#'
#' Una funcion auxiliar para desconectarse de la base de datos.
#'
#' @examples
#' censo_desconectar()
#' @export
#'
censo_desconectar <- function() {
  censo_disconnect_()
}

censo_disconnect_ <- function(environment = censo_cache) {
  db <- mget("censo_conectar", envir = censo_cache, ifnotfound = NA)[[1]]
  if (inherits(db, "DBIConnection")) {
    DBI::dbDisconnect(db, shutdown = TRUE)
  }
  observer <- getOption("connectionObserver")
  if (!is.null(observer)) {
    observer$connectionClosed("CensAr", "censAr")
  }
}

censo_status <- function(msg = TRUE) {
  expected_tables <- sort(censo_tables())
  existing_tables <- sort(DBI::dbListTables(censo_conectar()))

  if (isTRUE(all.equal(expected_tables, existing_tables))) {
    status_msg <- crayon::green(paste(cli::symbol$tick,
    "La base de datos local de CensAr esta OK."))
    out <- TRUE
  } else {
    status_msg <- crayon::red(paste(cli::symbol$cross,
    "La base de datos local de CensAr esta vacia, daniada o no es compatible con tu version de duckdb. Descargala con censo_descargar()."))
    out <- FALSE
  }
  if (msg) msg(status_msg)
  invisible(out)
}

censo_tables <- function() {
  c("vivienda", "radio", "prov", "persona",
    "hogar", "dpto", "frac","variables", "variables_codificacion", "metadatos")
}

censo_cache <- new.env()
reg.finalizer(censo_cache, censo_disconnect_, onexit = TRUE)
