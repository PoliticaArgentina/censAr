sql_action <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      exists("documentNew", asNamespace("rstudioapi"))) {
    contents <- paste(
      "-- !preview conn=censAr::censo_conectar()",
      "",
      "SELECT * FROM comunas",
      "",
      sep = "\n"
    )

    rstudioapi::documentNew(
      text = contents, type = "sql",
      position = rstudioapi::document_position(2, 40),
      execute = FALSE
    )
  }
}

censo_pane <- function() {
  observer <- getOption("connectionObserver")
  if (!is.null(observer) && interactive()) {
    observer$connectionOpened(
      type = "CensAr",
      host = "censAr",
      displayName = "Tablas Censo 2010",
      icon = system.file("img", "censAr.png", package = "censAr"),
      connectCode = "censAr::censo_pane()",
      disconnect = censAr::censo_desconectar,
      listObjectTypes = function() {
        list(
          table = list(contains = "data")
        )
      },
      listObjects = function(type = "datasets") {
        tbls <- DBI::dbListTables(censo_conectar())
        data.frame(
          name = tbls,
          type = rep("table", length(tbls)),
          stringsAsFactors = FALSE
        )
      },
      listColumns = function(table) {
        res <- DBI::dbGetQuery(censo_conectar(),
                               paste("SELECT * FROM", table, "LIMIT 1"))
        data.frame(
          name = names(res), type = vapply(res, function(x) class(x)[1],
                                           character(1)),
          stringsAsFactors = FALSE
        )
      },
      previewObject = function(rowLimit, table) {
        DBI::dbGetQuery(censo_conectar(),
                        paste("SELECT * FROM", table, "LIMIT", rowLimit))
      },
      actions = list(
        Status = list(
          icon = system.file("img", "ropensci-logo.png", package = "censAr"),
          callback = censo_status
        ),
        SQL = list(
          icon = system.file("img", "edit-sql.png", package = "censAr"),
          callback = sql_action
        )
      ),
      connectionObject = censo_conectar()
    )
  }
}

update_censo_pane <- function() {
  observer <- getOption("connectionObserver")
  if (!is.null(observer)) {
    observer$connectionUpdated("CensAr", "censAr", "")
  }
}
