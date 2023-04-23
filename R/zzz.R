.onAttach <- function(...) {
  msg(cli::rule(crayon::bold("CENSOS ARGENTINA")))
  msg(" ")
  msg("La documentacion del paquete {censo2017} sobre el que está basado {censAr} y ejemplos de uso se encuentran en https://pacha.dev/censo2017/.")
  msg("Visita https://buymeacoffee.com/pacha si deseas donar para contribuir al desarrollo de software como este.")
  msg("Esta libreria necesita XX GB libres para la crear la base de datos localmente. Una vez creada la base, esta ocupa YYY GB en disco.")
  msg(" ")
  if (interactive() && Sys.getenv("RSTUDIO") == "1"  && !in_chk()) {
    censo_pane()
  }
  if (interactive()) censo_status()
}
