#-----------------------------------------------------------------------

rm(list = ls())

#-----------------------------------------------------------------------
# Copia figuras do repositório do EstBas.

file <- "./102_tipos_de_variaveis.Rmd"
file_figs <- system(sprintf("cd .. && grep 'img/' %s", file),
                    intern = TRUE)
file_figs <- grep(x = file_figs, pattern = "#",
                  value = TRUE, invert = TRUE)
file_figs

used_figs <- sub('.*@(.+)@.*', "\\1", gsub('\\"', "@", file_figs))
used_figs <- unique(basename(used_figs))
used_figs

file.copy(from = sprintf("~/Projects/estbas_pe/img/%s", used_figs),
          to = sprintf("../img/%s", used_figs))

#-----------------------------------------------------------------------

# Figuras do diretório.
figs <- dir(path = "../img", full.names = FALSE)
figs

# Figuras que estão sob versionamento.
tracked_figs <- system("git ls-files ../img/", intern = TRUE)
tracked_figs <- basename(tracked_figs)
tracked_figs

# Figuras mencionadas nos arquivos rastreados.
# file_figs <- system("grep 'img/' *.Rmd", intern = TRUE)
# file_figs <- system("cd .. && grep 'img/' *.Rmd", intern = TRUE)
file_figs <- system("cd .. && grep 'img/' $(git ls-files *.Rmd)", intern = TRUE)
file_figs

used_figs <- sub('.*@(.+)@.*', "\\1", gsub('\\"', "@", file_figs))
used_figs <- unique(basename(used_figs))
# used_figs <- grep(x = used_figs, "\\.(png|jpg|jpeg)$", value = TRUE)
used_figs

# Mencionadas mas que não estão presentes.
not_found <- setdiff(used_figs, figs)
not_found

# Copia figuras de outro projeto.
# file.copy(from = sprintf("/home/walmes/Projects/dsbd_lingprog/img/%s", not_found),
#           to = sprintf("../img/%s", not_found))

# Usadas e não rastreadas.
found_figs <- intersect(used_figs, figs)
figs_to_add <- setdiff(found_figs, tracked_figs)
figs_to_add

system(sprintf("cd ../img && git add %s",
               paste(figs_to_add, collapse = " ")))
system("git commit -m 'Adds figures.'")

# Rastreadas e não usadas. Então remover.
setdiff(figs, used_figs)
setdiff(figs, tracked_figs)
setdiff(tracked_figs, used_figs)
setdiff(used_figs, tracked_figs)

file.remove(paste0("../img/", setdiff(figs, tracked_figs)))
