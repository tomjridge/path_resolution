(lang dune 2.0)
(name tjr_path_resolution)

;; for opam
(authors              "Tom Ridge <tom.j.ridge@googlemail.com>")
(maintainers          "Tom Ridge <tom.j.ridge@googlemail.com>")
(source               (github tomjridge/path_resolution))
(homepage             https://github.com/tomjridge/path_resolution)
(documentation        https://tomjridge.github.io/ocamldocs)
(generate_opam_files  true)


(package 
 (name tjr_path_resolution)
 (synopsis "Abstract path resolution (part of ImpFS, or usable independently)")
 (depends 
   tjr_monad
   tjr_fs_shared))
