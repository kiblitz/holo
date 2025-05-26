(lang dune 3.18)

(version 0.0.1)

(generate_opam_files)

(name jag)

(source
 (github kiblitz/jag))

(license MIT)

(maintainers glee)

(authors glee)

(package
 (name jag)
 (synopsis "LLVM targetted programming language")
 (description "Just Another Grammar")
 (depends
  ("ocaml"
   (>= "5.3.0"))
  "core"
  "ppx_jane"
  ("dune"
   (>= "3.18.1"))))
