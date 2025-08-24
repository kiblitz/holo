(lang dune 3.18)

(version 0.0.1)

(generate_opam_files)

(name holo)

(source
 (github kiblitz/holo))

(license MIT)

(maintainers glee)

(authors glee)

(package
 (name holo)
 (synopsis "LLVM targetted programming language")
 (description "LLVM targetted programming language")
 (depends
  "core"
  "core_kernel"
  ("dune"
   (>= "3.18.1"))
  ("ocaml"
   (>= "5.3.0"))
  "ppx_jane"))
