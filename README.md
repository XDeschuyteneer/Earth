Earth
=====

Earth 3D visualization using ocaml and Three.js library

Planet textures found: http://planetpixelemporium.com/planets.html

Compilation
===========
>>OCaml compilation
    ocamlbuild -use-ocamlfind -pkgs js_of_ocaml,js_of_ocaml.syntax,unix -syntax camlp4o index.byte
>>OCaml to Javascript compilation
    js_of_ocaml index.byte

Run
===

Because we are loading textures from files:

google-chrome --allow-file-access-from-files index.html
