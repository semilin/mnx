# mnx
mnx is a mini nix for Arch. I found using Guix along with Arch to be
too much of a hassle; the difficulties greatly outweighed the
benefits. So this is my new config system. It handles declarative
packagement through Pacman and links config files immutably throughout
the system, which covers 90% of what I was using Guix for with <60
lines of Common Lisp. It's also much faster.
