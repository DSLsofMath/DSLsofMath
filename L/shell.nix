# with import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.03.tar.gz) {};
# with import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-20.03.tar.gz) {};
with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-21.11.tar.gz) {};

stdenv.mkDerivation {
  name = "docsEnv";
  buildInputs = [ haskellPackages.lhs2tex
                  # python3Packages.pygments
                  biber
                  # zip
                  (texlive.combine {
                       inherit (texlive)
                       algorithm2e
                       a4wide
                       acmart
                       biblatex
                       boondox
                       collection-fontsrecommended
                       comment
                       cleveref
                       ccicons
                       csquotes
                       doclicense
                       enumitem
                       environ
                       fontaxes
                       framed
                       fvextra
                       ifplatform
                       ifsym
                       imakeidx
                       inconsolata
                       kastrup
                       latexmk
                       libertine
                       listings
                       lm
                       logreq
                       mathpartir
                       minted
                       mweights
                       ncclatex
                       ncctools
                       newtx
                       newtxsf
                       newtxtt
                       newunicodechar
                       prftree
                       relsize
                       scheme-small wrapfig marvosym wasysym
                       stmaryrd
                       lazylist polytable # lhs2tex
                       tabu
                       todonotes
                       totpages
                       tikz-cd
                       trimspaces
                       thmtools
                       ucs
                       wasy cm-super unicode-math filehook lm-math capt-of
                       xargs
                       xstring ucharcat
                       xifthen
                       ifmtarg
                       varwidth
                       ;
                     })
                ];
}
