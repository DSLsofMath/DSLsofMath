# with import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.03.tar.gz) {};
# with import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-20.03.tar.gz) {};
# with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-21.11.tar.gz) {};
with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-23.11.tar.gz) {};

stdenv.mkDerivation {
  name = "docsEnv";
  buildInputs = [ haskellPackages.lhs2tex
                  # python3Packages.pygments
                  biber
                  # zip
                  (texlive.withPackages (pkgs: [
                       pkgs.algorithm2e
                       pkgs.a4wide
                       pkgs.acmart
                       pkgs.biblatex
                       pkgs.boondox
                       pkgs.collection-fontsrecommended
                       pkgs.comment
                       pkgs.cleveref
                       pkgs.ccicons
                       pkgs.csquotes
                       pkgs.doclicense
                       pkgs.enumitem
                       pkgs.environ
                       pkgs.fontaxes
                       pkgs.framed
                       pkgs.fvextra
                       pkgs.ifplatform
                       pkgs.ifsym
                       pkgs.imakeidx
                       pkgs.inconsolata
                       pkgs.kastrup
                       pkgs.latexmk
                       pkgs.libertine
                       pkgs.listings
                       pkgs.lm
                       pkgs.logreq
                       pkgs.mathpartir
                       pkgs.minted
                       pkgs.mweights
                       pkgs.ncclatex
                       pkgs.ncctools
                       pkgs.newtx
                       pkgs.newtxsf
                       pkgs.newtxtt
                       pkgs.newunicodechar
                       pkgs.prftree
                       pkgs.relsize
                       pkgs.scheme-small pkgs.wrapfig pkgs.marvosym pkgs.wasysym
                       pkgs.stmaryrd
                       pkgs.lazylist pkgs.polytable # lhs2tex
                       pkgs.tabu
                       pkgs.todonotes
                       pkgs.totpages
                       pkgs.tikz-cd
                       pkgs.trimspaces
                       pkgs.thmtools
                       pkgs.ucs
                       pkgs.wasy pkgs.cm-super pkgs.unicode-math pkgs.filehook pkgs.lm-math pkgs.capt-of
                       pkgs.xargs
                       pkgs.xstring pkgs.ucharcat
                       pkgs.xifthen
                       pkgs.ifmtarg
                       pkgs.varwidth
		       pkgs.doi
                       ]
                     ))
                ];
}
