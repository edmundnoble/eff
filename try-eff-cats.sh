#!/bin/sh
test -e ~/.coursier/cr || (mkdir -p ~/.coursier && wget -q -O ~/.coursier/cr https://git.io/vgvpD && chmod +x ~/.coursier/cr)
CLASSPATH="$(~/.coursier/cr fetch -q -p \
  \
  org.atnos:eff-cats_2.11:1.7 \
  com.lihaoyi:ammonite-repl_2.11.7:0.5.2 \
  \
)" java ammonite.repl.Main --predef 'import org.atnos.eff._, all._, syntax.all._'
