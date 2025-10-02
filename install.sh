#!/usr/bin/env bash
set -e
FUNMACS_DIR="$(cd "$(dirname "$0")" && pwd)"
CONFIG_DIR="$HOME/.config"
EMACS_DIR="$CONFIG_DIR/emacs"
if [ -d "$EMACS_DIR" ] && [ ! -L "$EMACS_DIR" ]; then
  mv "$EMACS_DIR" "$EMACS_DIR.bak.$(date +%s)"
fi
ln -sfn "$FUNMACS_DIR" "$EMACS_DIR"
echo "Funmacs installed to $EMACS_DIR"
