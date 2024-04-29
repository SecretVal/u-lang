#!/usr/bin/env sh
set -xe
rm -rf main.asm main
cargo r -- main.ul
