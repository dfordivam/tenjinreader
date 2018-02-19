#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p 'ghc.withPackages (self: with self; [ turtle ])'

{-# LANGUAGE OverloadedStrings #-}
import Turtle

main = echo str

str = "Hello, world!"
