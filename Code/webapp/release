#!/bin/bash
cd "$(dirname "$0")"
grunt
echo
git st .
echo
sbt -DMODE=release clean package test
