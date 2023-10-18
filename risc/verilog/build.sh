#!/usr/bin/bash

set -xe

iverilog -o risc risc16.v 
vvp risc


