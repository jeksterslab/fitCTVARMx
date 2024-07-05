#!/bin/bash

git clone git@github.com:jeksterslab/fitCTVARMx.git
rm -rf "$PWD.git"
mv fitCTVARMx/.git "$PWD"
rm -rf fitCTVARMx
