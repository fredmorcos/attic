#!/bin/sh

cd FrontStep/
make
sudo -E make install

cd ..

cd OpenGrafik/
make
sudo -E make install

