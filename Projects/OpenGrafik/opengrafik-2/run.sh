# Frederic Morcos <fred.morcos@gmail.com>

#!/bin/sh

mkdir build; cd build && cmake ../ && make && cp ../src/ui/*.glade src/. && cd src && ./opengrafik

