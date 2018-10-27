# Frederic Morcos <fred.morcos@gmail.com>
#!/bin/sh
echo "Generating Agilith documentation..." && cd src/docs && doxygen Doxyfile >> /dev/null && cd ../../ && mv src/docs/html docs && echo "Done."

