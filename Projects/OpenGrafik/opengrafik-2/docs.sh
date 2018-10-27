# Frederic Morcos <fred.morcos@gmail.com>
#!/bin/sh
echo "Generating Canvas documentation..." && cd src/canvas/canvas-docs && doxygen Doxyfile >> /dev/null && cd ../../../ && mv src/canvas/docs/html docs && echo "Done."

