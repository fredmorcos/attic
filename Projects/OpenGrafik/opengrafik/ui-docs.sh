#!/bin/sh

echo "Generating UI documentation..." && cd src/ui/ui-docs && doxygen Doxyfile >> /dev/null && cd ../../../ && mv src/ui/ui-docs/html ui-docs && echo "Done."

