echo $PATH | grep -q "$(pwd)/bin" || export PATH="$PATH:$(pwd)/bin/"
echo $PYTHONPATH | grep -q "$(pwd)/modules" || export PYTHONPATH="$PYTHONPATH:$(pwd)/modules"