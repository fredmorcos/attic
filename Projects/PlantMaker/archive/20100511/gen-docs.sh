MODULES="../extra.py ../plant.py ../order.py ../scheduler.py ../plantmaker.py"

cd docs;

echo "Generating documentation for project..."
pydoc -w $MODULES

if [ $? -ne 0 ]; then
	echo "Failed."
fi

