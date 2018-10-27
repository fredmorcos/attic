# MODULES="../src/extra.py ../src/plant.py ../src/order.py ../src/scheduler.py ../src/plantmaker.py ../src/constraints.py ../src/evaluator.py ../src/optimizer.py ../src/simulator.py ../src/solutionparser.py"
MODULEs=`ls ../src/*.py`
cd ../docs;

echo "Generating documentation for project..."
pydoc -w $MODULES

if [ $? -ne 0 ]; then
	echo "Failed."
fi

