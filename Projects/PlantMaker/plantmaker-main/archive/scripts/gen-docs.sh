MODULES="../src/extra.py ../src/plant.py ../src/order.py ../src/constraints.py ../src/scheduler.py ../src/evaluator.py ../src/solutionparser.py ../src/simulatorutils.py ../src/printer.py ../src/schedule.py ../src/simulator.py ../src/utils.py ../src/optimizer.py ../src/plantmaker.py"

cd ../docs;

echo "Generating documentation for project..."

pydoc -w $MODULES

if [ $? -ne 0 ]; then
	echo "Failed."
fi

