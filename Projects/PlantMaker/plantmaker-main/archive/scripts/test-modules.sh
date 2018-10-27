MODULES="../src/extra.py ../src/plant.py ../src/order.py ../src/constraints.py ../src/scheduler.py ../src/evaluator.py ../src/solutionparser.py ../src/simulatorutils.py ../src/printer.py ../src/schedule.py ../src/simulator.py ../src/utils.py ../src/optimizer.py ../src/plantmaker.py"

for i in $MODULES
do
	echo "Testing $i..."
	python $i

	if [ $? -ne 0 ]; then
		echo "Module $i FAILED"
		break
	fi
done
