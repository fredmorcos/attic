#MODULES="../src/extra.py ../src/plant.py ../src/order.py ../src/scheduler.py ../src/constraints.py ../src/evaluator.py ../src/optimizer.py ../src/simulator.py ../src/solutionparser.py"
MODULES=`ls ../src/*.py`
for i in $MODULES
do
	echo "Testing $i..."
	python $i

	if [ $? -ne 0 ]; then
		echo "Module $i FAILED"
		break
	fi
done
