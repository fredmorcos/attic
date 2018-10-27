MODULES="extra.py plant.py order.py scheduler.py"

for i in $MODULES
do
	echo "Testing $i..."
	python $i

	if [ $? -ne 0 ]; then
		echo "Module $i FAILED"
		break
	fi
done

