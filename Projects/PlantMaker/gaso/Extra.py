def status(timestamp, msg, order = None):
	if order == None:
		print "{0:.<10}{1}".format(timestamp, msg)
	else:
		print "{0:.<10}{1} {2}".format(timestamp, "Order " + order.id, msg)

