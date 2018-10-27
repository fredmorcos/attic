class PerfReport(object):
	def __init__(self):
		self.report = {
			"num-orders-delayed": 0,
			"total-delay-time": 0,
			"num-orders-early": 0,
			"total-early-time": 0,
			"num-orders-late": 0,
			"total-late-time": 0,
			"num-orders-exact": 0
		}
