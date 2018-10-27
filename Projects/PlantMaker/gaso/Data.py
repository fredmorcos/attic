from Order import Order

orders = [
	Order(150, {"buffer": 0, "stripper": 10, "degreaser": 20, "pickler": 15,
				"rinser": 30, "fluxer": 10, "dryer": 30, "kettle": 40,
				"quencher": 5, "passivator": 5, "postrinser": 5
               }
    ),
	Order(100, {"buffer": 0, "stripper": 5, "degreaser": 10, "pickler": 10,
				"rinser": 15, "fluxer": 5, "dryer": 15, "kettle": 20,
				"quencher": 10, "passivator": 5, "postrinser": 10
               }
    ),
	Order(200, {"buffer": 0, "stripper": 15, "degreaser": 20, "pickler": 25,
				"rinser": 25, "fluxer": 10, "dryer": 25, "kettle": 30,
				"quencher": 20, "passivator": 25, "postrinser": 20
               }
    )
]

