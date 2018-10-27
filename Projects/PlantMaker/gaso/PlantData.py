# these are sorted for convenience, note that the dict object
# will change this order, machineSequence is the actual sequence
Machines = {
    "buffer": 5,
    "stripper": 1,
    "degreaser": 2,
    "pickler": 7,
    "rinser": 3,
    "fluxer": 1,
    "dryer": 4,
    "kettle": 1,
    "quencher": 1,
    "passivator": 1,
    "postrinser": 1
}

machineSequence = [
	"buffer", "stripper", "degreaser", "pickler", "rinser", "fluxer", 
	"dryer", "kettle", "quencher", "passivator", "postrinser"
]

# these are the "resources" in the plant, not sure if they'll be used
Resources = {
    "loading": 6,
    "crane": 6,
    "jig": 10,
    "unloading": 6
}

# list of queued order for each machine
ordersForBuffer = []
ordersForStripper = []
ordersForDegreaser = []
ordersForPickler = []
ordersForRinser = []
ordersForFluxer = []
ordersForDryer = []
ordersForKettle = []
ordersForQuencher = []
ordersForPassivator = []
ordersForPostRinser = []

Queued = {
	"buffer": ordersForBuffer,
    "stripper": ordersForStripper,
    "degreaser": ordersForDegreaser,
    "pickler": ordersForPickler,
    "rinser": ordersForRinser,
    "fluxer": ordersForFluxer,
    "dryer": ordersForDryer,
    "kettle": ordersForKettle,
    "quencher": ordersForQuencher,
    "passivator": ordersForPassivator,
    "postrinser": ordersForPostRinser
}

# list of currently running orders on each machine
ordersInBuffer = []
ordersInStripper = []
ordersInDegreaser = []
ordersInPickler = []
ordersInRinser = []
ordersInFluxer = []
ordersInDryer = []
ordersInKettle = []
ordersInQuencher = []
ordersInPassivator = []
ordersInPostRinser = []

Running = {
	"buffer": ordersInBuffer,
    "stripper": ordersInStripper,
    "degreaser": ordersInDegreaser,
    "pickler": ordersInPickler,
    "rinser": ordersInRinser,
    "fluxer": ordersInFluxer,
    "dryer": ordersInDryer,
    "kettle": ordersInKettle,
    "quencher": ordersInQuencher,
    "passivator": ordersInPassivator,
    "postrinser": ordersInPostRinser
}

