#!/usr/bin/python

import Data
import Optimizer
import Plant
import Evaluation

Evaluation.evaluate(Optimizer.run(Data.orders))

