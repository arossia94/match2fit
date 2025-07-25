import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="48", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	lamT1f3 = results.lamT1f3
	return np.abs(lamT1f3)

