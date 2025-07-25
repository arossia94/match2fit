import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="47", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	lamQ7f3 = results.lamQ7f3
	return np.abs(lamQ7f3)

