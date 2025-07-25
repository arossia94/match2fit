import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="40", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	lamDelta3f3 = results.lamDelta3f3
	return np.abs(lamDelta3f3)

