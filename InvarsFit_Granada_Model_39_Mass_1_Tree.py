import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="39", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	lamDelta1f3 = results.lamDelta1f3
	return np.abs(lamDelta1f3)

