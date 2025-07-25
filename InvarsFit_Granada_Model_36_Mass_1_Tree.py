import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="36", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	gY5f33 = results.gY5f33
	return np.abs(gY5f33)

