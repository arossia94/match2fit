import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="27", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	gHf33 = results.gHf33
	return np.abs(gHf33)

