import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="33", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	gQv5uqf33 = results.gQv5uqf33
	return np.abs(gQv5uqf33)

