import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="16", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	yOmega1qqf33 = results.yOmega1qqf33
	return np.abs(yOmega1qqf33)

