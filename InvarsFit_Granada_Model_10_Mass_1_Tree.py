import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="10", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	yomega1qqf33 = results.yomega1qqf33
	return np.abs(yomega1qqf33)

