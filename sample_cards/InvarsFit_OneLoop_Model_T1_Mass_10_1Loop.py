import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="T1", collection="OneLoop", mass=10, pto="NLO", eft="NHO" )


def inv1(results):
	lambdaT13 = results.lambdaT13
	return np.abs(lambdaT13)

