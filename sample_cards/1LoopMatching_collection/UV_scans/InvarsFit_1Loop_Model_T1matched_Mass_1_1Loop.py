import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="T1matched", collection="1Loop", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	lambdaT13 = results.lambdaT13
	return np.abs(lambdaT13)

