import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="T2", collection="1LoopMatching", mass=10, pto="NLO", eft="NHO" )


def inv1(results):
	lambdaT23 = results.lambdaT23
	return np.abs(lambdaT23)

