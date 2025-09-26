import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="Xi", collection="1LoopMatching", mass=20, pto="NLO", eft="NHO" )


def inv1(results):
	kappaXi = results.kappaXi
	return np.abs(kappaXi)

