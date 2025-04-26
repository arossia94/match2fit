import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="Xi1", collection="1LoopMatching", mass=20, pto="NLO", eft="NHO" )


def inv1(results):
	kappaXi1 = results.kappaXi1
	return np.abs(kappaXi1)

def inv2(results):
	lamXi1 = results.lamXi1
	return lamXi1

