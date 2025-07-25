import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="18", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	yOmega4f33 = results.yOmega4f33
	return np.abs(yOmega4f33)

