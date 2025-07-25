import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="15", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	yZetaqqf33 = results.yZetaqqf33
	return np.abs(yZetaqqf33)

