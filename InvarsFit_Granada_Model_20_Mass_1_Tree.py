import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="20", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	yPhiquf33 = results.yPhiquf33
	return np.abs(yPhiquf33)

