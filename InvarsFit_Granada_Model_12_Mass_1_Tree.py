import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="12", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	yomega4uuf33 = results.yomega4uuf33
	return np.abs(yomega4uuf33)

