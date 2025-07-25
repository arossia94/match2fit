import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="43", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	lamUf3 = results.lamUf3
	return np.abs(lamUf3)

