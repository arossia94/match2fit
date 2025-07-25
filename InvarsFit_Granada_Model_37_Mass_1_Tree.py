import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="37", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	lamNef3 = results.lamNef3
	return np.abs(lamNef3)

