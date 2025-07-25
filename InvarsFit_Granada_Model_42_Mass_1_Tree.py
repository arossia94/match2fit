import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="42", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	lamSigma1f3 = results.lamSigma1f3
	return np.abs(lamSigma1f3)

