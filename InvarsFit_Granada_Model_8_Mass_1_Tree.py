import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="8", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	lamTheta1 = results.lamTheta1
	return np.abs(lamTheta1)

