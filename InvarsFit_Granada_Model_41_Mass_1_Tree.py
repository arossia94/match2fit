import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="41", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	lamSigmaf3 = results.lamSigmaf3
	return np.abs(lamSigmaf3)

