import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="24", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	gW1H = results.gW1H
	return np.abs(gW1H)

