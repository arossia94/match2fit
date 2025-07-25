import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="22", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	gB1H = results.gB1H
	return np.abs(gB1H)

