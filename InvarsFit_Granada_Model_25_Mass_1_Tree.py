import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="25", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	gGdf11 = results.gGdf11
	return np.abs(gGdf11)

def inv2(results):
	gGdf11 = results.gGdf11
	gGqf33 = results.gGqf33
	return (gGdf11*gGqf33)/np.abs(gGdf11)

def inv3(results):
	gGdf11 = results.gGdf11
	gGuf33 = results.gGuf33
	return (gGdf11*gGuf33)/np.abs(gGdf11)

