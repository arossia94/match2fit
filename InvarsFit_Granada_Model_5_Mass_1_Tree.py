import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="5", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	lamVarphi = results.lamVarphi
	return np.abs(lamVarphi)

def inv2(results):
	lamVarphi = results.lamVarphi
	yVarphiuf33 = results.yVarphiuf33
	return (lamVarphi*yVarphiuf33)/np.abs(lamVarphi)

