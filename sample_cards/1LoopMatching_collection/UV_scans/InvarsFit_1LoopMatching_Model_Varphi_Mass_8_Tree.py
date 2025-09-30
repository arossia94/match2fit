import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="Varphi", collection="1LoopMatching", mass=8, pto="NLO", eft="NHO" )


def inv1(results):
	lamvarphi = results.lamvarphi
	return np.abs(lamvarphi)

def inv2(results):
	lamvarphi = results.lamvarphi
	yVarphiuf33 = results.yVarphiuf33
	return (lamvarphi*yVarphiuf33)/np.abs(lamvarphi)

