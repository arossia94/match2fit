import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="38", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	lamEff3 = results.lamEff3
	return np.abs(lamEff3)

