import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="45", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	lamQ1uf3 = results.lamQ1uf3
	return np.abs(lamQ1uf3)

