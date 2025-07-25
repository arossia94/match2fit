import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="44", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	lamDff3 = results.lamDff3
	return np.abs(lamDff3)

