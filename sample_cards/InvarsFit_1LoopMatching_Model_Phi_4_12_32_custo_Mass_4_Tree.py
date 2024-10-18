import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="Phi_4_12_32_custo", collection="1LoopMatching", mass=4, pto="NLO", eft="NHO" )


def inv1(results):
	lamPhi = results.lamPhi
	return np.abs(lamPhi)

