import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="3", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	yS1f12 = results.yS1f12
	yS1f21 = results.yS1f21
	return yS1f12*yS1f21

