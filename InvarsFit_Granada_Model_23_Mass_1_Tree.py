import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="23", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	gWH = results.gWH
	return np.abs(gWH)

def inv2(results):
	gWH = results.gWH
	gWLf11 = results.gWLf11
	return (gWH*gWLf11)/np.abs(gWH)

def inv3(results):
	gWH = results.gWH
	gWLf22 = results.gWLf22
	return (gWH*gWLf22)/np.abs(gWH)

def inv4(results):
	gWH = results.gWH
	gWLf33 = results.gWLf33
	return (gWH*gWLf33)/np.abs(gWH)

def inv5(results):
	gWH = results.gWH
	gWqf11 = results.gWqf11
	return (gWH*gWqf11)/np.abs(gWH)

def inv6(results):
	gWH = results.gWH
	gWqf33 = results.gWqf33
	return (gWH*gWqf33)/np.abs(gWH)

