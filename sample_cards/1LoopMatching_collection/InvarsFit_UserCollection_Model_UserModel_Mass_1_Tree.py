import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="UserModel", collection="UserCollection", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	lambdaQ12 = results.lambdaQ12
	lambdaQ72 = results.lambdaQ72
	return 0. + 1.*lambdaQ12**2 - 1.*lambdaQ72**2

def inv2(results):
	gWtiLe12 = results.gWtiLe12
	gWtiLe21 = results.gWtiLe21
	return gWtiLe12*gWtiLe21

def inv3(results):
	gWtiLe13 = results.gWtiLe13
	gWtiLe31 = results.gWtiLe31
	return gWtiLe13*gWtiLe31

def inv4(results):
	gWtiLe23 = results.gWtiLe23
	gWtiLe32 = results.gWtiLe32
	return gWtiLe23*gWtiLe32

def inv5(results):
	gWtiQ13 = results.gWtiQ13
	gWtiQ31 = results.gWtiQ31
	return gWtiQ13*gWtiQ31

def inv6(results):
	gWtiH = results.gWtiH
	gWtiLe11 = results.gWtiLe11
	return (gWtiH*gWtiLe11)/np.abs(gWtiLe11)

def inv7(results):
	gWtiLe11 = results.gWtiLe11
	return np.abs(gWtiLe11)

def inv8(results):
	gWtiLe11 = results.gWtiLe11
	gWtiLe22 = results.gWtiLe22
	return (gWtiLe11*gWtiLe22)/np.abs(gWtiLe11)

def inv9(results):
	gWtiLe11 = results.gWtiLe11
	gWtiLe33 = results.gWtiLe33
	return (gWtiLe11*gWtiLe33)/np.abs(gWtiLe11)

def inv10(results):
	gWtiLe11 = results.gWtiLe11
	gWtiQ11 = results.gWtiQ11
	return (gWtiLe11*gWtiQ11)/np.abs(gWtiLe11)

def inv11(results):
	gWtiH = results.gWtiH
	gWtiQ22 = results.gWtiQ22
	return (gWtiH*gWtiQ22)/np.abs(gWtiH)

def inv12(results):
	gWtiQ11 = results.gWtiQ11
	gWtiQ33 = results.gWtiQ33
	return (gWtiQ11*gWtiQ33)/np.abs(gWtiQ11)

def inv13(results):
	lambdaQ13 = results.lambdaQ13
	return np.abs(lambdaQ13)

def inv14(results):
	lambdaQ73 = results.lambdaQ73
	return np.abs(lambdaQ73)

