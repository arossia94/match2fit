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
	gWtiLe11 = results.gWtiLe11
	gWtiLe22 = results.gWtiLe22
	return 0. - 0.5*gWtiLe12*gWtiLe21 + 1.*gWtiLe11*gWtiLe22

def inv3(results):
	gWtiQ13 = results.gWtiQ13
	gWtiQ31 = results.gWtiQ31
	return gWtiQ13*gWtiQ31

def inv4(results):
	gWtiH = results.gWtiH
	gWtiLe11 = results.gWtiLe11
	return (gWtiH*gWtiLe11)/np.abs(gWtiLe11)

def inv5(results):
	gWtiLe11 = results.gWtiLe11
	return np.abs(gWtiLe11)

def inv6(results):
	gWtiH = results.gWtiH
	gWtiLe22 = results.gWtiLe22
	return (gWtiH*gWtiLe22)/np.abs(gWtiH)

def inv7(results):
	gWtiH = results.gWtiH
	gWtiLe33 = results.gWtiLe33
	return (gWtiH*gWtiLe33)/np.abs(gWtiH)

def inv8(results):
	gWtiQ11 = results.gWtiQ11
	return np.abs(gWtiQ11)

def inv9(results):
	gWtiH = results.gWtiH
	gWtiQ22 = results.gWtiQ22
	return (gWtiH*gWtiQ22)/np.abs(gWtiH)

def inv10(results):
	gWtiQ11 = results.gWtiQ11
	gWtiQ33 = results.gWtiQ33
	return (gWtiQ11*gWtiQ33)/np.abs(gWtiQ11)

def inv11(results):
	lambdaQ13 = results.lambdaQ13
	return np.abs(lambdaQ13)

def inv12(results):
	lambdaQ73 = results.lambdaQ73
	return np.abs(lambdaQ73)

