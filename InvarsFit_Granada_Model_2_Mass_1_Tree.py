import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="2", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	k3S = results.k3S
	kS = results.kS
	lamS = results.lamS
	return List(k3S*kS**3 - kS**2*lamS)

def inv2(results):
	                                                                      3            2     2     2    1          2
({match2fit`Private`\[Alpha]$17395[1]} /. {{}, {}}[[1 ;; All,1]]) . {2 k3S kS  lamS - 2 kS  lamS  + kS  (-(-) - 2 lamS )}
                                                                                                    2 = results.                                                                      3            2     2     2    1          2
({match2fit`Private`\[Alpha]$17395[1]} /. {{}, {}}[[1 ;; All,1]]) . {2 k3S kS  lamS - 2 kS  lamS  + kS  (-(-) - 2 lamS )}
                                                                                                    2
	return Dot(ReplaceAll(List(match2fitxPrivatex\[Alpha]$17395(1)),Part(List(List(),List()),Span(1,All),1)),List(2*k3S*kS**3*lamS - 2*kS**2*lamS**2 + kS**2*(-0.5 - 2*lamS**2)))

def inv3(results):
	k3S = results.k3S
	return List(np.abs(k3S))

