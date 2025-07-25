import numpy as np
from utils import inspect_model

MODEL_SPECS = dict(id="7", collection="Granada", mass=1, pto="NLO", eft="NHO" )


def inv1(results):
	kXi1 = results.kXi1
	lampriXi1 = results.lampriXi1
	lamXi1 = results.lamXi1
	return List(kXi1**2*lampriXi1 - np.emath.sqrt(2)*kXi1**2*lamXi1 + kXi1**2*((1.0350678194517167 + np.emath.sqrt(2)*lampriXi1 - 2*lamXi1)/np.emath.sqrt(2) + (-156875625 - 151560721*np.emath.sqrt(2)*lampriXi1 + 303121442*lamXi1)/(1.51560721e8*np.emath.sqrt(2))))

def inv2(results):
	                                                                    2                 2  156875625                                     596920352                                          2
({match2fit`Private`\[Alpha]$16055[1]} /. {{}, {}}[[1 ;; All,1]]) . {2 kXi1  lampriXi1 + kXi1  (--------- + 2 lampriXi1 + -------------------------------------------------- - 2 lamXi1) - 2 kXi1  lamXi1}
                                                                                         151560721                 156875625 + 303121442 lampriXi1 - 303121442 lamXi1 = results.                                                                    2                 2  156875625                                     596920352                                          2
({match2fit`Private`\[Alpha]$16055[1]} /. {{}, {}}[[1 ;; All,1]]) . {2 kXi1  lampriXi1 + kXi1  (--------- + 2 lampriXi1 + -------------------------------------------------- - 2 lamXi1) - 2 kXi1  lamXi1}
                                                                                         151560721                 156875625 + 303121442 lampriXi1 - 303121442 lamXi1
	return Dot(ReplaceAll(List(match2fitxPrivatex\[Alpha]$16055(1)),Part(List(List(),List()),Span(1,All),1)),List(np.emath.sqrt(2)*kXi1**2*lampriXi1 + kXi1**2*(1.0350678194517167 + np.emath.sqrt(2)*lampriXi1 + 596920352/(156875625 + 151560721*np.emath.sqrt(2)*lampriXi1 - 303121442*lamXi1) - 2*lamXi1) - 2*kXi1**2*lamXi1))

def inv3(results):
	kXi1 = results.kXi1
	return List(np.abs(kXi1))

