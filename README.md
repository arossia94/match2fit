# match2fit
Mathematica package to interface UV-EFT matching codes with SMEFT global fit codes. It interfaces matchmakereft and SMEFiT and supports one-loop-level matching results.

matchmakereft is a prerequisite for full functionality and hence requires using Linux/Mac as OS. 
Except for the matching functionality, the rest of the match2fit package should work on Windows too.

# Documentation

The User's Manual is included as a PDF file. Alternatively, a continuously evolving version can be seen online [here](https://www.overleaf.com/read/ysvstxwhyvsz)

# Usage
See the User's Manual and the PackageTester notebook for usage examples.

The runcards can be parsed into the SMEFiT format with [this script](https://github.com/LHCfitNikhef/smefit_release/blob/FCC_Feas_Rep/runcards/uv_models/write_runcards.py).

Examples of the usage of this package can be found in the following scientific publications:
 - J. ter Hoeve, Alejo N. Rossia, et al, "The Automation of SMEFT-Assisted Constraints on UV-Complete Models" [arXiv:2309.04523, JHEP 01 (2024) 179](https://inspirehep.net/literature/2696156)
 - E. Celada, Alejo N. Rossia, et al, "Mapping the SMEFT at High-Energy Colliders: from LEP and the (HL-)LHC to the FCC-ee" [arXiv:2404.12809, JHEP 09 (2024) 091](https://inspirehep.net/literature/2779255)
 - The ECFA Higgs, Electroweak, and Top Factory Study [arXiv:2506.15390, CERN Yellow Rep.Monogr. 5 (2025)](https://inspirehep.net/literature/2936712)
 - Future Circular Collider Feasibility Study Report: Volume 1, Physics, Experiments, Detectors [arXiv:2505.00272, Eur.Phys.J.C 85 (2025) 12, 1468](https://inspirehep.net/literature/2917350)
 - J. ter Hoeve, Alejo N. Rossia, et al, "Connecting Scales: RGE Effects in the SMEFT at the LHC and Future Colliders" [arXiv:2502.20453, JHEP 06 (2025) 125](https://inspirehep.net/literature/2895783)
 - J. ter Hoeve, Alejo N. Rossia, et al, "The Higgs trilinear coupling in the SMEFT at the HL-LHC and the FCC-ee" [arxiv:2504.05974, Phys. Rev. D 112 (2025) 1, 013008](https://inspirehep.net/literature/2909430)

# Citation

If you use this package and/or the UV capabilities of SMEFiT in a scientific publication, please cite

> Jaco ter Hoeve, Giacomo Magni, Juan Rojo, Alejo N. Rossia, and Eleni Vryonidou
>
> "The Automation of SMEFT-Assisted Constraints on UV-Complete Models"
> 
> DOI: [10.1007/JHEP01(2024)179](https://link.springer.com/article/10.1007/JHEP01(2024)179)
>
> Published in: JHEP 01 (2024), 179
>
> [https://arxiv.org/abs/2309.04523](https://inspirehep.net/literature/2696156)

# Acknowledgements

This work has been supported by the European Research Council (ERC) under the European Union’s Horizon 2020 research
and innovation programme (Grant agreement No. 949451) and by a Royal Society University
Research Fellowship through grant URF/R1/201553.
The author thanks the support, encouragement and discussions with  M. Chala, T. Giani, J. ter Hoeve, G. Magni, F. Maltoni, L. Mantani, K. Mimasu, Y. Oda, J. Pagès, J. Rojo, J. Santiago, C. Severi, A. Thomsen, and E. Vryonidou.
