# -*- coding: utf-8 -*-
import argparse
import pathlib

import yaml

here = pathlib.Path(__file__).parent

MAX_VALUE = 100
MIN_VALUE = -100


def load_base(collider) -> dict:
    """Load basic runcard."""
    with open(here / f"base_runcard_{collider}.yaml", encoding="utf-8") as f:
        card = yaml.safe_load(f)
    return card


def parse_UV_coeffs(model_dict: dict) -> dict:
    """Parse the UV coefficient dictionay."""
    coeff_dict = {}
    free_dofs = []

    # add uv couplings
    for c in model_dict["UV couplings"]:
        free_dofs.append(c)
        coeff_dict[c] = {"min": MIN_VALUE, "max": MAX_VALUE}

    # now add the non linear relations
    for coeff, rel in model_dict.items():
        if not coeff.startswith("c"):
            continue

        # drop (fix to zero) WC with zero coefficient
        if len(rel) == 1:
            # new syntax
            if any(val[0] == 0 for val in rel[0].values()):
                continue

        new_constrain = []
        for sum_element in rel:
            new_addend = {}
            is_non_zero = False
            for c, pair in sum_element.items():
                # if there is 0 drop the constrain
                if pair[0] == 0:
                    is_non_zero = True
                    continue
                if pair == [1, 0]:
                    continue
                if not is_non_zero:
                    new_addend[c] = pair
            if new_addend != {}:
                new_constrain.append(new_addend)

        coeff_dict[f"O{coeff[1:]}"] = {
            "constrain": new_constrain,
            "min": -MAX_VALUE,
            "max": MAX_VALUE,
        }
    return coeff_dict


def dump_runcard(
    collection: str,
    idx_model: int,
    eft_order: str,
    pto: str,
    fitting_mode: str,
    mass: str,
    collider: str,
    path: str
) -> None:
    """Parse a model card to a SMEFiT runcard."""
    if path =="--":
        if "OneLoop" in collection:
            file_path = f"UV_scan/{collection}/out_UV_dict_Coll_{collection}_Mod_{idx_model}_Mass_{mass}_Loop.yaml"
        elif "Multiparticle" in collection:
            file_path = f"UV_scan/{collection}/out_UV_dict_Coll_MultiParticleCollection_Mod_{idx_model}_Mass_{mass}_Tree.yaml"
        else:
            file_path = f"UV_scan/{collection}/out_UV_dict_Coll_{collection}_Mod_{idx_model}_Mass_{mass}_Tree.yaml"
    else:
        file_path = path

    with open(here / file_path, "r", encoding="utf-8") as f:
        model_dict = yaml.safe_load(f)

    runcard = load_base(collider)

    # orders
    runcard["order"] = pto
    runcard["use_quad"] = eft_order == "HO"

    # names
    #runcard["result_ID"] = f"Model_UV_{idx_model}_{pto}_{eft_order}"
    runcard["Model name"] = model_dict["Model name"]
    runcard["UV Collection"] = model_dict["UV Collection"]
    runcard["UV model"] = model_dict["UV model"]
    runcard["uv_couplings"] = True

    coeff_dict = parse_UV_coeffs(model_dict)
    flag = "UV"

    runcard["coefficients"] = coeff_dict
    with open(
        f"{here.parent}/{collection}_{collider}_{flag}_{idx_model}_{mass}_{pto}_{eft_order}_{fitting_mode}.yaml",
        "w",
        encoding="utf-8",
    ) as f:
        yaml.dump(runcard, f)


if __name__ == "__main__":

    parser = argparse.ArgumentParser(
        prog="SMEFiT_runcards",
        description="Write SMEFIT runcards for WC/UV models",
    )
    parser.add_argument("-i", "--model_idx", help="Model ID", required=True)
    parser.add_argument(
        "-e", "--eft_order", help="EFT order: HO, NHO", type=str, required=True
    )
    parser.add_argument(
        "-o", "--qcd_order", help="QCD order: LO, NLO", type=str, required=True
    )
    parser.add_argument(
        "-f", "--mode", help="Fitting mode: NS, MC", type=str, default="NS"
    )
    parser.add_argument(
        "-c",
        "--collection",
        help="Model collection: Granada, Fitmaker",
        type=str,
        required=True,
    )
    parser.add_argument("-m", "--mass", help="Particle masses", type=str, default="1")
    parser.add_argument("-d", "--collider", help="Collider to pick base runcard", type=str, required=True)
    parser.add_argument("-p", "--path", help="Path of the Match2Fit output to convert.", type=str,default="--")
    args = parser.parse_args()

    dump_runcard(
        args.collection,
        args.model_idx,
        args.eft_order,
        args.qcd_order,
        args.mode,
        args.mass,
        args.collider,
        args.path
    )
