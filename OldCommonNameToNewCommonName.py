
import os
import pandas as pd


# --------------------------------------------------
# PATHS AND SETTINGS
# --------------------------------------------------

input_csv = r"C:\Users\isaia\Downloads\Inuit Bird Ranges\Arctic Outputs\FWS_ArcticBreeders.csv"

taxonomy_folder = r"C:\Users\isaia\Downloads\Authoritative Data\eBird Taxonomy"

output_csv = r"C:\Users\isaia\Downloads\Inuit Bird Ranges\Arctic Outputs\FWS_ArcticBreeders_NewComName.csv"

ambiguity_csv = r"C:\Users\isaia\Downloads\Inuit Bird Ranges\Arctic Outputs\common_name_ambiguities.csv"

# Common-name field in your input CSV
common_field = "CommonName"

years = [y for y in range(2015, 2027) if y != 2020]


# --------------------------------------------------
# HELPER FUNCTIONS
# --------------------------------------------------

def clean_name(series):
    return (
        series.fillna("")
        .astype(str)
        .str.strip()
        .str.replace(r"\s+", " ", regex=True)
        .str.casefold()
    )


def read_taxonomy(path):
    try:
        return pd.read_csv(
            path,
            dtype=str,
            usecols=["PRIMARY_COM_NAME", "SCI_NAME", "CATEGORY"],
            encoding="utf-8-sig"
        )
    except UnicodeDecodeError:
        print(f"Using Windows encoding for: {os.path.basename(path)}")
        return pd.read_csv(
            path,
            dtype=str,
            usecols=["PRIMARY_COM_NAME", "SCI_NAME", "CATEGORY"],
            encoding="cp1252"
        )


# --------------------------------------------------
# READ INPUT CSV
# --------------------------------------------------

target = pd.read_csv(input_csv, dtype=str, encoding="utf-8-sig")

if common_field not in target.columns:
    raise ValueError(f"Input CSV does not contain '{common_field}'.")

target["_common_key"] = clean_name(target[common_field])

input_names = set(target["_common_key"]) - {""}

print(f"Input contains {len(input_names)} unique common names.")


# --------------------------------------------------
# STEP 1: COMMON NAME -> SCIENTIFIC NAME
# SEARCH ALL TAXONOMY YEARS
# --------------------------------------------------

common_to_sci = {}
ambiguity_records = []

for year in years:

    taxonomy_path = os.path.join(
        taxonomy_folder,
        f"eBird_Taxonomy_v{year}.csv"
    )

    if not os.path.exists(taxonomy_path):
        print(f"Missing taxonomy file for {year}. Skipping.")
        continue

    taxonomy = read_taxonomy(taxonomy_path)

    taxonomy = taxonomy[
        taxonomy["CATEGORY"].str.strip().str.casefold() == "species"
    ].copy()

    taxonomy["_common_key"] = clean_name(taxonomy["PRIMARY_COM_NAME"])
    taxonomy["_sci_key"] = clean_name(taxonomy["SCI_NAME"])

    taxonomy = taxonomy[
        taxonomy["_common_key"].isin(input_names) &
        (taxonomy["_sci_key"] != "")
    ]

    for _, row in taxonomy.iterrows():

        common_key = row["_common_key"]
        sci_name = row["SCI_NAME"]

        common_to_sci.setdefault(common_key, set()).add(sci_name)

        ambiguity_records.append({
            "INPUT_COMMON_NAME": row["PRIMARY_COM_NAME"],
            "SCI_NAME": sci_name,
            "YEAR": year
        })

    print(f"{year}: Checked common-name matches.")


# Identify names that map to multiple scientific names
ambiguous_names = {
    name for name, sci_names in common_to_sci.items()
    if len(sci_names) > 1
}

# Only accept common names with one unique scientific name
valid_lookup = {
    name: next(iter(sci_names))
    for name, sci_names in common_to_sci.items()
    if len(sci_names) == 1
}

target["MATCHED_SCI_NAME"] = target["_common_key"].map(valid_lookup)


# --------------------------------------------------
# STEP 2: SCIENTIFIC NAME -> LATEST COMMON NAME
# SEARCH ALL TAXONOMY YEARS AGAIN
# --------------------------------------------------

matched_sci = set(
    target["MATCHED_SCI_NAME"].dropna()
)

latest_common = {}
latest_year = {}

latest_conflicts = []

for year in years:

    taxonomy_path = os.path.join(
        taxonomy_folder,
        f"eBird_Taxonomy_v{year}.csv"
    )

    if not os.path.exists(taxonomy_path):
        continue

    taxonomy = read_taxonomy(taxonomy_path)

    taxonomy = taxonomy[
        taxonomy["CATEGORY"].str.strip().str.casefold() == "species"
    ].copy()

    taxonomy["_sci_key"] = clean_name(taxonomy["SCI_NAME"])
    taxonomy["_common_key"] = clean_name(taxonomy["PRIMARY_COM_NAME"])

    taxonomy = taxonomy[
        taxonomy["SCI_NAME"].isin(matched_sci) &
        (taxonomy["_common_key"] != "")
    ]

    for sci_name, group in taxonomy.groupby("SCI_NAME"):

        common_names = group["_common_key"].unique()

        if len(common_names) > 1:
            latest_common[sci_name] = pd.NA
            latest_year[sci_name] = year

            latest_conflicts.append({
                "SCI_NAME": sci_name,
                "YEAR": year,
                "COMMON_NAMES": "; ".join(
                    group["PRIMARY_COM_NAME"].dropna().unique()
                )
            })

        else:
            latest_common[sci_name] = (
                group["PRIMARY_COM_NAME"].iloc[0]
            )
            latest_year[sci_name] = year

    print(f"{year}: Checked latest common names.")


# --------------------------------------------------
# ADD OUTPUT FIELDS
# --------------------------------------------------

target["LATEST_COMMON_NAME"] = (
    target["MATCHED_SCI_NAME"].map(latest_common)
)

target["LATEST_TAXONOMY_YEAR"] = (
    target["MATCHED_SCI_NAME"].map(latest_year)
)

target["MATCH_STATUS"] = "No match"

target.loc[
    target["MATCHED_SCI_NAME"].notna(),
    "MATCH_STATUS"
] = "Matched"

target.loc[
    target["_common_key"].isin(ambiguous_names),
    "MATCH_STATUS"
] = "Ambiguous common name"

target.loc[
    target["MATCHED_SCI_NAME"].notna() &
    target["LATEST_COMMON_NAME"].isna(),
    "MATCH_STATUS"
] = "Latest common name ambiguous"


# --------------------------------------------------
# EXPORT
# --------------------------------------------------

target.drop(columns=["_common_key"], inplace=True)

target.to_csv(
    output_csv,
    index=False,
    encoding="utf-8-sig"
)

# Export common-name ambiguity report
if ambiguous_names:

    ambiguity_df = pd.DataFrame(ambiguity_records)

    ambiguity_df = ambiguity_df[
        clean_name(ambiguity_df["INPUT_COMMON_NAME"]).isin(
            ambiguous_names
        )
    ]

    ambiguity_df = (
        ambiguity_df
        .groupby(
            ["INPUT_COMMON_NAME", "SCI_NAME"],
            as_index=False
        )
        .agg(
            YEARS=("YEAR", lambda x: ", ".join(
                map(str, sorted(x.unique()))
            ))
        )
    )

    ambiguity_df.to_csv(
        ambiguity_csv,
        index=False,
        encoding="utf-8-sig"
    )

# Report results
print("\nFinished!")
print(f"Output: {output_csv}")
print(f"Matched: {(target['MATCH_STATUS'] == 'Matched').sum()}")
print(f"Ambiguous common names: {len(ambiguous_names)}")
print(f"Unmatched: {(target['MATCH_STATUS'] == 'No match').sum()}")
print(f"Latest-name conflicts: {len(latest_conflicts)}")

if ambiguous_names:
    print("\nCommon names associated with multiple scientific names:")
    print(sorted(ambiguous_names))

if latest_conflicts:
    print("\nScientific names with multiple latest common names:")
    print(pd.DataFrame(latest_conflicts))
