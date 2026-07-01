from arcgis.gis import GIS
from arcgis.features import analysis
import pandas as pd
import urllib3
import logging
from datetime import datetime
import os
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

def setup_logging(log_name="update_geometry", log_folder="logs"):
    """
    Sets up logging for the script.
    Creates timestamped log files and logs to both file and console.
    """

    if not os.path.exists(log_folder):
        os.makedirs(log_folder)

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    log_file_path = os.path.join(log_folder, f"{log_name}_{timestamp}.log")

    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s | %(levelname)s | %(message)s",
        handlers=[
            logging.FileHandler(log_file_path),
            logging.StreamHandler()
        ]
    )

    logging.info("Logging initialized.")
    return log_file_path

def filter_undissolved_polygons(undissolved_sdf):
    """
    Filter out undissolved polygons based on parameters (marked for removal, no space_place_id, etc)
    :param undissolved_sdf: raw undissolved spatial dataframe
    :return: a filtered version of the undissolved spatial dataframe

    NOTE: add more filters here as needed
    """
    # drop records where space and place id is null (but keep all fields)
    undissolved_sdf = undissolved_sdf.dropna(subset=["space_place_id"])

    # TODO: Drop rows where the remove field is true

    return undissolved_sdf

def aggregate_attributes(undissolved_sdf, domain_dict):
    """
    Run a group by to aggregate geometry related attributes before dissolve
    :param undissolved_sdf: The undissolved spatial dataframe that contains the geometry related attributes to be aggregated
    :param domain_dict: A dictionary that maps the original polygon_source values to their desired values
    :return: A dataframe that contains the aggregated attributes for features with dissolved geometry
    """
    undissolved_sdf = undissolved_sdf.copy()
    undissolved_sdf["polygon_source"] = (
        undissolved_sdf["polygon_source"]
        .map(domain_dict)
        .fillna(undissolved_sdf["polygon_source"])
    )

    aggregated_df = (
        undissolved_sdf.groupby("space_place_id")
        .agg(
            {
                "polygon_source": lambda x: ", ".join(sorted(x.dropna().astype(str).unique())),
                "digitized_building": lambda x: 0 if x.fillna(0).sum() == 0 else 1,
            }
        )
        .reset_index()
    )

    return aggregated_df


def dissolve_geometry(sdf):
    """
    Dissolve geometries by space_place_id
    :param layer: The input layer to be dissolved
    :return: A spatial dataframe that contains the dissolved geometries based on space_place_id, which will be used for geometry comparison and update.
    """
    # convert to feature layer
    feature_set = sdf.spatial.to_featureset()
    feature_dict = feature_set.to_dict()

    # dissolve geometries by space_place_id
    dissolved_fco = analysis.dissolve_boundaries(
        feature_dict, dissolve_fields=["space_place_id"], multi_part_features=True
    )
    # The result of dissolve_boundaries is a FeatureCollectionObject, so we need to query it to get a spatial dataframe
    dissolved_sdf = dissolved_fco.query().sdf
    logging.info(f"Dissolved geometries by space_place_id ::: {len(dissolved_sdf)}")

    return dissolved_sdf


def project_and_calculate_acres(geo_df, spatial_ref):
    """
    Project to appropriate spatial reference and calculate acres
    :param geo_df: dissolved spatial dataframe
    :param spatial_ref: the spatial reference constant to project to for accurate area calculations)

    :return:  None. Inplace projection and acres calculation are performed on the input dataframe.
    """

    geo_df.spatial.project(spatial_reference=spatial_ref)

    # Calculate acres using the area of the projected geometry (which will be in square meters) and converting to acres (1 square meter = 0.000247105 acres)
    geo_df["sp_gis_acres_unverified"] = geo_df["SHAPE"].apply(
        lambda g: g.area * 0.000247105
    )

    return


def join_attri_and_rename_fields(in_df, aggregated_df):
    """
    Joins aggregated attributes back to projected dissolved geometries and prepares final dataframe for update.

    :param in_df: The spatial dataframe that contains the projected dissolved geometries and calculated acres for features with changed geometry
    :param aggregated_df: The dataframe that contains the aggregated attributes for features with dissolved geometry
    
    :return: None. The input dataframe is modified in place by joining the aggregated attributes and renaming the columns.
    """
    # Join the aggregated attributes back to the projected dissolved geometries
    in_df = in_df.join(
        aggregated_df.set_index("space_place_id"), on="space_place_id", how="left"
    )
    # Select and rename the relevant columns for the final update dataframe
    in_df = in_df[
        [
            "space_place_id",
            "Count",
            "sp_gis_acres_unverified",
            "polygon_source",
            "digitized_building",
            "SHAPE",
        ]
    ]
    # Rename columns to match source schema
    in_df.rename(
        columns={
            "Count": "dissolved_polygon_count",
            "polygon_source": "polygon_source_list",
        },
        inplace=True,
    )

    return in_df


def polygons_to_centroids(polygon_sdf):
    """
    Converts the dissolved polygon geometries to centroid points for the points layer update,
    
    :param polygon_sdf: The spatial dataframe that contains the dissolved polygon geometries and aggregated attributes for features with changed geometry, 
    which will be used to create the point geometries for the points layer update.
    
    :return: A spatial dataframe that contains the centroid point geometries and aggregated attributes for features with changed geometry
    """
    points_sdf = polygon_sdf.copy()
    points_sdf["SHAPE"] = points_sdf["SHAPE"].apply(
        lambda g: g.label_point if g else None
    )

    logging.info(f"Number of point features generated: {len(points_sdf)}")
    qa_qc_numbers(len(points_sdf), len(polygon_sdf), "Number of points generated")

    return points_sdf


def qa_qc_numbers(num1, num2, desc=None):
    """Compare any two numbers to determine if they are equal.
    """

    if num1 == num2:
        logging.info(f"QAQC Numbers ::: {num1} == {num2}  {desc} ")
        return True
    else:
        logging.warning(f"QAQC Numbers ::: {num1} != {num2}  {desc}")
        return False


def find_changed_geometries(new_geo_sdf, old_geo_sdf):
    """
    Compare new geometries to old geometries and identify which features have changed geometry
    
    :param new_geo_sdf: newly dissolved spatial dataframe with updated geometries that we want to compare to the existing geometries in AGOL
    :param old_geo_sdf: existing spatial dataframe with old geometries in AGOL

    :return: A dataframe that contains the space_place_ids of features that have changed geometry based on the comparison of new and old geometries.
    """
    new_geo_sdf = new_geo_sdf.copy()
    old_geo_sdf = old_geo_sdf.copy()
    new_geo_sdf["space_place_id"] = new_geo_sdf["space_place_id"].astype("int64")
    old_geo_sdf["space_place_id"] = old_geo_sdf["space_place_id"].astype("int64")

    comparison_df = pd.merge(
        new_geo_sdf,
        old_geo_sdf[["space_place_id", "SHAPE"]],
        on="space_place_id",
        how="left",
        suffixes=("_new", "_old"),
        indicator=True,
    )

    # Only IDs in both dataframes are eligible for "changed"
    changed_mask = [
        (
            row_merge == "both"
            and pd.notna(g_new)
            and pd.notna(g_old)
            and not g_new.equals(g_old)
        )
        for row_merge, g_new, g_old in zip(
            comparison_df["_merge"], comparison_df["SHAPE_new"], comparison_df["SHAPE_old"]
        )
    ]

    # "New" means ID exists only in new dataframe
    new_mask = [row_merge == "left_only" for row_merge in comparison_df["_merge"]]

    changed_features = comparison_df[changed_mask].copy()
    changed_features = changed_features.rename(columns={"SHAPE_new": "SHAPE"}).drop(
        columns=["SHAPE_old", "_merge"]
    )
    changed_features.spatial.set_geometry("SHAPE")
    logging.info(
        f"Number of features with changed geometry ::: {len(changed_features)}"
        f" ::: [{changed_features['space_place_id'].unique()}]"
    )

    new_features = comparison_df[new_mask].copy()
    new_features = new_features.rename(columns={"SHAPE_new": "SHAPE"}).drop(
        columns=["SHAPE_old", "_merge"]
    )
    new_features.spatial.set_geometry("SHAPE")
    logging.info(
        f"Number of new features ::: {len(new_features)} "
        f"::: [{new_features['space_place_id'].unique()}]"
    )

    return changed_features, new_features



def push_adds(layer, sdf, chunk_size=250):
    """
    Pushes new features to a feature layer in chunks.
    """
    for start in range(0, len(sdf), chunk_size):
        try:

            chunk = sdf.iloc[start : start + chunk_size]

            # Convert the chunk to a list of features
            features = chunk.spatial.to_featureset().features
            # Append the features to the layer
            result = layer.edit_features(adds=features)
            for i, res in enumerate(result.get("addResults", [])):
                if not res.get("success", False):
                    logging.warning(f"Failed to add feature at index {res.get('space_place_id')}: {res}")

            logging.info(f"Push Results ::: {result}")
        except Exception as e:
            logging.error(f"Error pushing chunk {start} to {start + chunk_size}: {e}")
    return


def push_updates(layer, sdf, chunk_size=250):
    """
    Pushes update features to a feature layer in chunks.
    """
    for start in range(0, len(sdf), chunk_size):
        try:

            chunk = sdf.iloc[start : start + chunk_size]

            # Convert the chunk to a list of features
            features = chunk.spatial.to_featureset().features
            # Append the features to the layer
            result = layer.edit_features(updates=features, use_global_ids=True)
            for i, res in enumerate(result.get("updateResults", [])):
                if not res.get("success", False):
                    logging.warning(f"Failed to update feature at index {res.get('space_place_id')}: {res}")

            logging.info(f"Push Update Results ::: {result}")
        except Exception as e:
            logging.error(f"Error pushing update chunk {start} to {start + chunk_size}: {e}")
    return


def main(item_id, domain_dict, spatial_ref, original_spatial_ref):

    setup_logging("update_geometry")
    logging.info("Script started.")

    gis = GIS("home")
    logging.info(f"Connected to portal: {gis.properties.portalName}")
    logging.info(f"Logged in as: {gis.users.me.username}")

    # Load feature layers
    undissolved_layer = gis.content.get(item_id).layers[2]
    dissolved_layer = gis.content.get(item_id).layers[1]
    points_layer = gis.content.get(item_id).layers[0]

    # convert to spatial dataframes
    dissolved_sdf = pd.DataFrame.spatial.from_layer(dissolved_layer)
    undissolved_sdf = pd.DataFrame.spatial.from_layer(undissolved_layer)
    points_sdf = pd.DataFrame.spatial.from_layer(points_layer)

    # filter records
    undissolved_sdf_filtered = filter_undissolved_polygons(undissolved_sdf)


    # Aggregate geometry related attributes before dissolve
    aggregated_df = aggregate_attributes(undissolved_sdf_filtered, domain_dict)

    # Dissolve geometries, calculate acres, and join aggregated fields to geometry
    fresh_diss_polygons_sdf = dissolve_geometry(undissolved_sdf_filtered)
    original_shapes = fresh_diss_polygons_sdf["SHAPE"].copy()  # snapshot 4326 geometry
    project_and_calculate_acres(fresh_diss_polygons_sdf, spatial_ref)
    fresh_diss_polygons_sdf = join_attri_and_rename_fields(fresh_diss_polygons_sdf, aggregated_df)

    # Reset geometry to original spatial reference for AGOL
    fresh_diss_polygons_sdf["SHAPE"] = original_shapes  # restore 4326, no round-trip
    fresh_diss_polygons_sdf.spatial.set_geometry("SHAPE")

    # Create centroids from new dissolved geometries for points layer update
    fresh_points_sdf = polygons_to_centroids(fresh_diss_polygons_sdf)

    # Identify changes in geometry outputs a list of the space_place_ids
    logging.info("Finding changed polygons...")
    changed_polygons_sdf, new_polygons_sdf = find_changed_geometries(fresh_diss_polygons_sdf, dissolved_sdf)
    logging.info("Finding changed points...")
    changed_points_sdf, new_points_sdf= find_changed_geometries(fresh_points_sdf, points_sdf)
    qa_qc_numbers(len(changed_polygons_sdf), len(changed_points_sdf), "Number of Poly v. Points with changed geometry")
    qa_qc_numbers(len(new_polygons_sdf), len(new_points_sdf), "Number of Poly v. Points with newly created features")
    if len(changed_polygons_sdf) == 0 and len(changed_points_sdf) == 0 and len(new_polygons_sdf) == 0 and len(new_points_sdf) == 0:
        logging.info("No changes detected. Exiting script.")
        return
    else:
        logging.info(f"Changes detected ::: {len(changed_polygons_sdf)} changed polygons, {len(changed_points_sdf)} changed points, "
                     f"{len(new_polygons_sdf)} new polygons, {len(new_points_sdf)} new points. Proceeding with update.")
        # Pushes updates to hosted feature layer in AGOL
        logging.info(f">>> Pushing changed polygons: {len(changed_polygons_sdf)} features to update.")
        push_updates(dissolved_layer, changed_polygons_sdf)
        logging.info(f">>> Pushing changed points: {len(changed_points_sdf)} features to update.")
        push_updates(points_layer, changed_points_sdf)

        logging.info(f">>> Pushing newly created polygons: {len(new_polygons_sdf)} features to add.")
        push_adds(dissolved_layer, new_polygons_sdf)
        logging.info(f">>> Pushing newly created points: {len(new_points_sdf)} features to add.")
        push_adds(points_layer, new_points_sdf)
        return


if __name__ == "__main__":

    run_mode = input("Enter 'dev' to run in dev mode, 'prod' to run in prod mode: ")
    if run_mode == "prod":
        ITEM_ID = "424767989a5f4c90a78300187691ff13"
    else:
        ITEM_ID = "200ccece1f524f79b018ac73bea3e62d" # DEV Item ID


    DOMAIN_DICT = {
        "regrid_parcel": "REGRID Parcel",
        "county_parcel": "County Parcel",
        "aerial_imagery": "Aerial Imagery Digitization",
        "legacy_digitization": "Legacy Asset Digitization",
        "other": "Other Source Not Listed",
        "mixed": "Mixed Sources",
        "pseudo": "Pseudo Geometry",
    }
    SPATIAL_REF = "North_America_Albers_Equal_Area_Conic"
    ORIGINAL_SPATIAL_REF = 4326

    try:
        main(
            item_id=ITEM_ID,
            domain_dict=DOMAIN_DICT,
            spatial_ref=SPATIAL_REF,
            original_spatial_ref=ORIGINAL_SPATIAL_REF,
        )
        logging.info("Script completed successfully.")
    except Exception as e:
        logging.exception("Script failed due to an unexpected error.")
        raise
