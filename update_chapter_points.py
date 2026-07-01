# ## Set Up Environment and Load Data

# Import libraries
from arcgis.gis import GIS
from arcgis.features import Feature
import pandas as pd
import urllib3
import logging
from datetime import datetime
import os
from arcgis.features import FeatureSet
import time


urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

def setup_logging(log_name="update_chapter_points", log_folder="logs"):
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

def query_layer_chunked(layer, chunk_size=100):
    all_features = []
    offset = 0
    while True:
        result = layer.query(
            where="1=1",
            result_offset=offset,
            result_record_count=chunk_size,
            as_df=False
        )
        features = result.features
        if not features:
            break
        all_features.extend(features)
        print(f"Fetched {len(all_features)} features so far...")
        if len(features) < chunk_size:
            break
        offset += chunk_size
        time.sleep(1)  # be gentle on the server

    fs = FeatureSet(all_features)
    return fs.sdf


def prepare_boundaries_dataframe(bound_sdf, spatial_ref):
    """
    Prepares the boundaries spatial dataframe for the update process by selecting and renaming relevant columns.
    :param bound_sdf: The spatial dataframe that contains the original boundary geometries and attributes from AGOL,    
    :return: A spatial dataframe that contains a matching schema to the points layer and includes the relevant attributes for the update process, joined with the aggregated attributes from the dissolved geometries.
    """
    
    # Select and rename the relevant columns for the final update dataframe
    filtered = bound_sdf[
        [
            "ChapterCode",
            "ChapterName",
            "State",
            "City",
            "Address1",
            "Address2",
            "ZipCode",
            "Website",
            "Email",
            "SHAPE",
        ]
    ]
    # Rename columns to match source schema
    filtered = filtered.rename(
        columns={
            "ChapterCode": "Chapter_Code",
        "ChapterName": "Chapter_Name",
        "ZipCode": "Zip_Code",
        }
    )

    # Reproject to match spatial reference of points layer
    projected = filtered.copy()
    projected.spatial.project(spatial_ref)

    return projected.copy()

def polygons_to_centroids(polygon_sdf):
    """
    Converts the polygon geometries to centroid points for the points layer update,
    
    :param polygon_sdf: The spatial dataframe that contains the polygon geometries 
    which will be used to create the point geometries for the points layer update.
    
    :return: A spatial dataframe that contains the centroid point geometries and aggregated attributes for features with changed geometry
    """
    points_sdf = polygon_sdf.copy()
    points_sdf["SHAPE"] = points_sdf["SHAPE"].apply(
        lambda g: g.label_point if g else None
    )
    print(f"Number of points features to update: {len(points_sdf)}")
    logging.info(f"Number of point features generated: {len(points_sdf)}")

    return points_sdf


def find_changed_geometries(new_geo_sdf, old_geo_sdf):
    """
    Compare new geometries to old geometries and identify which features have changed geometry
    
    :param new_geo_sdf: newly created point layer with updated geometries that we want to compare to the existing geometries in AGOL
    :param old_geo_sdf: existing spatial dataframe with old geometries in AGOL

    :return: A dataframe that contains the Chapter_Code values of features that have changed geometry based on the comparison of new and old geometries.
    """
    comparison_df = pd.merge(
        new_geo_sdf,
        old_geo_sdf[["Chapter_Code", "SHAPE"]],
        on = "Chapter_Code",
        how="left",
        suffixes=("_new", "_old"),
    )
    # Create a boolean mask to identify rows where the geometry has changed (accounting for None values)
    changed_mask = [
        False
        if g_new is None and g_old is None
        else True
        if g_new is None or g_old is None
        else not g_new.equals(g_old)
        for g_new, g_old in zip(comparison_df["SHAPE_new"], comparison_df["SHAPE_old"])
    ]

    changed_features = comparison_df[changed_mask]
    print(f"Number of features with changed geometry: {len(changed_features)}")
    logging.info(f"Number of features with changed geometry: {len(changed_features)}")

    return changed_features[["Chapter_Code"]]


def build_update_dataframe(changes_df, updated_sdf):
    """
    Build the final dataframe that will be used to update the features in AGOL, 
    including the new geometries and aggregated attributes for features with changed geometry
    
    :param changes_df: A dataframe that contains the space_place_ids of features that have changed geometry based on the comparison of new and old geometries.
    :param updated_sdf: The updated  spatial dataframe that contains the new geometries for features with changed geometry.
    
    :return: A spatial dataframe that contains the new geometries and aggregated attributes for features with changed geometry, 
    and is filtered to only include features that have changed geometry based on the changes_df.
    """
    update_sdf = pd.merge(
        changes_df, updated_sdf, on="Chapter_Code", how="left"
    )
    return update_sdf


def attach_objectids(update_sdf, points_sdf):
    """
    Attach OBJECTIDs from the original points layer to the update dataframe so we know which features to update in AGOL
    
    :param update_sdf: The update dataframe that contains the new geometries and aggregated attributes for features with changed geometry
    :param points_sdf: The original points layer that has original OBJECTIDs to connect to updated attributes

    :return: A spatial dataframe that has been merged with existing OBJECTIDs from the AGOL layer, 
    and includes the new geometries and aggregated attributes for features with changed geometry. 
    """
    
    # We only need the OBJECTID and Chapter_Code fields from the  updated layer    
    existing_ids = points_sdf[["OBJECTID_1", "Chapter_Code"]] #Change to plain OBJECTID for any other layer that doesn't have OBJECTID_1 as the default ID field name

    # Join the update dataframe with the existing IDs to get the OBJECTIDs for the features that need to be updated
    print("ObjectIDs attached")
    logging.info("ObjectIDs attached")

    return update_sdf.merge(existing_ids, on="Chapter_Code", how="inner")


def build_update_features(merged_update_sdf):
    """
    Build the list of features to update in AGOL, including the new geometries 
    and aggregated attributes for features with changed geometry
    
    :param merged_update_sdf: A spatial dataframe that has been merged with existing OBJECTIDs from the AGOL layer, 
    and includes the new geometries and aggregated attributes for features with changed geometry.
    
    :return: A list of ArcGIS API for Python Feature objects that are ready to be updated in AGOL.
    """
    features_to_update = []

    # Iterate through the merged update dataframe and create a list of features to update in AGOL
    for _, row in merged_update_sdf.iterrows():
        attributes = row.drop(labels=["SHAPE"]).to_dict()

        # Create a new feature with the updated geometry and attributes
        feat = Feature(geometry=row["SHAPE"], attributes=attributes)

        features_to_update.append(feat)

    return features_to_update


def push_updates(layer, features):
    """
    Push updates to AGOL

    :param layer: The target layer in AGOL to which the updates will be pushed
    :param features: A list of ArcGIS API for Python Feature objects that are ready to be updated in AGOL.
    
    :return: The result of the edit operation, which includes information about the success or failure of the updates.
    """
    if not features:
        return None

    result = layer.edit_features(updates=features)
    print(f"Number of features successfully updated: {len(result['updateResults'])}")
    logging.info(f"Number of features successfully updated: {len(result['updateResults'])}")

    return result

def find_new_features(new_geo_sdf, old_geo_sdf):
    """
    Compare new geometries to old geometries and identify which features have been newly created (i.e. exist in new but not in old)
    
    :param new_geo_sdf: newly created point layer with updated geometries that we want to compare to the existing geometries in AGOL
    :param old_geo_sdf: existing spatial dataframe with old geometries in AGOL

    :return: A dataframe that contains the Chapter_Code values of features that have been newly created based on the comparison of new and old geometries.
    """
    new_mask = ~new_geo_sdf["Chapter_Code"].isin(old_geo_sdf["Chapter_Code"])
    new_features = new_geo_sdf[new_mask]
    print(f"Number of new features to add: {len(new_features)}")
    logging.info(f"Number of new features to add: {len(new_features)}")
    return new_features

def push_additions(layer, features):
    """
    Push additions to AGOL

    :param layer: The target layer in AGOL to which the additions will be pushed
    :param features: A list of ArcGIS API for Python Feature objects that are ready to be added in AGOL.
    
    :return: The result of the edit operation, which includes information about the success or failure of the additions.
    """
    if not features:
        return None

    result = layer.edit_features(adds=features)
    print(f"Number of features successfully added: {len(result['addResults'])}")
    logging.info(f"Number of features successfully added: {len(result['addResults'])}")

    return result



def find_deleted_features(new_geo_sdf, old_geo_sdf):
    """
    Compare new geometries to old geometries and identify which features have been deleted (i.e. exist in old but not in new)
    
    :param new_geo_sdf: newly created point layer with updated geometries that we want to compare to the existing geometries in AGOL
    :param old_geo_sdf: existing spatial dataframe with old geometries in AGOL

    :return: A dataframe that contains the Chapter_Code values of features that have been deleted based on the comparison of new and old geometries.
    """
    deleted_mask = ~old_geo_sdf["Chapter_Code"].isin(new_geo_sdf["Chapter_Code"])
    deleted_features = old_geo_sdf[deleted_mask][["OBJECTID_1"]] #Change to plain OBJECTID for any other layer that doesn't have OBJECTID_1 as the default ID field name
    print(f"Number of features to delete: {len(deleted_features)}")
    logging.info(f"Number of features to delete: {len(deleted_features)}")

    return deleted_features



def push_deletes(layer, features):
    """
    Push deletes to AGOL

    :param layer: The target layer in AGOL to which the deletes will be pushed
    :param features: A list of OBJECTIDs that are ready to be deleted in AGOL.
    
    :return: The result of the edit operation, which includes information about the success or failure of the deletes.
    """
    if not features:
        return None

    result = layer.edit_features(deletes=features)
    print(f"Number of features successfully deleted: {len(result['deleteResults'])}")
    logging.info(f"Number of features successfully deleted: {len(result['deleteResults'])}")

    return result


def main(bound_item_id,points_item_id):

    setup_logging("update_geometry")

    logging.info("Script started.")

    # Connect to ArcGIS Online using current notebook user session
    gis = GIS("home")
    print(gis.properties.portalName)
    print(gis.users.me)
    logging.info(f"Connected to portal: {gis.properties.portalName}")
    logging.info(f"Logged in as: {gis.users.me.username}")


    # Load feature layers
    bound_layer = gis.content.get(bound_item_id).layers[0]
    points_layer = gis.content.get(points_item_id).layers[0]

    # convert to spatial dataframes
    bound_sdf = query_layer_chunked(bound_layer, chunk_size=50)
    points_sdf = pd.DataFrame.spatial.from_layer(points_layer)

    print(points_sdf.columns.tolist()) #check to see if OBJECTID field is named differently than default and adjust code accordingly if so

    # Prepare the boundaries spatial dataframe for the update process by selecting and renaming relevant columns
    prepare_boundaries_sdf = prepare_boundaries_dataframe(bound_sdf, spatial_ref=points_sdf.spatial.sr['wkid'])

    # Convert the prepared boundary polygons to centroids for the points layer update
    updated_points_sdf = polygons_to_centroids(prepare_boundaries_sdf)

    # Identify changes in geometry outputs a list of the ChapterCodes
    points_changes_df = find_changed_geometries(updated_points_sdf, points_sdf)

    # Filters newly created points by changes and builds the update dataframe for the points layer
    points_update_sdf = build_update_dataframe(points_changes_df, updated_points_sdf)

    # Adds original OBJECTIDs for edit features and filter out unmatched OBJECTIDS so we only attempt to update existing features in AGOL
    merged_points_update_sdf = attach_objectids(points_update_sdf, points_sdf)

    # Builds features for update
    points_features = build_update_features(merged_points_update_sdf)

    # Pushes updates to hosted feature layer in AGOL
    update_points_result = push_updates(points_layer, points_features)

    # Identify new features that have been created (i.e. exist in new but not in old)
    new_points_df = find_new_features(updated_points_sdf, points_sdf)

    new_points_features = build_update_features(new_points_df)
    
    # Push new features to AGOL
    new_points_result = push_additions(points_layer, new_points_features)

    # Identify features that have been deleted (i.e. exist in old but not in new)
    deleted_points_df = find_deleted_features(updated_points_sdf, points_sdf)

    # Push deletes to AGOL
    delete_result = push_deletes(points_layer, deleted_points_df["OBJECTID_1"].tolist()) #Change to plain OBJECTID for any other layer that doesn't have OBJECTID_1 as the default ID field name

    return update_points_result, new_points_result, delete_result


if __name__ == "__main__":
    # DEV ITEM IDS
    # BOUNDARIES_ITEM_ID = "4f99caa46b3a4e84ae9c965cb9cc1077"
    # POINTS_ITEM_ID = "12426bd9a9db40a9a8121e3a53bb0990"
    # REAL ITEM IDs
    BOUNDARIES_ITEM_ID = "d0dbf1767d2b4f2ea6020666d37b884c"
    POINTS_ITEM_ID = "6df571f83d594055bafa41fe50ef5eb7"

    try:
        main(
            bound_item_id=BOUNDARIES_ITEM_ID,
            points_item_id=POINTS_ITEM_ID,
        )
        logging.info("Script completed successfully.")
    except Exception as e:
        logging.exception("Script failed due to an unexpected error.")
        raise
