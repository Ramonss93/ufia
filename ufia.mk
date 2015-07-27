

# Testing on small image
PAN_SPOT/SPOT_PanSharp_subsubset_wAddedFeatures.tif: R/addLayersImage.R
	cd R; Rscript addLayersImage.R SPOT_PanSharp_subsubset PAN_SPOT

# Add Layers Large Image
PAN_SPOT/geomatica_SPOT_panshp_wAddedFeatures.tif: R/addLayersImage.R
	cd R; Rscript addLayersImage.R geomatica_SPOT_panshp PAN_SPOT

# Convert to dataframe

PAN_SPOT/geomatica_SPOT_panshp_wAddedFeatures_df.Rdata: R/convertImageToDF.R
	cd R; Rscript convertImageToDF.R PAN_SPOT/geomatica_SPOT_panshp_wAdditionalFeatures
