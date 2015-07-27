
# Add Ratios Small Image
#PAN_SPOT/SPOT_PanSharp_subsubset_wRatios.tif: R/addRatiosImage.R
#	cd R; Rscript addRatiosImage.R SPOT_PanSharp_subsubset PAN_SPOT

# Add Ratios Large Image
#PAN_SPOT/geomatica_SPOT_panshp_wRatios.tif: R/addRatiosImage.R
#	cd R; Rscript addRatiosImage.R geomatica_SPOT_panshp PAN_SPOT

# Convert to dataframe
#PAN_SPOT/geomatica_SPOT_panshp_wAddedFeatures_df.Rdata: R/convertImageToDF.R
#	cd R; Rscript convertImageToDF.R PAN_SPOT/geomatica_SPOT_panshp_wAdditionalFeatures


# Add texture to Small Image, 5 is the NDVI layer
#PAN_SPOT/SPOT_PanSharp_subsubset_wRatios_wTexture.tif: R/addTextureImage.R PAN_SPOT/SPOT_PanSharp_subsubset_wRatios.tif
#	cd R; Rscript addTextureImage.R PAN_SPOT SPOT_PanSharp_subsubset_wRatios 5

# Add texture to Large Image, 5 is the NDVI layer
PAN_SPOT/geomatica_SPOT_panshp_wRatios_wTexture.tif: R/addTextureImage.R PAN_SPOT/geomatica_SPOT_panshp_wRatios.tif
	cd R; Rscript addTextureImage.R PAN_SPOT geomatica_SPOT_panshp_wRatios 5

