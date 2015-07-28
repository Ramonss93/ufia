
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

# Add texture of 3x3 window to Large Image, 5 is the NDVI layer
#PAN_SPOT/geomatica_SPOT_panshp_wRatios_wTexture.tif: R/addTextureImage.R PAN_SPOT/geomatica_SPOT_panshp_wRatios.tif
#	cd R; Rscript addTextureImage.R PAN_SPOT geomatica_SPOT_panshp_wRatios 5

# Add texture of 5x5 window to Large Image, 5 is the NDVI layer
#PAN_SPOT/geomatica_SPOT_panshp_wRatios_wTexture5x5.tif: R/addTexture5x5Image.R PAN_SPOT/geomatica_SPOT_panshp_wRatios.tif
#	cd R; Rscript addTexture5x5Image.R PAN_SPOT geomatica_SPOT_panshp_wRatios 5

# Extract Training Data from images: Lei's roi/training data
#PAN_SPOT/ExtractedTrainingDataFrames/lei_roi_train_df.txt: R/ExtractTrainingDataFrames.R
#	cd R; Rscript ExtractTrainingDataFrames.R PAN_SPOT geomatica_SPOT_panshp_wRatios geomatica_SPOT_panshp_wRatios_wTexture geomatica_SPOT_panshp_wRatios_wTexture5x5

# Extract Training Data from images: Ted's roi/training data
#PAN_SPOT/ExtractedTrainingDataFrames/ted_roi_train_df.txt: R/ExtractTrainingDataFrames.R
#	cd R; Rscript ExtractTrainingDataFrames.R PAN_SPOT geomatica_SPOT_panshp_wRatios geomatica_SPOT_panshp_wRatios_wTexture geomatica_SPOT_panshp_wRatios_wTexture5x5


# USE UNTIL LARGER WINDOW TEXTURE IMAGE AVAILABLE Extract Training Data from images: Lei's roi/training data
#PAN_SPOT/ExtractedTrainingDataFrames/lei_roi_train_df.txt: R/ExtractTrainingDataFrames.R
#	cd R; Rscript ExtractTrainingDataFrames.R PAN_SPOT geomatica_SPOT_panshp_wRatios geomatica_SPOT_panshp_wRatios_wTexture geomatica_SPOT_panshp_wRatios_wTexture5x5

# USE UNTIL LARGER WINDOW TEXTURE IMAGE AVAILABLE Extract Training Data from images: Ted's roi/training data
#PAN_SPOT/ExtractedTrainingDataFrames/ted_roi_train_df.txt: R/ExtractTrainingDataFrames.R
#	cd R; Rscript ExtractTrainingDataFrames.R PAN_SPOT geomatica_SPOT_panshp_wRatios geomatica_SPOT_panshp_wRatios_wTexture



# Save List of Best Classification Models
PAN_SPOT/best_tuning_results.Rdata: R/TuneModels.R
	cd R; Rscript TuneModels.R