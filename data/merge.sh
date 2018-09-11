#!/usr/bin/sh

# in minutes
RESOLUTION=$1

RES=$(echo "scale=15; $1/60" | bc)
OUTVRT=GAOP2018_"$RESOLUTION"m.vrt
OUTTIFF=$(basename $OUTVRT .vrt).tif
OUTNC=$(basename $OUTTIFF .tif).nc

gdalbuildvrt -resolution user -tr $RES $RES -r bilinear -overwrite\
    -vrtnodata NaN -input_file_list files_to_merge.txt $OUTVRT
gdal_translate -of GTiff -a_srs WGS84 $OUTVRT $OUTTIFF
gdal_translate -of NetCDF -a_srs WGS84 $OUTTIFF $OUTNC


ncrename -h -v '.Band1,free_air_anomaly' $OUTNC

ncatted -h\
    -a 'title,global,o,c,Free-air gravity anomalies'\
    -a 'comment,global,o,c,Mean free-air gravity anomalies for GAOP2018 global gravity field model.
        Reference field: WGS84 (GRS80), atmospheric correction where available.'\
    -a 'source,global,o,c,DTU13 merged with the local data for Russia, Canada, US, Australia and ArcGP.'\
    -a 'long_name,free_air_anomaly,o,c,gravity_free_air_anomaly'\
    -a 'units,free_air_anomaly,o,c,mGal' $OUTNC

rm -f *.xml *.vrt
