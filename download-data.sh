mkdir -p data/raw
# check if the file already exists and has the correct md5 checksum
#if [ -f data/raw/hlamajority-appnote-data-raw.tar.gz ]; then
#    echo "File already exists. Checking integrity..."
#    if echo "3e310bb0ef9e42d0c510766d6bdba19c data/raw/hlamajority-appnote-data-raw.tar.gz" | md5sum -c -; then
#	echo "File is valid. Skipping download - checking if it has been extracted..."
#        if [ -d data/raw/cell-lines-before-polysolver-change ]; then 
#            echo "Data has been extracted, skipping data extraction"
##	    exit 0
#        else
#            tar -xvf data/raw/hlamajority-appnote-data-raw.tar.gz -C data/raw/
#            mv data/raw/raw/* data/raw/
#            # remove the downloaded tar.gz file and the empty raw directory
#            rm data/raw/hlamajority-appnote-data-raw.tar.gz 
#            rmdir data/raw/raw
#            exit 0
#        fi
#    else
#	echo "File is corrupted. Removing and re-downloading..."
#	rm data/raw/hlamajority-appnote-data-raw.tar.gz
#    fi
#else
echo "Downloading data from Zenodo..."
wget -O data/raw/hlamajority-appnote-data-raw.tar.gz https://zenodo.org/records/21793415/files/hlamajority-appnote-data-raw.tar.gz?download=1
echo "Checking integrity of the downloaded file..."
if echo "3e310bb0ef9e42d0c510766d6bdba19c data/raw/hlamajority-appnote-data-raw.tar.gz" | md5sum -c -; then
    echo "File is valid."
    tar -xvf data/raw/hlamajority-appnote-data-raw.tar.gz -C data/raw/
    mv data/raw/raw/* data/raw/
    # remove the downloaded tar.gz file and the empty raw directory
    rm data/raw/hlamajority-appnote-data-raw.tar.gz 
    rmdir data/raw/raw
else
    echo "File is corrupted. Please try downloading again."
    exit 1
fi
