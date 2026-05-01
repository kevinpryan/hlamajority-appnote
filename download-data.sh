mkdir -p data/raw
# check if the file already exists and has the correct md5 checksum
if [ -f data/raw/hlamajority-appnote-data-raw.tar.gz ]; then
    echo "File already exists. Checking integrity..."
    if echo "bff5f67e5f535e892dec74846f392e03  data/raw/hlamajority-appnote-data-raw.tar.gz" | md5sum -c -; then
	echo "File is valid. Skipping download."
	exit 0
    else
	echo "File is corrupted. Removing and re-downloading..."
	rm data/raw/hlamajority-appnote-data-raw.tar.gz
    fi
fi
wget -O data/raw/hlamajority-appnote-data-raw.tar.gz https://zenodo.org/records/19947258/files/hlamajority-appnote-data-raw.tar.gz?download=1
# check the integrity of the downloaded file using md5 checksum
echo "Checking integrity of the downloaded file..."
if echo "bff5f67e5f535e892dec74846f392e03  data/raw/hlamajority-appnote-data-raw.tar.gz" | md5sum -c -; then
    echo "File is valid."
else
    echo "File is corrupted. Please try downloading again."
    exit 1
fi

tar -xvf data/raw/hlamajority-appnote-data-raw.tar.gz -C data/raw/
mv data/raw/raw/* data/raw/
# remove the downloaded tar.gz file and the empty raw directory
rm data/raw/hlamajority-appnote-data-raw.tar.gz data/raw/raw
