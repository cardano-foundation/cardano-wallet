
############################
# cardano-wallet-byron
############################

# Fetch the linux archive
curl -L https://hydra.iohk.io/job/Cardano/cardano-wallet/cardano-wallet-byron-linux64/latest/download/1 | tar xz
find . -maxdepth 1 -type d -name "cardano-wallet-byron-*" -exec mv \{} cardano-wallet-byron-linux64 \;
mv cardano-wallet-byron-linux64/cardano-wallet-byron cardano-wallet-byron-linux64/cardano-wallet
tar czf cardano-wallet-$TRAVIS_TAG-linux64.tar.gz cardano-wallet-byron-linux64
rm -r cardano-wallet-byron-linux64

# Fetch the MacOS archive
curl -L https://hydra.iohk.io/job/Cardano/cardano-wallet/cardano-wallet-byron-macos64/latest/download/1 | tar xz
find . -maxdepth 1 -type d -name "cardano-wallet-byron-*" -exec mv \{} cardano-wallet-byron-macos64 \;
mv cardano-wallet-byron-macos64/cardano-wallet-byron cardano-wallet-byron-macos64/cardano-wallet
tar czf cardano-wallet-$TRAVIS_TAG-macos64.tar.gz cardano-wallet-byron-macos64
rm -r cardano-wallet-byron-macos64

# Fetch the Windows archive
curl -L https://hydra.iohk.io/job/Cardano/cardano-wallet/cardano-wallet-byron-win64/latest/download/1 --output cardano-wallet-byron-win64.zip
unzip -d cardano-wallet-byron-win64 cardano-wallet-byron-win64.zip
mv cardano-wallet-byron-win64/cardano-wallet-byron.exe cardano-wallet-byron-win64/cardano-wallet.exe
zip -r cardano-wallet-$TRAVIS_TAG-win64.zip cardano-wallet-byron-win64
rm -r cardano-wallet-byron-win64
rm cardano-wallet-byron-win64.zip

############################
# cardano-wallet-jormungandr
############################

# Fetch the linux archive
curl -L https://hydra.iohk.io/job/Cardano/cardano-wallet/cardano-wallet-jormungandr-linux64/latest/download/1 | tar xz
find . -maxdepth 1 -type d -name "cardano-wallet-jormungandr-*" -exec mv \{} cardano-wallet-jormungandr-linux64 \;
mv cardano-wallet-jormungandr-linux64/cardano-wallet-jormungandr cardano-wallet-jormungandr-linux64/cardano-wallet
tar czf cardano-wallet-itn-$TRAVIS_TAG-linux64.tar.gz cardano-wallet-jormungandr-linux64
rm -r cardano-wallet-jormungandr-linux64

# Fetch the MacOS archive
curl -L https://hydra.iohk.io/job/Cardano/cardano-wallet/cardano-wallet-jormungandr-macos64/latest/download/1 | tar xz
find . -maxdepth 1 -type d -name "cardano-wallet-jormungandr-*" -exec mv \{} cardano-wallet-jormungandr-macos64 \;
mv cardano-wallet-jormungandr-macos64/cardano-wallet-jormungandr cardano-wallet-jormungandr-macos64/cardano-wallet
tar czf cardano-wallet-itn-$TRAVIS_TAG-macos64.tar.gz cardano-wallet-jormungandr-macos64
rm -r cardano-wallet-jormungandr-macos64

# Fetch the Windows archive
curl -L https://hydra.iohk.io/job/Cardano/cardano-wallet/cardano-wallet-jormungandr-win64/latest/download/1 --output cardano-wallet-jormungandr-win64.zip
unzip -d cardano-wallet-jormungandr-win64 cardano-wallet-jormungandr-win64.zip
mv cardano-wallet-jormungandr-win64/cardano-wallet-jormungandr.exe cardano-wallet-jormungandr-win64/cardano-wallet.exe
zip -r cardano-wallet-itn-$TRAVIS_TAG-win64.zip cardano-wallet-jormungandr-win64
rm -r cardano-wallet-jormungandr-win64
rm cardano-wallet-jormungandr-win64.zip
