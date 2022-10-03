#!/bin/bash

SERVICE_FILE=goban.service
INSTALL_DIR=/etc/systemd/system
INSTALL_TARGET=${INSTALL_DIR}/${SERVICE_FILE}

export WORKING_DIRECTORY=$(pwd)

cat goban.service | envsubst > /tmp/${SERVICE_FILE}
sudo cp /tmp/${SERVICE_FILE} ${INSTALL_TARGET}
sudo chmod 644 ${INSTALL_TARGET}
rm /tmp/${SERVICE_FILE}

echo "Installed ${INSTALL_TARGET}"