FROM debian:buster-slim

WORKDIR /opt/bash

COPY ./install.sh /opt/bash/install.sh
RUN apt-get update -y \
 && apt-get upgrade -y \
 && bash /opt/bash/install.sh \
 && apt-get clean -y

COPY ./src /opt/bash/src
COPY ./run.sh /opt/bash/run.sh

ENTRYPOINT ["/bin/bash"]
CMD ["/opt/bash/run.sh"]

