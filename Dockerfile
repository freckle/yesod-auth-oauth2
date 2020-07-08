FROM heroku/heroku:18-build

# Update system and install stack
ENV DEBIAN_FRONTEND noninteractive
ENV LANG   C.UTF-8
ENV LC_ALL C.UTF-8
RUN apt-get update \
  && apt-get install sudo haskell-stack \
  && apt-get clean

# Create user "testuser"
RUN useradd -m -d /home/testuser -s /bin/bash testuser
RUN mkdir -p /etc/sudoers.d \
  && echo "testuser ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/testuser \
  && chmod 0440 /etc/sudoers.d/testuser
ENV HOME /home/testuser
WORKDIR /home/testuser
USER testuser
COPY --chown=testuser package.yaml app/package.yaml
COPY --chown=testuser stack.yaml app/stack.yaml

# Update stack and cache dependencies and app in stages
ENV PATH="/home/testuser/.local/bin:${PATH}"
RUN stack upgrade
RUN cd app && stack setup
RUN cd app && stack build --only-dependencies Cabal
RUN cd app && stack build --only-dependencies yesod

COPY --chown=testuser . app
RUN cd app && stack install --flag yesod-auth-oauth2:example
WORKDIR /home/testuser/app

CMD yesod-auth-oauth2-example
