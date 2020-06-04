FROM fpco/stack-build:lts-15.10 as build
COPY . /opt/src
RUN cd /opt/src && stack install --system-ghc --local-bin-path /opt/bin

FROM fpco/pid1:18.04
COPY --from=build /opt/bin/wps-serve /usr/local/bin/wps-serve
