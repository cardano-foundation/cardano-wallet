############################################################################
#
# Super-minimal Docker image for cardano-wallet -- everything
# except the nixpkgs base is built from source.
#
# Make sure you have plenty of time and disk space for this.
#
# When auditing the build logs, note the sections
#   these derivations will be built:
#     ^ translation: The following packages will be built.
#   these paths will be fetched (_ MiB download, _ MiB unpacked):
#     ^ translation: The following pre-built packages will be used.
#   copying path '/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-_' from 'https://_'...
#     ^ translation: Downloading pre-built nix package.
#   unpacking 'https://github.com/input-output-hk/___.tar.gz'...
#     ^ translation: Downloading sources from github.
#
# https://www.rosetta-api.org/docs/node_deployment.html
#
############################################################################


############################################################################
# Builder image

# Use multi-stage build
FROM nixos/nix as builder

RUN echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf

# Set --build-arg USE_IOHK_CACHE=t if you lack the time and/or disk
# space to build ghc and everything else. These lines will enable the
# IOHK binary cache of nix builds from https://hydra.iohk.io.
ARG USE_IOHK_CACHE=
RUN test -n "${USE_IOHK_CACHE}" && ( echo "Note: using IOHK nix cache" > /dev/stderr ; echo 'substituters = https://hydra.iohk.io https://cache.nixos.org\ntrusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=' >> /etc/nix/nix.conf ) || echo "Note: Not using IOHK nix cache"

############################################################################
# Warm up cache

# Make a docker layer with a warm cache (helps re-use compilers for
# subsequent builds).
RUN nix build "github:input-output-hk/cardano-wallet#hydraJobs.linux.musl.cardano-wallet" --no-link


############################################################################
# Build

# You can choose a branch, tag, or git revision of the cardano-wallet repo.
ARG CODE_VERSION=master

# Build the wallet and node static binaries package, untar to "out" directory
RUN nix build "github:input-output-hk/cardano-wallet${CODE_VERSION}#hydraJobs.linux.musl.cardano-wallet-linux64"
RUN tar -xzvf result/*.tar.gz
RUN rm result
RUN mv cardano-wallet-* ../out


############################################################################
# Create final container

FROM scratch
CMD mkdir -p bin
# It is ok to COPY files from a build container (when using multi-stage builds)
COPY --from=builder out/cardano-node bin/cardano-node
COPY --from=builder out/cardano-wallet bin/cardano-wallet
ENTRYPOINT ["cardano-wallet"]
CMD []
