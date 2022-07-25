FROM nixos/nix:latest

RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable unstable
RUN nix-channel --update

RUN echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf
RUN echo 'sandbox = false'                            >> /etc/nix/nix.conf

RUN nix profile upgrade '.*'
RUN nix profile install nixpkgs#bash
RUN nix profile install nixpkgs#gnumake

ADD run.sh /run.sh

CMD ["/run.sh"]
