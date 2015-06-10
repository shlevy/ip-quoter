{ mkDerivation, base, network, split, stdenv, tasty, tasty-hunit
, template-haskell
}:
mkDerivation {
  pname = "ip-quoter";
  version = "1.0.0.0";
  src = ./.;
  buildDepends = [ base network split template-haskell ];
  testDepends = [ base network tasty tasty-hunit ];
  homepage = "https://github.com/shlevy/ip-quoter";
  description = "Quasiquoter for IP addresses";
  license = stdenv.lib.licenses.mit;
}
